package org.openpit.ui

import org.lwjgl.opengl.GL11._

/*
 * Both GL15 and the ARB extension can supply Vertex Buffer Objects.
 * For some reason, the Intel GMA 950 driver only supports the ARB
 * extension.  It's pretty slow anyway compared to display lists.
 */
//import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.ARBBufferObject.{
    glGenBuffersARB => glGenBuffers,
    glBufferDataARB => glBufferData,
    glBufferSubDataARB => glBufferSubData,
    glBindBufferARB => glBindBuffer,
    GL_DYNAMIC_DRAW_ARB => GL_DYNAMIC_DRAW
}
import org.lwjgl.opengl.ARBVertexBufferObject.{
    GL_ARRAY_BUFFER_ARB => GL_ARRAY_BUFFER
}

import org.lwjgl.util.glu._
import simplex3d.math.intm._
import simplex3d.math.floatm._

import scala.concurrent.{MailBox, TIMEOUT}
import scala.concurrent.ops.spawn

import org.openpit.util._
import ImplicitGL._
import org.openpit.world._
import org.openpit.world.blocks._

/**
 * A context to hold the state needed to render to VBOs (potentially
 * from any thread)
 */
class RenderContext {

    var vb = new FloatBufferBuffer()
    var sb = new ShortBufferBuffer()

    def clear() {
        vb.clear()
        sb.clear()
    }

    object LightValues {
        def f(dim: Int) = {
            val bright = (5000 * dim)
            Vec3i(bright, bright, bright)
        }
        final val values = (0 to 6).map(f).toArray
        def apply(dim: Int) = values(dim)
        final val Sun = ConstVec3i(32767, 32767, 32767)
    }

    def cube(loc: inVec3i, b: Block, top: Vec2i, side: Vec2i, bot: Vec2i) {

        /**
         * The original stupid/awesome lighting.
         */
        def light(dir: ConstVec3i, dim: Int = 1): Vec3i = {
            if (dim == 6)
                LightValues.Sun
            else {
                import Direction._
                World.get(loc + dir + `Z+`*dim).getOrElse(Air) match {
                        case Air | Glass | Water => light(dir, dim + 1)
                        case _ => LightValues(dim)
                }
            }
        }

        var occloc = Vec3i(0,0,0)
        def occluded(dir: inVec3i) = {
            occloc := loc + dir
            World.get(occloc).getOrElse(Air) match {
                    case Air =>   false
                    case Glass => b == Glass
                    case Water => b == Water
                    case _ =>     true
            }
        }

        val x = loc.x
        val y = loc.y
        val z = loc.z
        val X = x + 1
        val Y = y + 1
        val Z = z + 1

        var u = top.x
        var v = top.y
        var U = u + 1
        var V = v + 1

        import Direction._
        if (!occluded(`Z+`)) {
            val l = light(ConstVec3i(0,0,0))
            sb += Vec2i(u, V); sb += l; vb += Vec3f(x,Y,Z)
            sb += Vec2i(u, v); sb += l; vb += Vec3f(x,y,Z)
            sb += Vec2i(U, v); sb += l; vb += Vec3f(X,y,Z)
            sb += Vec2i(U, V); sb += l; vb += Vec3f(X,Y,Z)
        }

        u = side.x
        v = side.y
        U = u + 1
        V = v + 1

        if (!occluded(`Y-`)) {
            val l = light(`Y-`)
            sb += Vec2i(u, v); sb += l; vb += Vec3f(x,y,Z)
            sb += Vec2i(u, V); sb += l; vb += Vec3f(x,y,z)
            sb += Vec2i(U, V); sb += l; vb += Vec3f(X,y,z)
            sb += Vec2i(U, v); sb += l; vb += Vec3f(X,y,Z)
        }

        if (!occluded(`X+`)) {
            val l = light(`X+`)
            sb += Vec2i(U, v); sb += l; vb += Vec3f(X,y,Z)
            sb += Vec2i(U, V); sb += l; vb += Vec3f(X,y,z)
            sb += Vec2i(u, V); sb += l; vb += Vec3f(X,Y,z)
            sb += Vec2i(u, v); sb += l; vb += Vec3f(X,Y,Z)
        }

        if (!occluded(`Y+`)) {
            val l = light(`Y+`)
            sb += Vec2i(u, v); sb += l; vb += Vec3f(X,Y,Z)
            sb += Vec2i(u, V); sb += l; vb += Vec3f(X,Y,z)
            sb += Vec2i(U, V); sb += l; vb += Vec3f(x,Y,z)
            sb += Vec2i(U, v); sb += l; vb += Vec3f(x,Y,Z)
        }

        if (!occluded(`X-`)) {
            val l = light(ConstVec3i(-1,0, 0))
            sb += Vec2i(U, v); sb += l; vb += Vec3f(x,Y,Z)
            sb += Vec2i(U, V); sb += l; vb += Vec3f(x,Y,z)
            sb += Vec2i(u, V); sb += l; vb += Vec3f(x,y,z)
            sb += Vec2i(u, v); sb += l; vb += Vec3f(x,y,Z)
        }

        if (!occluded(`Z-`)) {
            val l = light(`Z-`)
            u = bot.x
            v = bot.y
            U = u + 1
            V = v + 1

            sb += Vec2i(U, V); sb += l; vb += Vec3f(X,y,z)
            sb += Vec2i(U, v); sb += l; vb += Vec3f(x,y,z)
            sb += Vec2i(u, v); sb += l; vb += Vec3f(x,Y,z)
            sb += Vec2i(u, V); sb += l; vb += Vec3f(X,Y,z)
        }
    }


    /**
     * Upload the computed VBO data to OpenGL.  Must be called from
     * the main (GL) thread.
     *
     * All attributes are packed into a single VBO.  Packing must
     * respect alignment of each datatype:  The easiest way is to
     * pack the data from widest to narrowest type.
     *
     * @param  vbo is the pre-existing VBO index to load with data
     * @result is a function which can be invoked from the GL thread
     *         to bind the buffer, set the pointers and paint
     */
    def upload(vbo: Int) = {
        val count = vb.length / 3
        glBindBuffer(GL_ARRAY_BUFFER, vbo)
        // Pack attribute arrays of all types into one buffer.  Ensure
        // alignment of each segment is acceptable for type
        glBufferData(GL_ARRAY_BUFFER, vb.bytes + sb.bytes, GL_DYNAMIC_DRAW)
        glBufferSubData(GL_ARRAY_BUFFER, 0, vb.result)
        val sbStart = vb.bytes
        glBufferSubData(GL_ARRAY_BUFFER, sbStart, sb.result)
        glBindBuffer(GL_ARRAY_BUFFER, 0)

        def paint() {
            if (count != 0) {
                glBindBuffer(GL_ARRAY_BUFFER, vbo)
                glVertexPointer(3, GL_FLOAT, 0, 0)
                glTexCoordPointer(2, GL_SHORT, 5*sb.sizeof, sbStart)
                glColorPointer(3, GL_SHORT, 5*sb.sizeof, sbStart + 2*sb.sizeof)
                glDrawArrays(GL_QUADS, 0, count)
            }
        }
        paint _
    }
}

class Renderable {
    var dirty = false
    lazy val vbo = glGenBuffers()

    var painter: () => Unit = null

    def update(bound: AABB)(f: Render.WorldRenderer) {
        if (!dirty) {
            dirty = true
            Render.updateMbox send Render.Update(this, bound, f)
        }
    }

    def upload(context: RenderContext) {
        painter = context.upload(vbo)
    }

    def paint() {
        Render.updateFinish()
        if (painter != null) {
            glEnableClientState(GL_TEXTURE_COORD_ARRAY)
            glEnableClientState(GL_COLOR_ARRAY)
            glEnableClientState(GL_VERTEX_ARRAY)
            Texture.Terrain.bind(true)
            glMatrixMode(GL_TEXTURE)
            glLoadIdentity()
            glScalef(1f/16f, 1f/16f, 1f)
            painter()
            glBindBuffer(GL_ARRAY_BUFFER, 0)
            glLoadIdentity()
            glDisableClientState(GL_VERTEX_ARRAY)
            glDisableClientState(GL_TEXTURE_COORD_ARRAY)
            glDisableClientState(GL_COLOR_ARRAY)
        }
    }
}

object Render {
    val grassTop = ConstVec2i(0, 0)
    val grassBot = ConstVec2i(1, 0)
    val grassSide = ConstVec2i(2, 0)
    val stoneAll = ConstVec2i(0, 1)
    val glassAll = ConstVec2i(0, 2)
    val cobblestoneAll = ConstVec2i(1, 1)
    val waterAll = ConstVec2i(2, 2)
    val sandAll = ConstVec2i(3, 0)
    val whiteAll = ConstVec2i(10, 10)

    def texture(block: Block) = block match {
        case Grass => (grassTop, grassSide, grassBot)
        case Stone => (stoneAll, stoneAll, stoneAll)
        case Glass => (glassAll, glassAll, glassAll)
        case Water => (waterAll, waterAll, waterAll)
        case Sand  => (sandAll, sandAll, sandAll)
        case Cobblestone => (cobblestoneAll, cobblestoneAll, cobblestoneAll)
        case Noise(c) => (whiteAll, whiteAll, whiteAll)
        case _     => (whiteAll, whiteAll, whiteAll)
    }

    def renderOpaqueBlock(rc: RenderContext, l: inVec3i, b: Block) = b match {
        case Glass | Water | Air => Unit
        case _ =>
            val tex = texture(b)
            rc.cube(l, b, tex._1, tex._2, tex._3)
    }

    def renderTranslucentBlock(rc: RenderContext, l: inVec3i, b: Block) = b match {
        case Glass | Water =>
            val tex = texture(b)
            rc.cube(l, b, tex._1, tex._2, tex._3)
        case _ => Unit
    }

    type WorldRenderer = (RenderContext, inVec3i, Block) => Unit

    def renderWorld(bound: AABB)(f: WorldRenderer) {
        World.foreach(bound) {
            (loc, block) => f(context, loc, block)
        }
    }

    class Message
    case class Update(rable: Renderable, region: AABB, f: WorldRenderer)
        extends Message
    case class Done(rable: Renderable) extends Message
    case object Next extends Message

    val context = new RenderContext
    val updateMbox = new MailBox
    var updateDone: Message = null

    def updateFinish() {
        updateDone match {
            case Done(rable) =>
                rable.upload(context)
                updateDone = null
                updateMbox send Next
            case null =>
                Unit
        }
    }

    def init() {
        spawn {
            while (true) {
                updateMbox receive {
                    case Update(rable, region, f) =>
                        if (rable.dirty) {
                            rable.dirty = false
                            context.clear()
                            renderWorld(region)(f)
                            updateDone = Done(rable)
                            updateMbox receive {
                                case Next => Unit
                            }
                        }
                }
            }
        }
    }
}
