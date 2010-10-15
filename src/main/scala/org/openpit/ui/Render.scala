package org.openpit.ui

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15._
import org.lwjgl.util.glu._
import simplex3d.math.intm._
import simplex3d.math.floatm._

import org.openpit.util._
import ImplicitGL._
import org.openpit.world._
import org.openpit.world.blocks._

object Render {

    final val SIZEOF_FLOAT = 4

    var vb = new FloatBufferBuffer()
    var eb = new ShortBufferBuffer()
    var vbos = Array.fill(2200, 3)(0) // gross hack to associate VBOs with displaylists

    def cube(loc: inVec3i, b: Block, top: Vec2i, side: Vec2i, bot: Vec2i) {

        def light(dir: ConstVec3i, dim: Int = 1): Vec3i = {
            if (dim == 6)
                ConstVec3i(32767, 32767, 32767)
            else {
                World.get(loc + dir + Dir.Up*dim).getOrElse(Air) match {
                        case Air | Glass | Water => light(dir, dim + 1)
                        case _ => val bright = (5000 * dim); Vec3i(bright, bright, bright)
                }
            }
        }
/*
def light(dir: ConstVec3i, dim: Int = 1) = ConstVec3i(32767, 32767, 32767)
*/

        object Dir {
            val Up = ConstVec3i(0,0,1)
            val Down = ConstVec3i(0,0,-1)
            val Fore = ConstVec3i(0,1,0)
            val Back = ConstVec3i(0,-1,0)
            val Left = ConstVec3i(-1,0,0)
            val Right = ConstVec3i(1,0,0)
        }

        def occluded(dir: inVec3i) = {
            World.get(loc + dir).getOrElse(Air) match {
                    case Air => false
                    case Glass => b == Glass
                    case Water => b == Water
                    case _ => true
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

        import Dir._
        if (!occluded(Up)) {
            val l = light(ConstVec3i(0,0,0))
            eb += Vec2i(u, V); eb += l; vb += Vec3f(x,Y,Z)
            eb += Vec2i(u, v); eb += l; vb += Vec3f(x,y,Z)
            eb += Vec2i(U, v); eb += l; vb += Vec3f(X,y,Z)
            eb += Vec2i(U, V); eb += l; vb += Vec3f(X,Y,Z)
        }

        u = side.x
        v = side.y
        U = u + 1
        V = v + 1

        if (!occluded(Back)) {
            val l = light(ConstVec3i(0,-1, 0))
            eb += Vec2i(u, v); eb += l; vb += Vec3f(x,y,Z)
            eb += Vec2i(u, V); eb += l; vb += Vec3f(x,y,z)
            eb += Vec2i(U, V); eb += l; vb += Vec3f(X,y,z)
            eb += Vec2i(U, v); eb += l; vb += Vec3f(X,y,Z)
        }

        if (!occluded(Right)) {
            val l = light(ConstVec3i(1,0, 0))
            eb += Vec2i(U, v); eb += l; vb += Vec3f(X,y,Z)
            eb += Vec2i(U, V); eb += l; vb += Vec3f(X,y,z)
            eb += Vec2i(u, V); eb += l; vb += Vec3f(X,Y,z)
            eb += Vec2i(u, v); eb += l; vb += Vec3f(X,Y,Z)
        }

        if (!occluded(Fore)) {
            val l = light(ConstVec3i(0,1, 0))
            eb += Vec2i(u, v); eb += l; vb += Vec3f(X,Y,Z)
            eb += Vec2i(u, V); eb += l; vb += Vec3f(X,Y,z)
            eb += Vec2i(U, V); eb += l; vb += Vec3f(x,Y,z)
            eb += Vec2i(U, v); eb += l; vb += Vec3f(x,Y,Z)
        }

        if (!occluded(Left)) {
            val l = light(ConstVec3i(-1,0, 0))
            eb += Vec2i(U, v); eb += l; vb += Vec3f(x,Y,Z)
            eb += Vec2i(U, V); eb += l; vb += Vec3f(x,Y,z)
            eb += Vec2i(u, V); eb += l; vb += Vec3f(x,y,z)
            eb += Vec2i(u, v); eb += l; vb += Vec3f(x,y,Z)
        }

        if (!occluded(Down)) {
            val l = light(ConstVec3i(0,0, 0))
            u = bot.x
            v = bot.y
            U = u + 1
            V = v + 1

            eb += Vec2i(U, V); eb += l; vb += Vec3f(X,y,z)
            eb += Vec2i(U, v); eb += l; vb += Vec3f(x,y,z)
            eb += Vec2i(u, v); eb += l; vb += Vec3f(x,Y,z)
            eb += Vec2i(u, V); eb += l; vb += Vec3f(X,Y,z)
        }
    }

    val grassTop = ConstVec2i(0, 0)
    val grassBot = ConstVec2i(1, 0)
    val grassSide = ConstVec2i(2, 0)
    val stoneAll = ConstVec2i(0, 1)
    val glassAll = ConstVec2i(0, 2)
    val cobblestoneAll = ConstVec2i(1, 1)
    val waterAll = ConstVec2i(2, 2)
    val sandAll = ConstVec2i(3, 0)
    val whiteAll = ConstVec2i(10, 10)

    def grass(loc: inVec3i, b: Block) { cube(loc, b, grassTop, grassSide, grassBot) }
    def stone(loc: inVec3i, b: Block) { cube(loc, b, stoneAll, stoneAll, stoneAll) }
    def glass(loc: inVec3i, b: Block) { cube(loc, b, glassAll, glassAll, glassAll) }
    def cobblestone(loc: inVec3i, b: Block) { cube(loc, b, cobblestoneAll, cobblestoneAll, cobblestoneAll) }
    def water(loc: inVec3i, b: Block) { cube(loc, b, waterAll, waterAll, waterAll) }
    def sand(loc: inVec3i, b: Block) { cube(loc, b, sandAll, sandAll, sandAll) }
    def noise(loc: inVec3i, c: ConstVec3f) {
      var u = whiteAll.x
      var v = whiteAll.y
      var U = u + 1f/16f
      var V = v + 1f/16f

      val x = loc.x
      val y = loc.y
      val z = loc.z
      val X = x + 1
      val Y = y + 1
      val Z = z + 1

//      glEnd()
//      glBegin(GL_POINTS)
//
//      glColor3f(c.x, c.y, c.z)
//      glVertex3i(x, y, z)
//
//      glEnd()
//      glBegin(GL_QUADS)

      glColor3f(c.x, c.y, c.z)
      glTexCoord2f(u, V); glVertex3i(x,Y,Z)
      glTexCoord2f(u, v); glVertex3i(x,y,Z)
      glTexCoord2f(U, v); glVertex3i(X,y,Z)
      glTexCoord2f(U, V); glVertex3i(X,Y,Z)


      glColor3f(c.x, c.y, c.z)
      glTexCoord2f(U, v); glVertex3i(x,Y,Z)
      glTexCoord2f(U, V); glVertex3i(x,Y,z)
      glTexCoord2f(u, V); glVertex3i(x,y,z)
      glTexCoord2f(u, v); glVertex3i(x,y,Z)

      glColor3f(c.x, c.y, c.z)
      glTexCoord2f(u, v); glVertex3i(x,y,Z)
      glTexCoord2f(u, V); glVertex3i(x,y,z)
      glTexCoord2f(U, V); glVertex3i(X,y,z)
      glTexCoord2f(U, v); glVertex3i(X,y,Z)

      glColor3f(c.x, c.y, c.z)
      glTexCoord2f(U, v); glVertex3i(X,y,Z)
      glTexCoord2f(U, V); glVertex3i(X,y,z)
      glTexCoord2f(u, V); glVertex3i(X,Y,z)
      glTexCoord2f(u, v); glVertex3i(X,Y,Z)

      glColor3f(c.x, c.y, c.z)
      glTexCoord2f(u, v); glVertex3i(X,Y,Z)
      glTexCoord2f(u, V); glVertex3i(X,Y,z)
      glTexCoord2f(U, V); glVertex3i(x,Y,z)
      glTexCoord2f(U, v); glVertex3i(x,Y,Z)

      glColor3f(c.x, c.y, c.z)
      glTexCoord2f(U, v); glVertex3i(x,Y,Z)
      glTexCoord2f(U, V); glVertex3i(x,Y,z)
      glTexCoord2f(u, V); glVertex3i(x,y,z)
      glTexCoord2f(u, v); glVertex3i(x,y,Z)

      glColor3f(c.x, c.y, c.z)
      glTexCoord2f(U, V); glVertex3i(X,y,z)
      glTexCoord2f(U, v); glVertex3i(x,y,z)
      glTexCoord2f(u, v); glVertex3i(x,Y,z)
      glTexCoord2f(u, V); glVertex3i(X,Y,z)
    }

    def renderOpaqueBlock(l: inVec3i, b: Block) = b match {
        case Grass => grass(l, b)
        case Stone => stone(l, b)
        case Cobblestone => cobblestone(l, b)
        case Sand => sand(l, b)
        case Noise(color) => noise(l, color)
        case _ => Unit
    }

    def renderTranslucentBlock(l: inVec3i, b: Block) = b match {
        case Glass => glass(l, b)
        case Water => water(l, b)
        case _ => Unit
    }

    def renderWorld(bound: AABB)(s: (inVec3i, Block)=>Unit) {
        World.foreach(bound)(s)
        //Main.debugTime("loop", World.foreach(bound){case _=>Unit} )
    }

    def updateDisplayList(list: Int, bound: AABB)(s: (inVec3i, Block) => Unit) = {
        Texture.Terrain // force load
        eb.clear()
        vb.clear()
        renderWorld(bound)(s)
        vbos(list)(2) = vb.length / 3

        if (vbos(list)(0) == 0) {
            vbos(list)(0) = glGenBuffers()
            vbos(list)(1) = glGenBuffers()
        }
        glBindBuffer(GL_ARRAY_BUFFER, vbos(list)(0))
        glBufferData(GL_ARRAY_BUFFER, vb.bytes, GL_DYNAMIC_DRAW)
        glBufferSubData(GL_ARRAY_BUFFER, 0, vb.result)
        glBindBuffer(GL_ARRAY_BUFFER, vbos(list)(1))
        glBufferData(GL_ARRAY_BUFFER, eb.bytes, GL_DYNAMIC_DRAW)
        glBufferSubData(GL_ARRAY_BUFFER, 0, eb.result)
        println("vb: " + vb.length + " eb: " + eb.length + " to " + vbos(list)(0) +"/"+vbos(list)(1))

        glBindBuffer(GL_ARRAY_BUFFER, 0)
    }

    def gogo(list: Int) {
        if (vbos(list)(0) != 0) {
            glEnableClientState(GL_TEXTURE_COORD_ARRAY)
            glEnableClientState(GL_COLOR_ARRAY)
            glEnableClientState(GL_VERTEX_ARRAY)
            glBindBuffer(GL_ARRAY_BUFFER, vbos(list)(1))
            glTexCoordPointer(2, GL_SHORT, 5*eb.sizeof, 0)
            glColorPointer(3, GL_SHORT, 5*eb.sizeof, 2*eb.sizeof)
            glBindBuffer(GL_ARRAY_BUFFER, vbos(list)(0))
            glVertexPointer(3, GL_FLOAT, 0*vb.sizeof, 0*vb.sizeof)
            Texture.Terrain.bind(true)
            glMatrixMode(GL_TEXTURE)
            glPushMatrix()
            glScalef(1f/16f, 1f/16f, 1f)
            glDrawArrays(GL_QUADS, 0, vbos(list)(2))
            glBindBuffer(GL_ARRAY_BUFFER, 0)
            glPopMatrix()
            glDisableClientState(GL_VERTEX_ARRAY)
            glDisableClientState(GL_TEXTURE_COORD_ARRAY)
            glDisableClientState(GL_COLOR_ARRAY)
        }
    }

    def render() {
    }

    def init() {
    }
}
