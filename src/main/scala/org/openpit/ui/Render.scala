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

    var vb = new FloatBufferBuffer()
    var eb = new IntBufferBuffer()
    var vbos = Array.fill(10, 3)(0) // gross hack to associate VBOs with displaylists

    def cube(loc: Vec3i, b: Block, top: Vec2f, side: Vec2f, bot: Vec2f) {

/*
        def light(dir: ConstVec3i, dim: Int = 1): Vec3f = {
            if (dim == 6)
                Vec3f(1.0f, 1.0f, 1.0f)
            else {
                World.get(loc + dir + ConstVec3i(0,0,1)*dim).getOrElse(Air) match {
<<<<<<< HEAD:src/main/scala/org/openpit/ui/Render.scala
                        case Air | Glass | Water => light(dir, dim + 1)
                        case _ => val bright = dim.toFloat * 0.15f; glColor3f(bright, bright, bright)
=======
                        case Air => light(dir, dim + 1)
                        case Glass() => light(dir, dim + 1)
                        case _ => val bright = dim.toFloat * 0.15f; Vec3f(bright, bright, bright)
>>>>>>> Gigantic ugly VBO hack.:src/main/scala/org/openpit/ui/Render.scala
                }
            }
        }
*/
def light(dir: ConstVec3i, dim: Int = 1) = ConstVec3f(1.0f,1.0f,1.0f)

        def occluded(dir: ConstVec3i) = {
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
        var U = u + 1f/16f
        var V = v + 1f/16f

        if (!occluded(ConstVec3i(0,0,1))) {
            val l = light(ConstVec3i(0,0,0))
            vb += Vec2f(u, V); vb += l; vb += Vec3f(x,Y,Z)
            vb += Vec2f(u, v); vb += l; vb += Vec3f(x,y,Z)
            vb += Vec2f(U, v); vb += l; vb += Vec3f(X,y,Z)
            vb += Vec2f(U, V); vb += l; vb += Vec3f(X,Y,Z)
        }

        u = side.x
        v = side.y
        U = u + 1f/16f
        V = v + 1f/16f

        if (!occluded(ConstVec3i(0,-1,0))) {
            val l = light(ConstVec3i(0,-1, 0))
            vb += Vec2f(u, v); vb += l; vb += Vec3f(x,y,Z)
            vb += Vec2f(u, V); vb += l; vb += Vec3f(x,y,z)
            vb += Vec2f(U, V); vb += l; vb += Vec3f(X,y,z)
            vb += Vec2f(U, v); vb += l; vb += Vec3f(X,y,Z)
        }

        if (!occluded(ConstVec3i(1,0,0))) {
            val l = light(ConstVec3i(1,0, 0))
            vb += Vec2f(U, v); vb += l; vb += Vec3f(X,y,Z)
            vb += Vec2f(U, V); vb += l; vb += Vec3f(X,y,z)
            vb += Vec2f(u, V); vb += l; vb += Vec3f(X,Y,z)
            vb += Vec2f(u, v); vb += l; vb += Vec3f(X,Y,Z)
        }

        if (!occluded(ConstVec3i(0,1,0))) {
            val l = light(ConstVec3i(0,1, 0))
            vb += Vec2f(u, v); vb += l; vb += Vec3f(X,Y,Z)
            vb += Vec2f(u, V); vb += l; vb += Vec3f(X,Y,z)
            vb += Vec2f(U, V); vb += l; vb += Vec3f(x,Y,z)
            vb += Vec2f(U, v); vb += l; vb += Vec3f(x,Y,Z)
        }

        if (!occluded(ConstVec3i(-1,0,0))) {
            val l = light(ConstVec3i(-1,0, 0))
            vb += Vec2f(U, v); vb += l; vb += Vec3f(x,Y,Z)
            vb += Vec2f(U, V); vb += l; vb += Vec3f(x,Y,z)
            vb += Vec2f(u, V); vb += l; vb += Vec3f(x,y,z)
            vb += Vec2f(u, v); vb += l; vb += Vec3f(x,y,Z)
        }

        if (!occluded(ConstVec3i(0,0,-1))) {
            val l = light(ConstVec3i(0,0, 0))
            u = bot.x
            v = bot.y
            U = u + 1f/16f
            V = v + 1f/16f

            vb += Vec2f(U, V); vb += l; vb += Vec3f(X,y,z)
            vb += Vec2f(U, v); vb += l; vb += Vec3f(x,y,z)
            vb += Vec2f(u, v); vb += l; vb += Vec3f(x,Y,z)
            vb += Vec2f(u, V); vb += l; vb += Vec3f(X,Y,z)
        }
    }

    val grassTop = Vec2f(0f, 0f)
    val grassBot = Vec2f(1f/16f, 0f)
    val grassSide = Vec2f(2f/16f, 0f)
    val stoneAll = Vec2f(0, 1f/16f)
    val glassAll = Vec2f(0, 2f/16f)
    val cobblestoneAll = Vec2f(1f/16f, 1f/16f)
    val waterAll = Vec2f(2f/16f, 2f/16f)
    val sandAll = Vec2f(3f/16f, 0f/16f)

    val whiteAll = Vec2f(10f/16f, 10f/16f)

    def grass(loc: Vec3i, b: Block) { cube(loc, b, grassTop, grassSide, grassBot) }
    def stone(loc: Vec3i, b: Block) { cube(loc, b, stoneAll, stoneAll, stoneAll) }
    def glass(loc: Vec3i, b: Block) { cube(loc, b, glassAll, glassAll, glassAll) }
    def cobblestone(loc: Vec3i, b: Block) { cube(loc, b, cobblestoneAll, cobblestoneAll, cobblestoneAll) }
    def water(loc: Vec3i, b: Block) { cube(loc, b, waterAll, waterAll, waterAll) }
    def sand(loc: Vec3i, b: Block) { cube(loc, b, sandAll, sandAll, sandAll) }
    def noise(loc: Vec3i, c: ConstVec3f) {
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

    def renderOpaqueBlock(l: Vec3i, b: Block) = b match {
        case Grass => grass(l, b)
        case Stone => stone(l, b)
        case Cobblestone => cobblestone(l, b)
        case Sand => sand(l, b)
        case Noise(color) => noise(l, color)
        case _ => Unit
    }

    def renderTranslucentBlock(l: Vec3i, b: Block) = b match {
        case Glass => glass(l, b)
        case Water => water(l, b)
        case _ => Unit
    }

    def renderWorld(bound: AABB)(s: (Vec3i, Block)=>Unit) {
        World.foreach(bound)(s)
    }

    def updateDisplayList(list: Int, bound: AABB)(s: (Vec3i, Block) => Unit) = {
        Texture.Terrain // force load
        eb.clear()
        vb.clear()
        renderWorld(bound)(s)
        vbos(list)(2) = vb.length / 8

        if (vbos(list)(0) == 0) {
            vbos(list)(0) = glGenBuffers()
            vbos(list)(1) = glGenBuffers()
        }
        Window.checkErrors()
        glBindBuffer(GL_ARRAY_BUFFER, vbos(list)(0))
        Window.checkErrors()
        glBufferData(GL_ARRAY_BUFFER, vb.length * 4, GL_DYNAMIC_DRAW)
        Window.checkErrors()
        glBufferSubData(GL_ARRAY_BUFFER, 0, vb.result)
        Window.checkErrors()
        //glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbos(list)(1))
        Window.checkErrors()
        println("vb: " + vb.length + " eb: " + eb.length + " to " + vbos(list)(0) +"/"+vbos(list)(1))
        Window.checkErrors()
        //glBufferData(GL_ELEMENT_ARRAY_BUFFER, eb, GL_DYNAMIC_DRAW)
        Window.checkErrors()

        Window.checkErrors()
        glBindBuffer(GL_ARRAY_BUFFER, 0)
        Window.checkErrors()
        //glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0)
    }

    def gogo(list: Int) {
        if (vbos(list)(0) != 0) {
        Window.checkErrors()
            glEnableClientState(GL_TEXTURE_COORD_ARRAY)
        Window.checkErrors()
            glEnableClientState(GL_COLOR_ARRAY)
        Window.checkErrors()
            glEnableClientState(GL_VERTEX_ARRAY)
        Window.checkErrors()
        Window.checkErrors()
            glBindBuffer(GL_ARRAY_BUFFER, vbos(list)(0))
        Window.checkErrors()
            glTexCoordPointer(2, GL_FLOAT, 8*4, 0*4)
            glColorPointer(3, GL_FLOAT, 8*4, 2*4)
            glVertexPointer(3, GL_FLOAT, 8*4, 5*4)
        Window.checkErrors()
            Texture.Terrain.bind(true)
        Window.checkErrors()
            glDrawArrays(GL_QUADS, 0, vbos(list)(2))
        Window.checkErrors()
            glBindBuffer(GL_ARRAY_BUFFER, 0)
        Window.checkErrors()
            glDisableClientState(GL_VERTEX_ARRAY)
            glDisableClientState(GL_TEXTURE_COORD_ARRAY)
            glDisableClientState(GL_COLOR_ARRAY)
        Window.checkErrors()
        }
    }

    def render() {
    }

    def init() {
    }
}
