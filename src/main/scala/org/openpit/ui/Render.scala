package org.openpit.ui

import org.lwjgl.opengl.GL11._
import org.lwjgl.util.glu._
import simplex3d.math.intm._
import simplex3d.math.floatm._

import org.openpit.util._
import ImplicitGL._
import org.openpit.world._
import org.openpit.world.blocks._

object Render {

    def cube(loc: inVec3i, b: Block, top: inVec2f, side: inVec2f, bot: inVec2f) {

        def light(dir: inVec3i, dim: Int = 1) {
            if (dim == 6)
                glColor3f(1.0f, 1.0f, 1.0f)
            else {
                World.get(loc + dir + ConstVec3i(0,0,1)*dim).getOrElse(Air) match {
                        case Air | Glass | Water => light(dir, dim + 1)
                        case _ => val bright = dim.toFloat * 0.15f; glColor3f(bright, bright, bright)
                }
            }
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
        var U = u + 1f/16f
        var V = v + 1f/16f

        import Direction._

        if (!occluded(`Z+`)) {
            light(Vec3i.Zero)
            glTexCoord2f(u, V); glVertex3i(x,Y,Z)
            glTexCoord2f(u, v); glVertex3i(x,y,Z)
            glTexCoord2f(U, v); glVertex3i(X,y,Z)
            glTexCoord2f(U, V); glVertex3i(X,Y,Z)
        }

        u = side.x
        v = side.y
        U = u + 1f/16f
        V = v + 1f/16f

        if (!occluded(`Y-`)) {
            light(`Y-`)
            glTexCoord2f(u, v); glVertex3i(x,y,Z)
            glTexCoord2f(u, V); glVertex3i(x,y,z)
            glTexCoord2f(U, V); glVertex3i(X,y,z)
            glTexCoord2f(U, v); glVertex3i(X,y,Z)
        }

        if (!occluded(`X+`)) {
            light(`X+`)
            glTexCoord2f(U, v); glVertex3i(X,y,Z)
            glTexCoord2f(U, V); glVertex3i(X,y,z)
            glTexCoord2f(u, V); glVertex3i(X,Y,z)
            glTexCoord2f(u, v); glVertex3i(X,Y,Z)
        }

        if (!occluded(`Y+`)) {
            light(`Y+`)
            glTexCoord2f(u, v); glVertex3i(X,Y,Z)
            glTexCoord2f(u, V); glVertex3i(X,Y,z)
            glTexCoord2f(U, V); glVertex3i(x,Y,z)
            glTexCoord2f(U, v); glVertex3i(x,Y,Z)
        }

        if (!occluded(`X-`)) {
            light(`X-`)
            glTexCoord2f(U, v); glVertex3i(x,Y,Z)
            glTexCoord2f(U, V); glVertex3i(x,Y,z)
            glTexCoord2f(u, V); glVertex3i(x,y,z)
            glTexCoord2f(u, v); glVertex3i(x,y,Z)
        }

        if (!occluded(`Z-`)) {
            light(`Z-`)
            u = bot.x
            v = bot.y
            U = u + 1f/16f
            V = v + 1f/16f

            glTexCoord2f(U, V); glVertex3i(X,y,z)
            glTexCoord2f(U, v); glVertex3i(x,y,z)
            glTexCoord2f(u, v); glVertex3i(x,Y,z)
            glTexCoord2f(u, V); glVertex3i(X,Y,z)
        }
    }

    val grassTop = ConstVec2f(0f, 0f)
    val grassBot = ConstVec2f(1f/16f, 0f)
    val grassSide = ConstVec2f(2f/16f, 0f)
    val stoneAll = ConstVec2f(0, 1f/16f)
    val glassAll = ConstVec2f(0, 2f/16f)
    val cobblestoneAll = ConstVec2f(1f/16f, 1f/16f)
    val waterAll = ConstVec2f(2f/16f, 2f/16f)
    val sandAll = ConstVec2f(3f/16f, 0f/16f)

    val whiteAll = ConstVec2f(10f/16f, 10f/16f)

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
        Texture.Terrain.bind(true)
        glColor4f(1,1,1,1)
        glBegin(GL_QUADS)
        World.foreach(bound)(s)
        glEnd()
    }

    def updateDisplayList(list: Int, bound: AABB)(s: (inVec3i, Block) => Unit) = {
        Texture.Terrain // force load
        glMatrixMode(GL_MODELVIEW)
        glPushMatrix()
        glLoadIdentity()
        glNewList(list, GL_COMPILE)
        renderWorld(bound)(s)
        glEndList()
        glPopMatrix()
    }

    def render() {
    }

    def init() {
    }
}
