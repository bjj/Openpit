package org.openpit.ui

import org.lwjgl.opengl.GL11._
import org.lwjgl.util.glu._
import simplex3d.math.intm._
import simplex3d.math.floatm._

import org.openpit.util.ImplicitGL._
import org.openpit.world._
import org.openpit.world.blocks._

object Render {

    def cube(loc: Vec3i, b: Block, top: Vec2f, side: Vec2f, bot: Vec2f) {

        def occluded(dir: ConstVec3i) = {
            World.get(loc + dir).getOrElse(Air) match {
                    case Air => false
                    case Glass() => b.isInstanceOf[Glass]
                    case Water() => b.isInstanceOf[Water]
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
            glTexCoord2f(u, V); glVertex3i(x,Y,Z)
            glTexCoord2f(u, v); glVertex3i(x,y,Z)
            glTexCoord2f(U, v); glVertex3i(X,y,Z)
            glTexCoord2f(U, V); glVertex3i(X,Y,Z)
        }

        u = side.x
        v = side.y
        U = u + 1f/16f
        V = v + 1f/16f

        if (!occluded(ConstVec3i(0,-1,0))) {
            glTexCoord2f(u, v); glVertex3i(x,y,Z)
            glTexCoord2f(u, V); glVertex3i(x,y,z)
            glTexCoord2f(U, V); glVertex3i(X,y,z)
            glTexCoord2f(U, v); glVertex3i(X,y,Z)
        }

        if (!occluded(ConstVec3i(1,0,0))) {
            glTexCoord2f(U, v); glVertex3i(X,y,Z)
            glTexCoord2f(U, V); glVertex3i(X,y,z)
            glTexCoord2f(u, V); glVertex3i(X,Y,z)
            glTexCoord2f(u, v); glVertex3i(X,Y,Z)
        }

        if (!occluded(ConstVec3i(0,1,0))) {
            glTexCoord2f(u, v); glVertex3i(X,Y,Z)
            glTexCoord2f(u, V); glVertex3i(X,Y,z)
            glTexCoord2f(U, V); glVertex3i(x,Y,z)
            glTexCoord2f(U, v); glVertex3i(x,Y,Z)
        }

        if (!occluded(ConstVec3i(-1,0,0))) {
            glTexCoord2f(U, v); glVertex3i(x,Y,Z)
            glTexCoord2f(U, V); glVertex3i(x,Y,z)
            glTexCoord2f(u, V); glVertex3i(x,y,z)
            glTexCoord2f(u, v); glVertex3i(x,y,Z)
        }

        if (!occluded(ConstVec3i(0,0,-1))) {
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

    val grassTop = Vec2f(0f, 0f)
    val grassBot = Vec2f(1f/16f, 0f)
    val grassSide = Vec2f(2f/16f, 0f)
    val stoneAll = Vec2f(0, 1f/16f)
    val glassAll = Vec2f(0, 2f/16f)
    val cobblestoneAll = Vec2f(1f/16f, 1f/16f)
    val waterAll = Vec2f(2f/16f, 2f/16f)

    def grass(loc: Vec3i, b: Block) { cube(loc, b, grassTop, grassSide, grassBot) }
    def stone(loc: Vec3i, b: Block) { cube(loc, b, stoneAll, stoneAll, stoneAll) }
    def glass(loc: Vec3i, b: Block) { cube(loc, b, glassAll, glassAll, glassAll) }
    def cobblestone(loc: Vec3i, b: Block) { cube(loc, b, cobblestoneAll, cobblestoneAll, cobblestoneAll) }
    def water(loc: Vec3i, b: Block) { cube(loc, b, waterAll, waterAll, waterAll) }

    def renderOpaqueBlock(l: Vec3i, b: Block) = b match {
        case Grass() => grass(l, b)
        case Stone() => stone(l, b)
        case Cobblestone() => cobblestone(l, b)
        case _ => Unit
    }

    def renderTranslucentBlock(l: Vec3i, b: Block) = b match {
        case Glass() => glass(l, b)
        case Water() => water(l, b)
        case _ => Unit
    }

    def renderWorld(s: (Vec3i, Block)=>Unit) {
        Texture.Terrain.bind(true)
        glColor4f(1,1,1,1)
        glBegin(GL_QUADS)
        World.foreach(s)
        glEnd()
    }

    def makeDisplayList(s: (Vec3i, Block) => Unit) = {
        val index = glGenLists(1)
        glNewList(index, GL_COMPILE)
        renderWorld(s)
        glEndList()
        index
    }

    def updateDisplayList(list: Int, s: (Vec3i, Block) => Unit) = {
        Texture.Terrain // force load
        glMatrixMode(GL_MODELVIEW)
        glPushMatrix()
        glLoadIdentity()
        glNewList(list, GL_COMPILE)
        renderWorld(s)
        glEndList()
        glPopMatrix()
    }

    def render() {
    }

    def init() {
    }
}
