package org.openpit.ui

import org.lwjgl.opengl.GL11._
import org.lwjgl.util.glu._
import simplex3d.math.intm._
import simplex3d.math.floatm._

import org.openpit.ui.Texture
import org.openpit.world._
import org.openpit.world.blocks._

object Render {

    def cube(loc: Vec3i, top: Vec2f, side: Vec2f, bot: Vec2f) {
	var u = top.x
	var v = top.y
	var U = u + 1f/16f
	var V = v + 1f/16f
	val x = loc.x
	val y = loc.y
	val z = loc.z
	val X = x + 1
	val Y = y + 1
	val Z = z - 1

	glTexCoord2f(u, V); glVertex3i(x,Y,z)
	glTexCoord2f(u, v); glVertex3i(x,y,z)
	glTexCoord2f(U, v); glVertex3i(X,y,z)
	glTexCoord2f(U, V); glVertex3i(X,Y,z)

	u = side.x
	v = side.y
	U = u + 1f/16f
	V = v + 1f/16f

	glTexCoord2f(u, v); glVertex3i(x,y,z)
	glTexCoord2f(u, V); glVertex3i(x,y,Z)
	glTexCoord2f(U, V); glVertex3i(X,y,Z)
	glTexCoord2f(U, v); glVertex3i(X,y,z)

	glTexCoord2f(U, v); glVertex3i(X,y,z)
	glTexCoord2f(U, V); glVertex3i(X,y,Z)
	glTexCoord2f(u, V); glVertex3i(X,Y,Z)
	glTexCoord2f(u, v); glVertex3i(X,Y,z)

	glTexCoord2f(u, v); glVertex3i(X,Y,z)
	glTexCoord2f(u, V); glVertex3i(X,Y,Z)
	glTexCoord2f(U, V); glVertex3i(x,Y,Z)
	glTexCoord2f(U, v); glVertex3i(x,Y,z)

	glTexCoord2f(U, v); glVertex3i(x,Y,z)
	glTexCoord2f(U, V); glVertex3i(x,Y,Z)
	glTexCoord2f(u, V); glVertex3i(x,y,Z)
	glTexCoord2f(u, v); glVertex3i(x,y,z)

	u = bot.x
	v = bot.y
	U = u + 1f/16f
	V = v + 1f/16f

	glTexCoord2f(U, V); glVertex3i(X,y,Z)
	glTexCoord2f(U, v); glVertex3i(x,y,Z)
	glTexCoord2f(u, v); glVertex3i(x,Y,Z)
	glTexCoord2f(u, V); glVertex3i(X,Y,Z)
    }

    val grassTop = Vec2f(0f, 0f)
    val grassBot = Vec2f(1f/16f, 0f)
    val grassSide = Vec2f(2f/16f, 0f)
    val stoneAll = Vec2f(0, 1f/16f)

    def grass(loc: Vec3i) { cube(loc, grassTop, grassSide, grassBot) }
    def stone(loc: Vec3i) { cube(loc, stoneAll, stoneAll, stoneAll) }

    def renderBlock(l: Vec3i, b: Block) = b match {
	case Grass() => grass(l)
	case Stone() => stone(l)
	case Air     => Unit
    }

    def renderWorld() {
	Texture.Terrain.bind
	glColor4f(1,1,1,1)
	glBegin(GL_QUADS)
	World.foreach(renderBlock)
	glEnd()
    }

    lazy val displayList = {
	val index = glGenLists(1)
	glNewList(index, GL_COMPILE)
	renderWorld()
	glEndList()
	index
    }

    def render() {
	glCallList(displayList)
    }
}
