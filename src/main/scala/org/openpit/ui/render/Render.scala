package org.openpit.ui

import org.lwjgl.opengl.GL11._
import org.lwjgl.util.glu._
import simplex3d.math.intm._
import simplex3d.math.floatm._

import org.openpit.util.ImplicitGL._
import org.openpit.ui.Texture
import org.openpit.world._
import org.openpit.world.blocks._
import render.Camera

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
    val glassAll = Vec2f(0, 2f/16f)
    val cobblestoneAll = Vec2f(1f/16f, 1f/16f)

    def grass(loc: Vec3i) { cube(loc, grassTop, grassSide, grassBot) }
    def stone(loc: Vec3i) { cube(loc, stoneAll, stoneAll, stoneAll) }
    def glass(loc: Vec3i) { cube(loc, glassAll, glassAll, glassAll) }
    def cobblestone(loc: Vec3i) { cube(loc, cobblestoneAll, cobblestoneAll, cobblestoneAll) }

    def renderOpaqueBlock(l: Vec3i, b: Block) = b match {
	case Grass() => grass(l)
	case Stone() => stone(l)
	case Cobblestone() => cobblestone(l)
	case _ => Unit
    }

    def renderTranslucentBlock(l: Vec3i, b: Block) = b match {
	case Glass() => glass(l)
	case _ => Unit
    }

    def renderWorld(s: (Vec3i, Block)=>Unit) {
	Texture.Terrain.bind
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

    lazy val opaqueDisplayList = makeDisplayList(renderOpaqueBlock)
    lazy val translucentDisplayList = makeDisplayList(renderTranslucentBlock)

    def render() {
	glClear(GL_COLOR_BUFFER_BIT |
		GL_STENCIL_BUFFER_BIT |
		GL_DEPTH_BUFFER_BIT)

	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	// view frustrum should track fog setup
	GLU.gluPerspective(40, aspect, 0.5f, 100)
	Camera.look()

	glCallList(opaqueDisplayList)
	glEnable(GL_BLEND)
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
	glCallList(translucentDisplayList)
	glDisable(GL_BLEND)
    }

    var aspect = 1.0f
    def reshape(width: Int, height: Int) {
	aspect = (width toFloat) / height
    }

    def init() {
	glEnable(GL_DEPTH_TEST)
	glEnable(GL_CULL_FACE)
	glEnable(GL_TEXTURE_2D)
	glShadeModel(GL_SMOOTH)
	glClearColor(135f/255f, 205f/255f, 222f/255f, 1.0f)

	glFogi(GL_FOG_MODE, GL_LINEAR)
	glFog(GL_FOG_COLOR, Array(198f/255f, 215f/255f, 216f/255f, 1.0f))
	glFogf(GL_FOG_START, 50f)
	glFogf(GL_FOG_END, 100f)  // with linear, that's where it goes opaque
	glEnable(GL_FOG)
    }
}
