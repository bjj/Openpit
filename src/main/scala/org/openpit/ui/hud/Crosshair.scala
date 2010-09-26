package org.openpit.ui.hud

import org.lwjgl.opengl.GL11._
import simplex3d.math.intm._
import simplex3d.math.floatm._

import org.openpit.util.ImplicitGL._
import org.openpit.ui.{Texture, Window, Layer2d}

object Crosshair extends Layer2d(1000) {
    val radius = 16f   // XXX ortho setup to draw in screen pix -- should crosshair scale?

    def update() {
        val index = glGenLists(1)
        glNewList(index, GL_COMPILE)
        Texture.Crosshair.bind()
        glColor4f(1,1,1,1)
        glBegin(GL_QUADS)
        val x = Window.width / 2
        val y = Window.height / 2
        glTexCoord2f(0, 0); glVertex3f(x - radius, y - radius, 0)
        glTexCoord2f(0, 1); glVertex3f(x - radius, y + radius, 0)
        glTexCoord2f(1, 1); glVertex3f(x + radius, y + radius, 0)
        glTexCoord2f(1, 0); glVertex3f(x + radius, y - radius, 0)
        glEnd()
        glEndList()
        replace(index)
    }
}
