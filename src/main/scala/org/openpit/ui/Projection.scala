package org.openpit.ui

import org.lwjgl.opengl.GL11._
import org.lwjgl.util.glu._

object Projection {
    var current: () => Unit = null

    private def setup_ortho() {
        glOrtho(0, Window.width, Window.height, 0, 0, 1)
    }

    val maxRenderDistance = 250f
    private def setup_perspective() {
        import org.openpit.ui.Camera
        // view frustrum should track fog setup
        GLU.gluPerspective(40, Window.aspect, 0.001f, maxRenderDistance)
        Camera.look()
    }

    def set(way: () => Unit) {
        if (way != current) {
            glMatrixMode(GL_MODELVIEW)
            glLoadIdentity()
            glMatrixMode(GL_PROJECTION)
            glLoadIdentity()
            current = way
            current()
        }
    }

    def ortho() { set(setup_ortho) }
    def perspective() { set(setup_perspective) }
}

