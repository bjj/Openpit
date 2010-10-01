package org.openpit.ui

import org.openpit.ui._
import org.lwjgl.opengl.GL11._

object TerrainLayer extends Layer3d(100, false) {
    def update() {
        Render.updateDisplayList(displayList, Render.renderOpaqueBlock)
    }
}

object SelectLayer extends Layer3d(500, true) {
    import simplex3d.math.intm._
    import simplex3d.math.floatm._

    var selected: Option[Vec3i] = None

    def scale = 1.01f

    override def dopaint() = {

        selected.foreach { case loc =>
            val locf = loc - Vec3f.One * (scale - 1f) / 2
            glMatrixMode(GL_MODELVIEW)
            glPushMatrix()
            glTranslatef(locf.x, locf.y, locf.z)
            glCallList(displayList)
            glPopMatrix()
        }
    }

    def update() {
        glNewList(displayList, GL_COMPILE)
        Texture.unbind()
        glColor4f(0.3f,0.3f,0.3f,1);
        glBegin(GL_LINES)
        for (axis <- 0 until 3; p1 <- 0 to 1; p2 <- 0 to 1) {
            val a = Vec3f(Vec3f.Zero);
            a((axis + 1) % 3) = p1; a((axis + 2) % 3) = p2
            val b = Vec3f(a)
            b(axis) = 1
            a *= scale; b *= scale
            glVertex3f(a.x, a.y, a.z); glVertex3f(b.x, b.y, b.z)
        }
        glEnd()
        glEndList()
    }
}

object GlassLayer extends Layer3d(900, true) {
    def update() {
        Render.updateDisplayList(displayList, Render.renderTranslucentBlock)
    }
}
