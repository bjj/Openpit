package org.openpit.ui

import org.lwjgl.opengl.GL11._
import org.openpit.util._

import simplex3d.math.floatm._

abstract class GeoLayer(val bound: AABB, var zz: Int, var trans: Boolean)
    extends Layer3d(zz, trans) {

    /**
     * We want to update if we intersect an update region, but when we
     * traverse the World we don't want to process blocks from adjacent
     * regions.
     */
    val renderBound = new AABB(bound.min + 0.5f, bound.max - 0.5f)

    def renderFunc: Render.WorldRenderer

    /**
     * Layers are sorted by draw depth, so add a secondary key of
     * proximity to Camera.eye
     */
    override def compare(other: Layer) = other match {
        case that: GeoLayer =>
            super.compare(that) match {
                case 0 =>
                    import simplex3d.math.floatm.FloatMath._
                    val delta = distance(bound.center, Camera.eye) -
                                distance(that.bound.center, Camera.eye)
                    if (delta < 0)      -1
                    else if (delta > 0)  1
                    else                 0
                case i: Int => i
            }
        case that: Layer => super.compare(that)
    }

    /**
     * Add view culling visibility test
     */
    override def visible(frustum: List[Vec4f]) = bound intersects frustum

    def update(region: AABB) {
        if (bound intersects region)
            renderable.update(renderBound)(renderFunc)
    }
    override def dopaint() {
        renderable.paint()
    }
}

class TerrainLayer(bound: AABB) extends GeoLayer(bound, 100, false) {
    def renderFunc = Render.renderOpaqueBlock
}

class GlassLayer(bound: AABB) extends GeoLayer(bound, 900, true) {
    def renderFunc = Render.renderTranslucentBlock
}

object SelectLayer extends Layer3d(500, true) {
    import simplex3d.math.intm._
    import simplex3d.math.floatm._

    var selected: Option[Vec3i] = None
    var distance = Float.MaxValue

    var target: Option[Vec3i] = None

    def scale = 1.0025f

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

    def update(region: AABB) {
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
