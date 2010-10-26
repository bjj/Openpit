package org.openpit.ui

import org.lwjgl.opengl.GL11._
import org.lwjgl.util.glu._

import simplex3d.math.floatm._
import org.openpit.util.AABB

object Layer {
    abstract class Message
    case class Update(region: AABB) extends Message
    case object Shutdown extends Message

    private var _all: List[Layer] = Nil
    def all = _all
    def all_= (xs: List[Layer]) { _all = xs.sorted }

    def update(region: AABB = AABB.Everywhere) {
        for (layer <- all) layer.update(region)
    }

    def init() {
        import hud.Crosshair
        all = Crosshair :: SelectLayer :: all;
        val div = 32
        for (x <- -256 to 256 by div; y <- -256 to 256 by div) {
            all = new TerrainLayer(new AABB(Vec3f(x,y,-100),
                                            Vec3f(x+div, y+div, 100))) ::
                  new GlassLayer  (new AABB(Vec3f(x,y,-100),
                                            Vec3f(x+div, y+div, 100))) ::
                  all;
        }
        update();
    }

    var blending = false
}

abstract class Layer(val z:Int) extends Ordered[Layer] {
    def visible = true
    def visible(frustum: List[Vec4f]): Boolean = visible

    lazy val displayList = glGenLists(1)
    lazy val renderable = new Renderable

    def compare(that: Layer) = this.z - that.z

    def update(region: AABB)
    def paint()
    def dopaint() = glCallList(displayList)
    def doblend(on: Boolean) = {
        if (on != Layer.blending) {
            if (on) {
                glEnable(GL_BLEND)
                glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
            } else {
                glDisable(GL_BLEND)
            }
            Layer.blending = on
        }
    }
}

abstract class Layer3d(zz: Int, val blend: Boolean) extends Layer(zz) {
    def paint() {
        import org.openpit.ui.Projection

        if (displayList != 0) {
            Projection.perspective()
            doblend(blend)
            dopaint()
        }
    }
}

abstract class Layer2d(zz: Int) extends Layer(zz) {
    def paint() {
        import org.openpit.ui.Projection

        if (displayList != 0) {
            Projection.ortho()
            doblend(true)
            dopaint()
        }
    }
}
