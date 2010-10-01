package org.openpit.ui

import org.lwjgl.opengl.GL11._
import org.lwjgl.util.glu._

import scala.actors.Actor
import java.lang.System.currentTimeMillis

object Layer {
    abstract class Message
    case object Update extends Message
    case object Shutdown extends Message
}

abstract class Layer(val z:Int) extends Actor with Ordered[Layer] {
    var visible = true
    lazy val displayList = glGenLists(1)

    def compare(that: Layer) = this.z - that.z

    def update()
    def paint()
    def dopaint() = glCallList(displayList)

    // XXX This can't be multithreaded with DisplayLists, but it could
    // after a conversion to VBOs.  If some layers still draw with
    // DisplayLists this would have to have both a threaded update and
    // "invalidate and re-render on demand" flavor
    def act() {
        var running = true

        while (running) {
            receive {
                case Layer.Update => update()
                case Layer.Shutdown => running = false
            }
        }
    }
}

abstract class Layer3d(zz: Int, val blend: Boolean) extends Layer(zz) {
    def paint() {
        import org.openpit.ui.Projection

        if (displayList != 0) {
            Projection.perspective()
            if (blend) {
                glEnable(GL_BLEND)
                glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
            }
            dopaint()
            if (blend)
                glDisable(GL_BLEND)
        }
    }
}

abstract class Layer2d(zz: Int) extends Layer(zz) {
    def paint() {
        import org.openpit.ui.Projection

        if (displayList != 0) {
            Projection.ortho()
            glEnable(GL_BLEND)
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
            dopaint()
            glDisable(GL_BLEND)
        }
    }
}
