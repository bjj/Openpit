package org.openpit.ui

import org.lwjgl.opengl.Display
import org.lwjgl.input.{Keyboard, Mouse}

object Input {

    sealed abstract class Input
    case object Quit extends Input
    case class Move(val dx: Int, val dy: Int, val yaw: Float, val pitch: Float,
                    val jump: Boolean, val crouch: Boolean) extends Input
    case object Inventory extends Input
    case object Menu extends Input

    def init() {
        Keyboard.create()
        Mouse.create()
        Mouse.setGrabbed(true)
    }

    def cleanup() {
        Mouse.setGrabbed(false)
    }

    var KEY_LEFT = Keyboard.KEY_A
    var KEY_RIGHT = Keyboard.KEY_D
    var KEY_FORWARD = Keyboard.KEY_W
    var KEY_BACKWARD = Keyboard.KEY_S
    var KEY_CROUCH = Keyboard.KEY_LCONTROL
    var KEY_JUMP = Keyboard.KEY_SPACE

    var LOOK_SENSITIVITY = 0.3f
    var LOOK_INVERT = 1f

    def input() = {
        if (Keyboard.isKeyDown(Keyboard.KEY_ESCAPE)) {
            Quit // Menu
        } else if (Display.isCloseRequested()) {
            Quit
        } else {
            def dir(l: Int, r: Int) = {
                val ldown = Keyboard.isKeyDown(l)
                val rdown = Keyboard.isKeyDown(r)
                if (ldown && rdown)  0
                else if (ldown)     -1
                else if (rdown)      1
                else                 0
            }

            Move(dx = dir(KEY_LEFT, KEY_RIGHT),
                 dy = dir(KEY_BACKWARD, KEY_FORWARD),
                 yaw = -Mouse.getDX.toFloat * LOOK_SENSITIVITY,
                 pitch = Mouse.getDY.toFloat * LOOK_SENSITIVITY * LOOK_INVERT,
                 jump = Keyboard.isKeyDown(KEY_JUMP),
                 crouch = Keyboard.isKeyDown(KEY_CROUCH))
        }
    }
}
