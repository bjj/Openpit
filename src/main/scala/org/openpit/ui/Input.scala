package org.openpit.ui

import org.lwjgl.opengl.Display
import org.lwjgl.input.{Keyboard, Mouse}

import simplex3d.math.intm._

object Input {

    sealed abstract class Input
    case object Quit extends Input
    case class Move(val walk: Vec3i,
                    val yaw: Float, val pitch: Float,
                    val jump: Boolean, val crouch: Boolean,
                    val tool: Boolean, val use: Boolean) extends Input
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

    var KEY_MENU = Keyboard.KEY_ESCAPE
    var KEY_INVENTORY = Keyboard.KEY_I

    var LOOK_SENSITIVITY = 0.3f
    var LOOK_INVERT = 1f

    var KEY_UP = Keyboard.KEY_Z
    var KEY_DOWN = Keyboard.KEY_X

    def input() = {
        if (Keyboard.isKeyDown(KEY_MENU)) {
            Quit // Menu
        } else if (Display.isCloseRequested()) {
            Quit
        } else if (Keyboard.isKeyDown(KEY_INVENTORY)) {
            Inventory
        } else {
            def dir(l: Int, r: Int) = {
                val ldown = Keyboard.isKeyDown(l)
                val rdown = Keyboard.isKeyDown(r)
                if (ldown && rdown)  0
                else if (ldown)     -1
                else if (rdown)      1
                else                 0
            }

            Move(walk = Vec3i(dir(KEY_LEFT, KEY_RIGHT),
                              dir(KEY_BACKWARD, KEY_FORWARD),
                              dir(KEY_UP, KEY_DOWN)),
                 yaw = -Mouse.getDX.toFloat * LOOK_SENSITIVITY,
                 pitch = Mouse.getDY.toFloat * LOOK_SENSITIVITY * LOOK_INVERT,
                 jump = Keyboard.isKeyDown(KEY_JUMP),
                 crouch = Keyboard.isKeyDown(KEY_CROUCH),
                 tool = Mouse.isButtonDown(0),
                 use = Mouse.isButtonDown(1))
        }
    }
}
