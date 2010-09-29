package org.openpit.ui

import org.lwjgl.opengl.Display
import org.lwjgl.input.{Keyboard, Mouse}

import org.openpit.ui.console.FPS
import org.openpit.world.World

object Main {
    var finished = false

    def main(args: Array[String]) {

        init()
        while (!finished) {
            handleInput()
            Window.paint()
        }
        cleanup()
        exit(0)
    }

    def init() {
        Window.init()
        World.generate()
        Window.update() // XXX should be Layers.update() or something
        Input.init()
        FPS.start()
    }

    def cleanup() {
        Input.cleanup()
    }

    def handleInput() {
        import Input._
        input() match {
            case Quit => finished = true
            case Inventory => Unit
            case Menu => Unit
            case m: Move =>
                Camera.update(m.yaw, m.pitch)
                Camera.strafe(m.dx * 0.3f)
                Camera.walk(m.dy * 0.3f)
        }
    }
}
