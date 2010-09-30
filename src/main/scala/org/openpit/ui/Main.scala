package org.openpit.ui

import org.lwjgl.Sys
import org.lwjgl.opengl.Display
import org.lwjgl.input.{Keyboard, Mouse}

import org.openpit.ui.console.FPS
import org.openpit.world.World

object Main {
    val unitsPerSecond = 18.0
    lazy val ticksPerSecond : Double = Sys.getTimerResolution()

    var finished = false
    var lastTime = 0.0

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

        lastTime = getCurrentTime()
    }

    def getCurrentTime() = {
        Sys.getTime() / ticksPerSecond
    }
 
    def cleanup() {
        Input.cleanup()
    }

    def handleInput() {
        var currentTime = getCurrentTime()
        var elapsedTime = currentTime - lastTime
        lastTime = currentTime

        var movement = unitsPerSecond * elapsedTime
        var movementFloat = movement.toFloat

        import Input._
        input() match {
            case Quit => finished = true
            case Inventory => Unit
            case Menu => Unit
            case m: Move =>
                Camera.update(m.yaw, m.pitch)
                Camera.strafe(m.dx * movementFloat)
                Camera.walk(m.dy * movementFloat)
        }
    }
}
