package org.openpit.ui

import org.lwjgl.Sys
import org.lwjgl.opengl.Display
import org.lwjgl.input.{Keyboard, Mouse}

import org.openpit.ui.console.FPS
import org.openpit.world.World

object Main {
    val mouseSensitivity = 3f
    val unitsPerSecond = 18.0
    lazy val ticksPerSecond : Double = Sys.getTimerResolution()

    var finished = false
    var lastTime = 0.0

    def main(args: Array[String]) {

        init()
        while (!finished) {
            input()
            Window.paint()
        }
        cleanup()
        exit(0)
    }

    def init() {
        Window.init()
        World.generate()
        Window.update() // XXX should be Layers.update() or something

        Keyboard.create()
        Mouse.create()
        Mouse.setGrabbed(true)

        FPS.start()

        lastTime = getCurrentTime()
    }

    def getCurrentTime() = {
        Sys.getTime() / ticksPerSecond
    }
 
    def cleanup() {
        Mouse.setGrabbed(false)
    }

    def input() {
        if (Keyboard.isKeyDown(Keyboard.KEY_ESCAPE)) {
            finished = true
            println("esc hit")
        } else if (Display.isCloseRequested()) {
            finished = true
            println("exit clicked")
        }

        var currentTime = getCurrentTime()
        var elapsedTime = currentTime - lastTime
        lastTime = currentTime

        var movement = unitsPerSecond * elapsedTime
        var movementFloat = movement.toFloat

        import org.openpit.ui.Camera
        Camera.update(-Mouse.getDX * movementFloat * mouseSensitivity,
                       Mouse.getDY * movementFloat * mouseSensitivity)

        if (Keyboard.isKeyDown(Keyboard.KEY_A)) Camera.strafe(-movement)
        if (Keyboard.isKeyDown(Keyboard.KEY_S)) Camera.walk(-movement)
        if (Keyboard.isKeyDown(Keyboard.KEY_D)) Camera.strafe(movement)
        if (Keyboard.isKeyDown(Keyboard.KEY_W)) Camera.walk(movement)

        if (Keyboard.isKeyDown(Keyboard.KEY_Q)) Camera.update(movementFloat * mouseSensitivity, 0)
        if (Keyboard.isKeyDown(Keyboard.KEY_E)) Camera.update(-movementFloat * mouseSensitivity, 0)
    }
}
