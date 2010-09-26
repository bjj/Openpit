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
	    input()
	    Window.paint()
	}
	cleanup()
	exit(0)
    }

    def init() {
	Window.init()
	World.init()
	Window.update() // XXX should be Layers.update() or something

	Keyboard.create()
	Mouse.create()
	Mouse.setGrabbed(true)

	FPS.start()
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

	import org.openpit.ui.Camera
	Camera.update(-Mouse.getDX * 0.3f, Mouse.getDY * 0.3f)

	if (Keyboard.isKeyDown(Keyboard.KEY_A)) Camera.strafe(-0.3)
	if (Keyboard.isKeyDown(Keyboard.KEY_S)) Camera.walk(-0.3)
	if (Keyboard.isKeyDown(Keyboard.KEY_D)) Camera.strafe(0.3)
	if (Keyboard.isKeyDown(Keyboard.KEY_W)) Camera.walk(0.3)

	if (Keyboard.isKeyDown(Keyboard.KEY_Q)) Camera.update(10, 0)
	if (Keyboard.isKeyDown(Keyboard.KEY_E)) Camera.update(-10, 0)
    }
}
