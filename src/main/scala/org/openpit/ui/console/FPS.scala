package org.openpit.ui.console

import scala.actors.Actor
import java.lang.System.currentTimeMillis

object FPS extends Actor {
    abstract class FpsMessage
    case object Frame    extends FpsMessage
    case object Shutdown extends FpsMessage

    val interval = 1000

    def act() {
	var running = true
	var end = currentTimeMillis + interval
	var frames = 0

	while (running) {
	    val now = currentTimeMillis
	    if (now >= end) {
		if (frames > 0) printf("%d FPS\n", frames * 1000 / interval)
		frames = 0
		end = now + interval
	    }
	    receiveWithin(end - now) {
		case Frame =>
		    frames += 1
		case Shutdown =>
		    running = false
	    }
	}
    }
}
