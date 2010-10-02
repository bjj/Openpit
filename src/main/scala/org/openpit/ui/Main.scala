package org.openpit.ui

import org.lwjgl.Sys
import org.lwjgl.opengl.Display
import org.lwjgl.input.{Keyboard, Mouse}

import org.openpit.ui.console.FPS
import org.openpit.world.World
import simplex3d.math.floatm.Vec3f

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
                Camera.climb(m.dz * movementFloat)
                if(m.jump) {
                   Camera.fly(movementFloat)
                }
                {
                    import simplex3d.math.intm._
                    import simplex3d.math.floatm.FloatMath._
                    import org.openpit.util.AABB
                    import org.openpit.world.blocks._
                    val eye = Camera.eye
                    val dir = Camera.direction
                    val reach = 5f
                    val bound = AABB.fromRay(eye, dir, reach).rounded

                    var bestdist = Float.MaxValue
                    var bestloc: Option[Vec3i] = None
                    World.foreach(bound) {
                        case (loc, Air) => Unit
                        case (loc, b) =>
                            val blockvolume = AABB.fromBlock(loc)
                            blockvolume.raycast(eye, dir, reach) match {
                                case Some(dist) => if (dist < bestdist) {
                                    bestdist = dist
                                    bestloc = Some(loc)
                                }
                                case None => Unit
                            }
                    }

                    if (bestdist < 0.3f) bestloc = None // Too close, match player radius?
                    SelectLayer.selected = bestloc
                    SelectLayer.distance = bestdist

                    bestloc match {
                        case None =>
                            SelectLayer.target = None
                        case _ =>
                            val hitpoint = eye + dir * (bestdist * 0.99f)
                            SelectLayer.target = Some(Vec3i(floor(hitpoint)))
                    }

                    if (m.use) SelectLayer.target.foreach {
                        case loc =>
                            if (!Camera.collisionBox.intersects(AABB.fromBlock(loc)))
                                World(loc) = Cobblestone()
                    }
                    if (m.tool) SelectLayer.selected.foreach(World(_) = Air)
                    if (m.use || m.tool)
                        Window.update()
                }
        }
    }
}
