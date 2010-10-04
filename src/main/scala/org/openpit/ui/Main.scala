package org.openpit.ui

import org.lwjgl.Sys
import org.lwjgl.opengl.Display
import org.lwjgl.input.{Keyboard, Mouse}

import org.openpit.ui.console.FPS
import org.openpit.world.World
import simplex3d.math.floatm.Vec3f

object Main {
    val unitsPerSecond = 10.0  // XXX what speed
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
                Camera.update(m.yaw, m.pitch, elapsedTime.toFloat)

                Camera.strafe(m.dx * movementFloat)
                Camera.walk(m.dy * movementFloat)
                Camera.climb(m.dz * movementFloat)
                if (m.jump) {
                    Camera.beginJump
                }

                // Update selection point
                val reach    = 5f
                val minreach = 0.3f
                var hit = World.raycast(Camera.eye, Camera.direction, reach)

                // Filter out hits too near
                hit = hit match {
                    case World.Hit(loc, dist, _) =>
                        if (dist < minreach) World.Miss else hit
                    case World.Miss => World.Miss
                }

                hit match {
                    case World.Hit(loc, dist, axis) =>
                        import org.openpit.util.Axes._
                        import simplex3d.math.floatm.FloatMath._
                        import simplex3d.math.intm._
                        import math.signum
                        SelectLayer.selected = Some(loc)
                        SelectLayer.distance = dist
                        val displacement = axis match {
                            case XX => Vec3i(1,0,0)*signum(Camera.eye.x - loc.x).toInt
                            case YY => Vec3i(0,1,0)*signum(Camera.eye.y - loc.y).toInt
                            case ZZ => Vec3i(0,0,1)*signum(Camera.eye.z - loc.z).toInt
                        }
                        SelectLayer.target = Some(loc + displacement)
                    case World.Miss =>
                        SelectLayer.selected = None
                        SelectLayer.target = None
                        SelectLayer.distance = Float.MaxValue
                }

                {
                    import org.openpit.world.blocks._
                    import org.openpit.util._
                    if (m.use) SelectLayer.target.foreach {
                        case loc =>
                            if (!Camera.collisionBox.intersects(AABB.fromBlock(loc))) {
                                World(loc) = Cobblestone()
                                // XXX use position interface
                                SoundEffect.KungFuPunch.playAsSoundEffect(1f, 1f, false)
                            }
                    }
                    if (m.tool) SelectLayer.selected.foreach {
                        case loc =>
                            World.get(loc).filter(_ != Air).foreach {
                                case _ =>
                                SoundEffect.Digging.playAsSoundEffect(1f, 1f, false)
                            }
                            World(loc) = Air
                    }
                    if (m.use || m.tool)
                        Window.update()
                }
        }
    }
}
