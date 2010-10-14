package org.openpit.ui

import org.lwjgl.Sys
import org.lwjgl.opengl.Display
import org.lwjgl.input.{Keyboard, Mouse}

import org.openpit.ui.console.FPS
import org.openpit.world.World
import simplex3d.math.floatm.Vec3f
import org.openpit.world.gen.{MultipassGenerator, NoiseGenerator}

object Main {
    val unitsPerSecond = 10.0  // XXX what speed
    lazy val secondsPerTick : Float = 1.0f / Sys.getTimerResolution().toFloat

    var finished = false

    def main(args: Array[String]) {

        init()
        while (!finished) {
            elapsedTime() match {
            case 0.0f => Unit
            case dt   => handleInput(dt)
            }
            Window.paint()
        }
        cleanup()
        exit(0)
    }

    def init() {
        Window.init()
        //World.generate()
        NoiseGenerator.init()
        MultipassGenerator.init()
        //NoiseGenerator.generate()
        World.generate()
        Layer.init()
        Input.init()
        FPS.start()
    }

    def cleanup() {
        Input.cleanup()
    }

    var lastTicks = Sys.getTime()
    def elapsedTime() = {
        val nowTicks = Sys.getTime()
        val delta = nowTicks - lastTicks
        val elapsed = delta * secondsPerTick
        if (elapsed >= 0.01f) {
            lastTicks = nowTicks
            elapsed
        } else
            0f
    }

    def handleInput(elapsedTime: Float) {
        var movement = unitsPerSecond * elapsedTime
        var movementFloat = movement.toFloat

        import Input._
        input() match {
            case Quit => finished = true
            case Inventory => Unit
            case Menu => Unit
            case m : WorldGen => {
               if(m.generate || m.invert) {
                 // System.out.println("scale = " + m.scale + "; " + NoiseGenerator.noise3scale)
                 //Camera.loc = Vec3f(20.5f, 20.5f, 12.5f)
                 World.clear()
                 //World.generate()
                 //NoiseGenerator.generate(m.invert)
                 MultipassGenerator.generate(m.invert)
                 Layer.update()
               }
            }
           case m: Move =>
                Camera.update(elapsedTime.toFloat, Some(m))

                Camera.climb(m.walk.z * movementFloat)

                // Update selection point
                val reach = 5f
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
                                Layer.update(AABB.fromBlock(loc))
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
                            Layer.update(AABB.fromBlock(loc))
                    }
                }
        }
    }
}
