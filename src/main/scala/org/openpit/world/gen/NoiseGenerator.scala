package org.openpit.world.gen

import org.openpit.world._
import simplex3d.math.intm._
import simplex3d.math.floatm._
import simplex3d.math.floatm.FloatMath._
import org.openpit.world.blocks._
import org.openpit.util._

object NoiseGenerator {
  var height = 100
  var noise3scale = 3900.0f
  var noise3rotate = ConstQuat4f(quaternion(radians(30.0f), Vec3f.UnitX) * quaternion(radians(30.0f), Vec3f.UnitY) * quaternion(radians(30.0f), Vec3f.UnitZ))

  val noise2 = new PerlinNoise2D()
  val noise3 = new PerlinNoise3D()

  def init() {
    noise2.init()
    noise3.init()    
  }

  def generate() {
      noisy(20000, Vec2i(-100, -100), Vec2i(100, 100))
  }

  /**
   * Render a region of a flat, noisy world in the 2D box (from, to).
   * World is "defined" in the box (0 - worldsize, 0 - worldsize, -100) to (worldsize, worldsize, 100)
   */
    def noisy(worldsize : Int, from : Vec2i, to : Vec2i) {
  //      object CellArray {
  //        val length = to.x - from.x
  //        val width = to.y - from.y
  //        val height = 100 - -100
  //        val size = length * width * height
  //        val tworld = new Array[Byte](size)
  //        def index(x : Int, y : Int, z : Int) = {
  //          val ox = x - from.x
  //          val oy = y - from.y
  //          val oz = z - -100
  //          ox * width * length + oy * width + oz
  //        }
  //        def apply(x : Int, y : Int, z : Int) = {
  //          tworld(index(x, y, z))
  //        }
  //        def update(x : Int, y : Int, z : Int, v : Byte) = {
  //          tworld(index(x, y, z)) = v
  //        }
  //      }

      // got OOM errors when I tried to render the world in multiple passes into the octtree
      var counter = 0
      val sealevel = -height + (height / 8)
      for(x <- from.x to to.x; y <- from.y to to.y) {
          val nv = ConstVec2f(((x + worldsize) / (worldsize*2.0f)), ((y + worldsize) / (worldsize*2.0f)))
          val n = noise2(nv.x, nv.y)
          val h = (floor(2f*height * n + 10) - height).toInt
          //CellArray(x, y, -100) = 1
          World(x, y, -height) = Water()
          for(z <- -height to sealevel) 
            World(x, y, z) = Water()
          for(z <- -height to h) {
            val nr = ConstVec3f(nv.x, nv.y, ((z + height) / 2f*height))
            val nr2 = nr * noise3scale
            val rnr = noise3rotate.rotateVector(nr2)
            val nz = noise3(rnr.x, rnr.y, rnr.z / 10.0f)
            if(nz > -0.2f) {
              counter += 1
              World(x, y, z) = if (z > (h-3) && (h > sealevel+2)) Grass() else (if (h <= (sealevel+2)) Sand() else Stone())
            }
          }
      }
      System.out.println("count = " + counter)

  //      System.out.println("Generating " + from + " to " + to + " -100 to 100")
  //      for(x <- from.x to to.x; y <- from.y to to.y; z <- -100 to 100) {
  //          CellArray(x, y, z) match {
  //            case 0 => Unit // Air
  //            case 1 => this(x, y, z) = Stone()
  //            case 2 => this(x, y, z) = Grass()
  //          }
  //      }
    }

}

