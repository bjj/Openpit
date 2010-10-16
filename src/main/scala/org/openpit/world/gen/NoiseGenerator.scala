package org.openpit.world.gen

import org.openpit.world._
import simplex3d.math.intm._
import simplex3d.math.floatm._
import simplex3d.math.floatm.FloatMath._
import org.openpit.world.blocks._
import org.openpit.util._
import util.Random

object NoiseGenerator {
  var height = 100
  var noise3scale = 20000.0f

  val noise2 = new PerlinNoise2D()
//  val noise3 = new PerlinNoise3D()

  def init() {
    noise2.init()
//    noise3.init()
  }

  def generate(invert : Boolean) {
      this(20000, Vec2i(-100, -100), Vec2i(100, 100), invert)
  }

  /**
   * Render a region of a flat, noisy world in the 2D box (from, to).
   * World is "defined" in the box (0 - worldsize, 0 - worldsize, -100) to (worldsize, worldsize, 100)
   */
    def apply(worldsize : Int, from : Vec2i, to : Vec2i, invert : Boolean) {

      // got OOM errors when I tried to render the world in multiple passes into the octtree
      var counter = 0
      val sealevel = -height + (height / 8)
      for(x <- from.x to to.x; y <- from.y to to.y) {
          val nv = ConstVec2f(((x + worldsize) / (worldsize*2.0f)), ((y + worldsize) / (worldsize*2.0f)))
          val n = noise2(nv.x, nv.y)
          val h = (floor(2f*height * n + 10) - height).toInt
          //CellArray(x, y, -100) = 1
          World(x, y, -height) = Water
          for(z <- -height to sealevel) 
            World(x, y, z) = Water
          for(z <- -height to h) {
            val nr = ConstVec3f(nv.x, nv.y, ((z + height) / 2f*height))
//            val nz = noise3(nr.x, nr.y, nr.z)
//            if(invert) {
//                 if(nz > 0.55f) {
//                   World(x, y, z) = Noise(if (nz < 0.0f) ConstVec3f(-nz, 0.0f, 0.0f) else ConstVec3f(0.0f, nz, 0.0f))
//                 }
//            } else {
//                if(nz < 0.55f) {
                  counter += 1
                  World(x, y, z) = if (z > (h-3) && (h > sealevel+2)) Grass else (if (h <= (sealevel+2)) Sand else Stone)
//                }
//            }
          }
      }
      System.out.println("count = " + counter)
    }
}


object MultipassGenerator {
  var height = 100
  val heightnoise = new PerlinNoise2D()

  val worldSize = ConstVec3i(160934, 160934, 1000) // 100 miles x 100 miles x 1km
  val islandRadius = 160934 / 2                    // 50 miles 
  val random = new Random()

  def init() {
    heightnoise.init()
    random.setSeed(35345345452L)

  }

  def generate(invert : Boolean) {
      this(20000, Vec2i(-100, -100), Vec2i(100, 100), invert)
  }

  /**
   * Render a region of a flat, noisy world in the 2D box (from, to).
   * World is "defined" in the box (0 - worldsize, 0 - worldsize, -100) to (worldsize, worldsize, 100)
   */
    def apply(worldsize : Int, from : Vec2i, to : Vec2i, invert : Boolean) {

      // got OOM errors when I tried to render the world in multiple passes into the octtree
      var counter = 0
      val sealevel = -height + (height / 8)
      val grassline = height - (height / 20)
      val world = new Dense3DArray(ConstVec3i(from.x-2, from.y-2, -height-2), ConstVec3i(to.x - from.x + 5, to.y - from.y + 5, 2*height + 5))
      var heightmap = new Dense3DArray(ConstVec3i(from.x, from.y, 0), ConstVec3i(to.x - from.x + 1, to.y - from.y + 1, 1))
      val cavemap = new Dense3DArray(ConstVec3i(from.x, from.y, 0), ConstVec3i(to.x - from.x + 1, to.y - from.y + 1, 2))
      val width = to.x - from.x
      val depth = to.y - from.y
      for(x <- from.x to to.x; y <- from.y to to.y) {
          val nv = ConstVec2f(((x + worldsize) / (worldsize*2.0f)), ((y + worldsize) / (worldsize*2.0f)))
          val n = heightnoise(nv.x, nv.y)
          val h = max(-height, (floor(2f*height * n + 10) - height)).toInt

          //CellArray(x, y, -100) = 1
          world(x, y, -height) = 2
          heightmap(x, y, 0) = h.toByte
          for(z <- -height to sealevel) {
            world(x, y, z) = 1
          }
          for(z <- -height to h) {
            world(x, y, z) = 3
          }
          for(z <- sealevel+2 to h) {
            world(x, y, z) = 4
          }
          for(z <- sealevel+2 to h-4) {
            world(x, y, z) = 2
          }
          for(z <- grassline to h) {
            world(x, y, z) = 2
          }
      }

       def caveBlaster() = {
         def blast(point : ConstVec3i) = {
            for(x <- point.x to (point.x + 2); y <- point.y to (point.y + 2); z <- point.z to (point.z + 2)) {
              world(x, y, z) = 0
            }
         }

         def randomCaveDig(start : ConstVec3i, power : Int) : Int = {
            val nextpower = (random.nextInt(4) - 3) + power
            if(!(start.x > to.x || start.y > to.y || start.z > height || start.x < from.x || start.y < from.y || start.z < -height)) {
              if(nextpower >= 0) {
                 blast(start)
                 val direction = ConstVec3i(random.nextInt(2) - 1, random.nextInt(2) - 1, random.nextInt(3) - 2)
                 randomCaveDig(start + direction, nextpower)
              }
            }
           0
         }

        // TODO: flood-fill grass and dirt so placement can be more coherent? fill in sand in pass 2 -- underwater land and land within 2 height and 2 distance of water
         for(i <- 0 to 500) {
           val x = random.nextInt(width - 5) + from.x + 2
           val y = random.nextInt(depth - 5) + from.y + 2
           val h = heightmap(x, y, 0)
           if(h > (sealevel + 8)) {
              randomCaveDig(ConstVec3i(x, y, h), 100)
           }
         }
       }

       def caveCells(targetheight : Int) = {
          // generate 2000 random underground points
         cavemap() = 1
         var c = 1
         var p = 0
         for(x <- (from.x+2) to (to.x-2); y <- (from.y+2) to (to.y-2)) {
           if(random.nextFloat() < 0.40f) {
             cavemap(x, y, 0) = 0
             cavemap(x, y, 1) = 0
           }
         }

         def countAdj(x : Int, y : Int) : Int = {
             (  cavemap(x-1,   y+1, p)
             + cavemap(x,     y+1, p)
             + cavemap(x+1,   y+1, p)
             + cavemap(x-1,   y,   p)
             + cavemap(x+1,   y,   p)
             + cavemap(x-1,   y-1, p)
             + cavemap(x,     y-1, p)
             + cavemap(x+1,   y-1, p))
         }

         def max(x : Int, y : Int) = {
            if(x > y) x else y
         }

         for(i <- 0 to 15) {
           val t = p; p = c; c = t
           for(x <- (from.x+2) to (to.x-2); y <- (from.y+2) to (to.y-2)) {
               val adj = countAdj(x, y)
               cavemap(x, y, c) = if(adj < 4) 0 else if(adj > 5) 1 else cavemap(x, y, p)
           }
         }

//  Render into the world as a plane (just to see what is going on)
//         for(x <- (from.x+2) to (to.x-2); y <- (from.y+2) to (to.y-2)) {
//             cavemap(x, y, c) match {
//                case 1 => World(x, y, -height-1) = Stone
//                case 0 => Unit // World(x, y, -height-1) = Air
//             }
//         }

         // TODO: merge some (or all) of the disjoint cave cells together with passages

         // now render our 2D cave system into the 3D world
         // for now, just render into a slice just above sea level, 3 squares high
         // eventually, generate another noise heightmap (or a pair of them) to decide floor and ceiling of caves
         // or figure a way to merge several levels of caves
         for(x <- (from.x+2) to (to.x-2); y <- (from.y+2) to (to.y-2)) {
             if(cavemap(x, y, c) == 0) {
               world(x, y, targetheight) = 0
               world(x, y, targetheight+1) = 0
               world(x, y, targetheight+2) = 0
             }
         }
       }

      caveCells(sealevel-10)
      caveCells(sealevel+5)
      caveCells(sealevel+15)
      caveCells(sealevel+25)
      caveCells(sealevel+40)

      

       // TODO: fix the water
        println("Generating " + from + " to " + to + " -100 to 100")
        if(invert) {
          println("inverting")
          var loc = Vec3i(0,0,0)
          for(x <- from.x to to.x; y <- from.y to to.y; z <- -100 to 100) {
              loc.x = x; loc.y = y; loc.z = z
              val h = heightmap(x, y, 0)
              if(z <= h) {
                world(x, y, z) match {
                  case 0 => World(loc) = Stone
                  case _ => Unit
                }
              }
          }
        } else {
          var loc = Vec3i(0,0,0)
          for(x <- from.x to to.x; y <- from.y to to.y; z <- -100 to 100) {
              loc.x = x; loc.y = y; loc.z = z
              world(x, y, z) match {
                case 1 => World(loc) = Water
                case 2 => World(loc) = Stone
                case 3 => World(loc) = Sand
                case 4 => World(loc) = Grass
                case _ => Unit
              }
          }
        }
        println("done!")
    }



}
