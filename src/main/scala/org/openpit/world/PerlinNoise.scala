// based on http://mrl.nyu.edu/~perlin/noise/ and http://planetgenesis.cvs.sourceforge.net/viewvc/planetgenesis/planetGenesis3/net/vargolsoft/planetGenesis/plugins/noise/   
package org.openpit.world

import simplex3d.math.intm._
import simplex3d.math.floatm._
import simplex3d.math.floatm.FloatMath._
import org.openpit.world.blocks._
import org.openpit.util._
import util.Random

class PerlinNoise2D {

  val SIZE = 256
  val perlin = new Array[Int](SIZE);
  val gradient = new Array[Vec2f](SIZE);

  def init() = {
     val random = new Random()
     random.setSeed(6121874123L) // so we get the same noise function each time while debugging scaling
     for(i <- 0 to 255)
        perlin(i) = i
     for(i <- 0 to 255) {
       val tmp = perlin(i)
       val j = random.nextInt(SIZE)
       perlin(i) = perlin(j)
       perlin(j) = tmp
     }

    for(i <- 0 to 255) {
      gradient(i) = normalize(Vec2f(random.nextFloat() * 2 - 1, random.nextFloat() * 2 - 1))
    }
  }

  def noise(x : Float, y : Float) = {
     val grid1 = Vec2i(floor(x).toInt, floor(y).toInt)
     val grid2 = Vec2i(grid1.x + 1, grid1.y + 1)

     val r1 = Vec2f(x - grid1.x, y - grid1.y)
     val r2 = Vec2f(r1.x - 1, r1.y - 1)

     val ogrid1 = Vec2i(grid1.x & 0xff, grid1.y & 0xff)
     val ogrid2 = Vec2i(grid2.x & 0xff, grid2.y & 0xff)

     val g1 = perlin((ogrid1.y + perlin(ogrid1.x)) & 0xff)
     val g2 = perlin((ogrid1.y + perlin(ogrid2.x)) & 0xff)
     val g3 = perlin((ogrid2.y + perlin(ogrid1.x)) & 0xff)
     val g4 = perlin((ogrid2.y + perlin(ogrid2.x)) & 0xff)

     val i1 = dot(gradient(g1), r1)
     val i2 = dot(gradient(g2), Vec2f(r2.x, r1.y))
     val i3 = dot(gradient(g3), Vec2f(r1.x, r2.y))
     val i4 = dot(gradient(g4), r2)

     val wx = (3 - (2 * r1.x)) * r1.x * r1.x
     val wy = (3 - (2 * r1.y)) * r1.y * r1.y
     val ix1 = i1 - wx * (i1 - i2)
     val ix2 = i3 - wx * (i3 - i4)

     ix1 - wy * (ix1 - ix2)
  }

  def apply(x : Float, y : Float) = {
     var total = 0.0f
     val p = 0.30f
     val zx = x * 100.0f
     val zy = y * 200.0f
     for(i <- 0 to 9) {
          val freq = pow(2, i)
          val amp = pow(p, i)
          total += noise(zx * freq, zy *freq) * amp
     }
     total
  }
  

}

final class PerlinNoise3D {
  val SIZE = 256
  val perlin = new Array[Int](SIZE * 2 + 2);
  val gradient = new Array[Vec2f](SIZE);
  val rotate = ConstQuat4f(quaternion(radians(30.0f), Vec3f.UnitX) * quaternion(radians(30.0f), Vec3f.UnitY) * quaternion(radians(30.0f), Vec3f.UnitZ))

  def lerp(w : Float, a : Float, b : Float) = {
     a + w * (b - a)
  }

  def fade(o : Float) = {
    o * o * o * (o * (o * 6 - 15) + 10)
  }

  def grad(hash : Int, x : Float, y : Float, z : Float) = {
     val h = hash & 15
     val u = if (h < 8) x else y
     val v = if (h < 4 || h == 12 || h == 13) y else z
     (if ((h & 1) == 0) u else -u) + (if ((h & 2) == 0) v else -v)
  }

  def init() = {
     val random = new Random()
     random.setSeed(6121874123L) // so we get the same noise function each time while debugging scaling
     for(i <- 0 to 255)
        perlin(i) = i
     for(i <- 0 to 255) {
       val tmp = perlin(i)
       val j = random.nextInt(SIZE)
       perlin(i) = perlin(j)
       perlin(j) = tmp
     }
     for(i <- 0 to 255) {
       perlin(i + SIZE) = perlin(i)
     }
  }

  def noise(x : Float, y : Float, z : Float) = {
     val point = Vec3i(floor(x).toInt & 0xff, floor(y).toInt & 0xff, floor(z).toInt & 0xff)

     val rp = Vec3f(x - floor(x), y - floor(y), z - floor(z))
     val u = fade(rp.x)
     val v = fade(rp.y)
     val w = fade(rp.z)

     val A = perlin(point.x) + point.y
     val AA = perlin(A) + point.z
     val AB = perlin(A + 1) + point.z
     val B = perlin(point.x + 1) + point.y
     val BA = perlin(B) + point.z
     val BB = perlin(B + 1) + point.z


    lerp(w,
          lerp(v,
            lerp(u, grad(perlin(AA), rp.x, rp.y, rp.z),
              grad(perlin(BA), rp.x - 1, rp.y, rp.z)),
            lerp(u, grad(perlin(AB), rp.x, rp.y - 1, rp.z),
              grad(perlin(BB), rp.x - 1, rp.y - 1, rp.z))),
          lerp(v, lerp(u, grad(perlin(AA + 1), rp.x, rp.y, rp.z - 1),
            grad(perlin(BA + 1), rp.x - 1, rp.y, rp.z - 1)),
          lerp(
            u,
            grad(perlin(AB + 1), rp.x, rp.y - 1, rp.z - 1),
            grad(perlin(BB + 1), rp.x - 1, rp.y - 1, rp.z - 1))))
  }

  def apply(x : Float, y : Float, z : Float) = {
     var total = 0.0f
     val p = 0.50f
     val zx = x * 6000.0f
     val zy = y * 6000.0f
     val zz = z * 6000.0f
     val zot = rotate.rotateVector(ConstVec3f(zx, zy, zz))
     for(i <- 0 to 9) {
          val freq = pow(2, i)
          val amp = pow(p, i)
          total += noise(zot.x * freq, zot.y *freq, zot.z*freq/15.0f) * amp
     }
     total
  }

}
