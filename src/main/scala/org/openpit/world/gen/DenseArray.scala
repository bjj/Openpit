package org.openpit.world.gen

import org.openpit.world._
import simplex3d.math.intm._
import simplex3d.math.floatm._
import simplex3d.math.floatm.FloatMath._
import org.openpit.world.blocks._
import org.openpit.util._


class Dense3DArray(origin : ConstVec3i, size : ConstVec3i) {
  val tworld = new Array[Byte](size.x * size.y * size.z)
  final def index(x : Int, y : Int, z : Int) = {
    val ox = x - origin.x
    val oy = y - origin.y
    val oz = z - origin.z
    oz * size.x * size.y + oy * size.x + ox
  }
  final def apply(x : Int, y : Int, z : Int) = {
    tworld(index(x, y, z))
  }
  final def update(x : Int, y : Int, z : Int, v : Byte) = {
    tworld(index(x, y, z)) = v
  }
}
