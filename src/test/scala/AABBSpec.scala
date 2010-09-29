package org.openpit.test

import org.openpit.util.AABB

import simplex3d.math.floatm._
import simplex3d.math.floatm.FloatMath._

import org.scalacheck._

object AABBSpec extends Properties("AABB") {
    import Prop.forAll
    import Generators._

    property("normalized") = forAll { (a: Vec3f, b: Vec3f) =>
        val aabb = new AABB(a, b)
        aabb.min.x <= aabb.max.x &&
        aabb.min.y <= aabb.max.y &&
        aabb.min.z <= aabb.max.z
    }

    property("contains self") = forAll { (a: Vec3f, b: Vec3f) =>
        val aabb = new AABB(a, b)
        (aabb contains a) && (aabb contains b) && (aabb contains aabb)
    }
}
