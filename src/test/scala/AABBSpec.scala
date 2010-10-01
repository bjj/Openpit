package org.openpit.test

import org.openpit.util.AABB

import simplex3d.math.floatm._
import simplex3d.math.floatm.FloatMath._

import org.scalacheck._

object AABBSpec extends Properties("AABB") {
    import Prop._
    import Generators._

    property("normalized") = forAll { (a: Vec3f, b: Vec3f) =>
        val aabb = new AABB(a, b)
        aabb.min.x <= aabb.max.x &&
        aabb.min.y <= aabb.max.y &&
        aabb.min.z <= aabb.max.z
    }

    property("contains self") = forAll { (a: Vec3f, b: Vec3f) =>
        val aabb = new AABB(a, b)
        (aabb contains a) && (aabb contains b) &&
        (aabb contains aabb) && (aabb intersects aabb)
    }

    property("contains center") = forAll { (a: Vec3f, b: Vec3f) =>
        val aabb = new AABB(a, b)
        val center = (a + b) / 2
        (aabb contains center)
    }

    property("raycast to center") = forAll { (a: Vec3f, b: Vec3f) =>
        val aabb = new AABB(a, b)
        val center = (a + b) / 2
        aabb.raycast(ConstVec3f(0,0,0), center, 1.0f).get < 1.0f
    }

    property("raycast through center") = forAll { (a: Vec3f, b: Vec3f) =>
        val aabb = new AABB(a, b)
        val beyond = (a + b)
        aabb.raycast(ConstVec3f(0,0,0), beyond, 1.0f).get < 0.5f
    }

    property("raycast behind") = forAll { (a: Vec3f, b: Vec3f) =>
        distance(a, b) > 0.0f ==> {
            val aabb = new AABB(a, b)
            val d = normalize(aabb.max - aabb.min)
            val outside = aabb.max + d
            aabb.raycast(outside, d, Float.MaxValue) == None
    }}

    property("raycast parallel") = forAll { (a: Vec3f, b: Vec3f) =>
        distance(a, b) > 0.0f ==> {
            val aabb = new AABB(a, b)
            var d = normalize(aabb.max - aabb.min)
            val outside = aabb.min - d
            d(Gen.choose(0,2).sample.get) = 0  // make one axis parallel
            aabb.raycast(outside, d, Float.MaxValue) == None
    }}

    property("raycast distance") = forAll { (a: Vec3f, b: Vec3f) =>
        val axis = Gen.choose(0,2).sample.get
        (a(axis) != b(axis) && distance(a, b) > 0.0f) ==> {
            val aabb = new AABB(a, b)
            val center = (a + b) / 2
            val edge = Vec3f(center); edge(axis) = a(axis)
            var d = normalize(edge - center)
            edge += d
            ("axis = " + axis + " edge = " + edge + " d = " + d) |:
            approxEqual(aabb.raycast(edge, -d, 2.0f).
                             getOrElse(0f), 1.0f, 0.0001f)
        }
    }

    property("rounded contains self") = forAll { (a: Vec3f, b: Vec3f) =>
        val aabb = new AABB(a, b)
        val round = aabb.rounded
        (round contains aabb) &&
        (round intersects aabb) &&
        (round == aabb || !(aabb contains round))
    }
}
