package org.openpit.test

import org.openpit.util._

import simplex3d.math.intm._
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

    property("raycast perpendicular") = forAll { (a: Vec3f, b: Vec3f) =>
        distance(a, b) > 0.0f ==> {
            val axis = Gen.choose(0,2).sample.get
            val aabb = new AABB(a, b)
            var byface = aabb.center
            byface(axis) = a(axis) // move to face
            val d = normalize(byface - aabb.center)
            byface += d
            val res = aabb.raycast(byface, -d, Float.MaxValue)
            ("distance = " + res) |: approxEqual(res.map(_.distance).get, 1f, 0.0001f)
    }}

    property("raycast from face") = forAll { (a: Vec3f, b: Vec3f, v: Vec3f) =>
        val axis = Gen.choose(0,2).sample.get
        (a(axis) != b(axis) && distance(a, b) > 0.0f) ==> {
            val aabb = new AABB(a, b)
            var facepoint = aabb.center
            facepoint(axis) = a(axis)
            val res = aabb.raycast(facepoint, v, Float.MaxValue)
            ("distance = " + res) |: res.map(_.distance).get == 0f
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
            approxEqual(aabb.raycast(edge, -d, 2.0f).map(_.distance).
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

    property("sweep unit blocks") = forAll { (a: Vec3i, b: Vec3i) =>
        distance(a, b) > 2.0 ==> {
            val aa = AABB.fromBlock(a)
            val bb = AABB.fromBlock(b)
            val vec = aa.center - bb.center
            val eps = (2.0 / length(vec)).toFloat
            (aa.sweep(bb, -vec) == None) :| "moving away" &&
            (aa.sweep(bb, vec) match {
                case Some(AxialDistance(_, d)) => approxEqual(d, 1.0f, eps)
                case None => false
            })                           :| "moving toward"
        }
    }

    property("escapes self") = forAll { (a: Vec3i, b: Vec3i) =>
        val aabb = new AABB(a, b)
        val escape = aabb.escape(aabb)
        ("escape vec " + escape) |: !(aabb intersects (aabb + escape))
    }

    property("escapes any") = forAll { (a: Vec3i, b: Vec3i, c: Vec3i, d: Vec3i) =>
        val aa = new AABB(a, b)
        val bb = new AABB(c, d)
        val escape = aa.escape(bb)
        ("escape vec " + escape) |: !(aa intersects (bb + escape))
    }

    val escapeMinThresh = 1.0f - normalize(Vec3f.One).x * 0.999f + 0.01f
    property("escapes minimum") = forAll { (a: Vec3i) =>
        val push = normalize(a) * 0.999f
        val aa = new AABB(Vec3f.Zero, Vec3f.One)
        val bb = aa + push
        val escape = aa.escape(bb)
        (("escape works " + escape) |: !(aa intersects (bb + escape))) &&
        (("escape short " + length(escape)) |: length(escape) <= escapeMinThresh)
    }

    property("escapes tiny") = forAll { (a: Int) =>
        val face = abs(a) % 6
        var push = face match {
            case 0 => -Vec3f.UnitX
            case 1 =>  Vec3f.UnitX
            case 2 => -Vec3f.UnitY
            case 3 =>  Vec3f.UnitY
            case 4 => -Vec3f.UnitZ
            case 5 =>  Vec3f.UnitZ
        }
        push *= 0.99f
        val aa = new AABB(Vec3f.Zero, Vec3f.One)
        val bb = aa + push
        val escape = aa.escape(bb)
        (("escape works " + escape) |: !(aa intersects (bb + escape))) &&
        (("escape short " + escape) |: length(escape) <= 0.02f)
    }
}
