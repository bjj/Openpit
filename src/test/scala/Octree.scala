package org.openpit.test

import org.openpit.util.Octree
import simplex3d.math.intm._

import org.scalacheck._

object OctreeSpec extends Properties("Octree") {
    import Prop.forAll
    import Generators._

    property("single storage") = forAll { (c: Vec3i, p: Vec3i, v: Int) =>
        val o = new Octree[Int](c,2)
        o(p) = v
        o(p) == v
    }

    property("multiple storage") = {
        val o = new Octree[Int](Vec3i(0,0,0), 2)
        forAll { (c: Vec3i, p: Vec3i, v: Int) => o(p) = v; o(p) == v }
    }
}
