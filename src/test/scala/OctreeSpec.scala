package org.openpit.test

import collection.mutable.HashSet

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

    // XXX can't use Int here because Octree uses "null" for "nonexist" :(
    property("foreach") = forAll { (c: Vec3i, s: Set[String] ) =>
        val o = new Octree[String](c,2)
        // Put the random set in random locations (locations ignored)
        val where = Arbitrary.arbitrary[Vec3i]
        for (i <- s) { o(where.sample.get) = i }
        s == (HashSet() ++ o)
    }
}
