package org.openpit.util

import collection.Traversable

import simplex3d.math.intm._
import simplex3d.math.intm.IntMath._

/*
         6---7
        /|  /|
       4---5 |  z
       | 2-|-3  | y
       |/  |/   |/      In a leaf node, the block "named" with
       0---1    o---x   integer coord 'center' is at 7 (essentially
                        there is an implicit offset of 0.5,0.5,0.5)

                        So an Octree with center x,y,z and radius r
                        covers (x-r,y-r,z-r))..(x+(r-1),y+(r-1),z+(r-1))
                        inclusive.  E.g. a leaf at 0,0,0 r=1 contains in order:
                        (-1,-1,-1), (0,-1,-1), (-1,0,-1), (0, 0, -1)
                        (-1,-1, 0), (0,-1, 0), (-1,0, 0), (0, 0, 0)
*/

abstract class IOctree[T:Manifest] (var center: Vec3i, var radius: Int) extends Traversable[T] {

    type Child

    def makeChildren: Array[Child]
    var children = makeChildren

    final def encloses(p: Vec3i) = {
        val d = p - center
        (d.x < radius && d.x >= -radius &&
         d.y < radius && d.y >= -radius &&
         d.z < radius && d.z >= -radius)
    }

    protected final def indexOf(p: Vec3i) = {
        val d = p - center
        (if (d.x < 0) 0 else 1) +
        (if (d.y < 0) 0 else 2) +
        (if (d.z < 0) 0 else 4)
    }

    protected final def lerp(index: Int, dist: Int) = {
        Vec3i(if ((index & 1) == 0) center.x - dist else center.x + dist,
              if ((index & 2) == 0) center.y - dist else center.y + dist,
              if ((index & 4) == 0) center.z - dist else center.z + dist)
    }
    protected final def lerp(p: Vec3i, dist: Int): Vec3i =
        lerp(indexOf(p), dist)

    def apply(p: Vec3i): T
    def update(p: Vec3i, value: T): Unit
    def update(x: Int, y: Int, z: Int, value: T) {
        update(ConstVec3i(x,y,z), value)
    }
    def foreach[U](f: (Vec3i, T) => U)
    def foreach[U](f: (T) => U)
}

case class Octree[T:Manifest] (c: Vec3i, r: Int) extends IOctree[T](c, r) {

    type Child = IOctree[T]

    def makeChildren = new Array[Child](8)

    def apply(p: Vec3i): T = children(indexOf(p))(p)

    // Create a new child (call only if needed)
    protected def grow(index: Int) = {
        val newr = radius / 2
        val child = if (newr == 1) new OctreeLeaf[T](lerp(index, newr))
                              else new Octree[T](lerp(index, newr), newr)
        children(index) = child
        child
    }

    // Wrap self in a larger enclosing Octree, moving toward p
    protected def wrap(p: Vec3i) {
        var repl = new Octree[T](center, radius)
        repl.children = children
        children = makeChildren
        center = lerp(p, radius)
        radius *= 2
        children(indexOf(repl.center)) = repl
    }

    def update(p: Vec3i, value: T) {
        while (!encloses(p)) wrap(p)
        val i = indexOf(p)
        children(i) match {
            case null => grow(i).update(p, value)
            case c    => c.update(p, value)
        }
    }

    def foreach[U](f: (Vec3i, T) => U) =
        for (c <- children if c != null) c.foreach(f)
    def foreach[U](f: (T) => U) =
        for (c <- children if c != null) c.foreach(f)
}

private object LeafOffsets {
    lazy val offsets = Array(
            ConstVec3i(-1,-1,-1),
            ConstVec3i( 0,-1,-1),
            ConstVec3i(-1, 0,-1),
            ConstVec3i( 0, 0,-1),
            ConstVec3i(-1,-1, 0),
            ConstVec3i( 0,-1, 0),
            ConstVec3i(-1, 0, 0),
            ConstVec3i( 0, 0, 0))

    def apply(i:Int) = offsets(i)
}

case class OctreeLeaf[T:Manifest] (c: Vec3i) extends IOctree[T](c, 1) {

    type Child = T

    def makeChildren = new Array[Child](8)

    def apply(p: Vec3i): T = children(indexOf(p))
    def update(p: Vec3i, value: T) { children(indexOf(p)) = value }

    def foreach[U](f: (Vec3i, T) => U) =
        for (ci <- 0 until 8 if children(ci) != null) {
            f(center + LeafOffsets(ci), children(ci))
        }
    def foreach[U](f: (T) => U) =
        for (ci <- 0 until 8 if children(ci) != null) {
            f(children(ci))
        }
}
