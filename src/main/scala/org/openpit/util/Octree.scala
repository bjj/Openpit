package org.openpit.util

import collection.Traversable

import simplex3d.math.intm._
import simplex3d.math.intm.IntMath._

/**
 *       6---7
 *      /|  /|
 *     4---5 |  z
 *     | 2-|-3  | y
 *     |/  |/   |/      In a leaf node, the block "named" with
 *     0---1    o---x   integer coord 'center' is at 7 (essentially
 *                      there is an implicit offset of 0.5,0.5,0.5)
 *
 *                      So an Octree with center x,y,z and radius r
 *                      covers (x-r,y-r,z-r))..(x+(r-1),y+(r-1),z+(r-1))
 *                      inclusive.  E.g. a leaf at 0,0,0 r=1 contains in order:
 *                      (-1,-1,-1), (0,-1,-1), (-1,0,-1), (0, 0, -1)
 *                      (-1,-1, 0), (0,-1, 0), (-1,0, 0), (0, 0, 0)
 */

abstract class IOctree[T:Manifest] (var center: inVec3i, var radius: Int) extends Traversable[T] {

    final def contains(p: inVec3i) = {
        val d = p - center
        (d.x < radius && d.x >= -radius &&
         d.y < radius && d.y >= -radius &&
         d.z < radius && d.z >= -radius)
    }

    final implicit def toAABB = new AABB(center - Vec3i.One * radius,
                                         center + Vec3i.One * radius)

    final def contains(aabb: AABB): Boolean = toAABB contains aabb
    final def intersects(aabb: AABB): Boolean = toAABB intersects aabb
    final def inside(aabb: AABB) = aabb contains toAABB

    protected final def indexOf(p: inVec3i) = {
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
    protected final def lerp(p: inVec3i, dist: Int): Vec3i =
        lerp(indexOf(p), dist)

    def apply(p: inVec3i): T
    def apply(x: Int, y: Int, z: Int): T = apply(ConstVec3i(x,y,z))
    def get(p: inVec3i): Option[T]
    def update(p: inVec3i, value: T): Unit
    def update(x: Int, y: Int, z: Int, value: T) {
        update(ConstVec3i(x,y,z), value)
    }
    def foreach[U](f: (inVec3i, T) => U)
    def foreach[U](bound: AABB)(f: (inVec3i, T) => U)
    def foreach[U](f: (T) => U)
}

case class OctreeNode[T:Manifest] (c: inVec3i, r: Int) extends IOctree[T](c, r) {

    val children = new Array[IOctree[T]](8)

    def get(p: inVec3i): Option[T] = children(indexOf(p)) match {
        case null => None
        case c    => c.get(p)
    }
    def apply(p: inVec3i): T = children(indexOf(p))(p)

    // Create a new child (call only if needed)
    protected def grow(index: Int) = {
        val newr = radius / 2
        val child = if (newr == 1) OctreeLeaf[T](lerp(index, newr))
                              else OctreeNode[T](lerp(index, newr), newr)
        children(index) = child
        child
    }

    private[util] def setsubtree(p: inVec3i, subtree: OctreeNode[T]) {
        children(indexOf(p)) = subtree
    }

    def update(p: inVec3i, value: T) {
        val i = indexOf(p)
        children(i) match {
            case null => grow(i).update(p, value)
            case c    => c.update(p, value)
        }
    }

    def foreach[U](f: (inVec3i, T) => U) =
        for (c <- children if c != null) c.foreach(f)
    def foreach[U](bound: AABB)(f: (inVec3i, T) => U) = {
        if (inside(bound))
            foreach(f)  // no bounds checking needed within
        else if (intersects(bound))
            for (c <- children if c != null) c.foreach(bound)(f)
    }
    def foreach[U](f: (T) => U) =
        for (c <- children if c != null) c.foreach(f)
}

case class Octree[T:Manifest] (c: inVec3i = ConstVec3i(0,0,0)) extends IOctree[T](c, 2) {
    var tree = OctreeNode[T](c, 2)

    /**
     * Clear the octree of everything
     */
    def clear() {
        radius = 2
        tree = OctreeNode[T](center, radius)
    }

    /**
     * Expand Octree one level, shifting the center toward p
     */
    protected def expand(p: inVec3i) {
        var repl = OctreeNode[T](lerp(p, radius), radius * 2)
        repl.setsubtree(center, tree)
        tree = repl
        center = tree.center
        radius = tree.radius
    }

    def apply(p: inVec3i) = tree.apply(p)
    def get(p: inVec3i) = tree.get(p)
    def update(p: inVec3i, value: T) {
        while (!contains(p)) expand(p)
        tree(p) = value
    }

    def foreach[U](f: (inVec3i, T) => U) = tree.foreach(f)
    def foreach[U](bound: AABB)(f: (inVec3i, T) => U) = tree.foreach(bound)(f)
    def foreach[U](f: (T) => U) = tree.foreach(f)
}

case class OctreeLeaf[T:Manifest] (c: inVec3i) extends IOctree[T](c, 1) {

    var children = new Array[T](8)

    def get(p: inVec3i): Option[T] = children(indexOf(p)) match {
        case null => None
        case c    => Some(c)
    }
    def apply(p: inVec3i): T = children(indexOf(p))
    def update(p: inVec3i, value: T) { children(indexOf(p)) = value }

    def foreach[U](f: (inVec3i, T) => U) =
        for (ci <- 0 until 8 if children(ci) != null) {
            f(center + LeafOffsets(ci), children(ci))
        }
    def foreach[U](bound: AABB)(f: (inVec3i, T) => U) = {
        val me = toAABB
        if (bound contains me)
            foreach(f)  // no bounds checking needed within
        else if (bound intersects me)
            for (ci <- 0 until 8 if children(ci) != null) {
                val loc = center + LeafOffsets(ci)
                if (bound.intersects(AABB.fromBlock(loc)))
                    f(loc, children(ci))
            }
    }
    def foreach[U](f: (T) => U) =
        for (ci <- 0 until 8 if children(ci) != null) {
            f(children(ci))
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
}
