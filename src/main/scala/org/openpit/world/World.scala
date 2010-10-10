package org.openpit.world

import simplex3d.math.intm._
import simplex3d.math.floatm._
import simplex3d.math.floatm.FloatMath._
import org.openpit.world.blocks._
import org.openpit.util._

object World extends Octree[Block] {

    def generate() {
        plain()
    }

    def plain() {
        // some random test objects
        for (x <- -20 until 70; y <- -20 until 70)
            this(x, y, 6) = Water()
        for (z <- 5 to 10; x <- z until 50-z; y <- z until 50-z)
            this(x, y, z) = Grass()
        this(10,10,11) = Stone()
        this(12,10,13) = Stone()
        this(12,10,18) = Stone()
        for (x <- 20 to 30) this(x, 20, 11) = Cobblestone()
        for (x <- 22 to 28) this(x, 20, 12) = Cobblestone()
        for (x <- 20 to 30) this(x, 50, 11) = Stone()
        for (x <- 22 to 28) this(x, 50, 12) = Stone()
        for (y <- 20 until 30; z <- 11 until 15)
            this(15, y, z) = Glass()
    }

    def min(x : Int, y : Int) = {
       if (x > y) y else x
    }

    def put(x: Int, y: Int, z: Int, b: Block) = {
        this(Vec3i(x,y,z)) = b
    }

  /**
   *  A case class Shot is Hit or Miss with a world location and distance
   */
    abstract class Shot extends Ordered[Shot]
    case class Hit(val loc: Vec3i, val distance: Float, val axis: Axes.Axis) extends Shot {
        def compare(that: Shot) = that match {
            case Miss            => -1
            case Hit(_, thatdistance, _) =>
                val delta = distance - thatdistance
                if (delta < 0)      -1
                else if (delta > 0)  1
                else                 0
        }
    }
    case object Miss extends Shot {
        def compare(that: Shot) = that match {
            case Miss        => 0
            case Hit(_,_,_)  => 1
        }
    }

    /**
     * Select the first non-Air block that is hit by the given ray
     *
     * @param  point is the starting point for the search
     * @param  vec is the search direction (assumed normalized vs reach)
     * @param  reach is the distance to search along vec
     * @return The location of the first block hit and the distance to
     *         the intersection with the ray
     */
    def raycast(point: Vec3f, vec: Vec3f, reach: Float): Shot = {
        // bounding box of all blocks we could possibly hit
        val bound = AABB.fromRay(point, vec, reach).rounded
        var result: Shot = Miss
        foreach(bound) {
            case (loc, Air) => Unit
            case (loc, b)   =>
                var t = AABB.fromBlock(loc).raycast(point, vec, reach) match {
                    case None                           => Miss
                    case Some(AxialDistance(axis,dist)) => Hit(loc, dist, axis)
                }
                if (t < result)
                    result = t
        }
        result
    }

    /**
     * Return first non-Air block the given AABB would collide with.
     *
     * @param  point is the starting point for the search
     * @param  vec is the search direction (assumed normalized vs reach)
     * @param  reach is the distance to search along vec
     * @return The location of the first block hit and the distance to
     *         the intersection with the ray
     */
    def sweep(aabb: AABB, vec: Vec3f): Shot = {
        // bounding box of all blocks we could possibly hit
        val bound = aabb union (aabb + vec)
        var result: Shot = Miss
        val absvec = abs(vec)
        foreach(bound) {
            case (loc, Air) => Unit
            case (loc, b)   =>
                var t = AABB.fromBlock(loc).sweep(aabb, vec) match {
                    case Some(AxialDistance(axis, dist)) =>
                        if (absvec(axis) == 0.0f) Miss
                        else                      Hit(loc, dist, axis)
                    case None                  => Miss
                }
                if (t < result)
                    result = t
        }
        result
    }
}
