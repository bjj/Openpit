package org.openpit.util

import simplex3d.math.intm._
import simplex3d.math.floatm._
import simplex3d.math.floatm.FloatMath._

object Axes extends Enumeration {
  type Axis = Value
  val XX, YY, ZZ = Value

  implicit def toInt(axis: Axis) = axis.id
}

import Axes._

final case class AxialDistance(axis: Axis, distance: Float) {
    final def min(other: AxialDistance): AxialDistance = {
        if (other.distance < distance) other else this
    }
    final def max(other: AxialDistance): AxialDistance = {
        if (other.distance > distance) other else this
    }
    final def <(other: AxialDistance) = distance < other.distance
    final def >(other: AxialDistance) = distance > other.distance
    final def <=(other: AxialDistance) = distance <= other.distance
    final def >=(other: AxialDistance) = distance >= other.distance
    final def <(dist: Float) = distance < dist
    final def >(dist: Float) = distance > dist
    final def <=(dist: Float) = distance <= dist
    final def >=(dist: Float) = distance >= dist
}

class AABB (a: Vec3f, b: Vec3f) {
    val min = FloatMath.min(a, b)
    val max = FloatMath.max(a, b)

    final def contains(v: Vec3f) = {
        min.x <= v.x && v.x <= max.x &&
        min.y <= v.y && v.y <= max.y &&
        min.z <= v.z && v.z <= max.z
    }

    final def contains(aabb: AABB) = {
        min.x <= aabb.min.x && aabb.max.x <= max.x &&
        min.y <= aabb.min.y && aabb.max.y <= max.y &&
        min.z <= aabb.min.z && aabb.max.z <= max.z
    }

    final def intersects(aabb: AABB) = {
        ! ((max.x < aabb.min.x) || (min.x > aabb.max.x) ||
           (max.y < aabb.min.y) || (min.y > aabb.max.y) ||
           (max.z < aabb.min.z) || (min.z > aabb.max.z))
    }

    final def + (v: Vec3f) = new AABB(min + v, max + v)

    final override def equals(other: Any) = other match {
        case aabb: AABB => (min == aabb.min && max == aabb.max)
        case _ => false
    }

    /**
     * Round up/down to the nearest int coordinates which enclose the
     * same volume
     */
    final def rounded = new AABB(floor(min), ceil(max))

    final def center = (min + max) * 0.5f

    /**
     * Cast a ray toward this AABB and find the distance.
     *
     * XXX Over many AABB this should be refactored to eliminate
     # common subexpressions like 1 ./ v
     *
     * @param  p The start point of the ray
     * @param  v The direction of the ray
     * @param  l The length of the ray
     * @return Distance to the closest point of intersection.
     *         Intersection point is then p + v * result and
     *         result <= l.
     */
    final def raycast(p: Vec3f, v: Vec3f, l: Float): Option[AxialDistance] = {
        def raycastaxis(px: Float, vx: Float, minx: Float, maxx: Float, axis: Axis) = {
            def ax(f: Float) = AxialDistance(axis, f)
            import Float.{MinValue, MaxValue}
            if (abs(vx) < 0.00001f) {
                if (px >= minx && px <= maxx)
                    (ax(MinValue), MaxValue)
                else
                    (ax(MaxValue), MinValue)
            } else {
                val t1 = (minx - px) / vx
                val t2 = (maxx - px) / vx
                if (t1 > t2) (ax(t2), t1)
                else         (ax(t1), t2)
            }
        }
        var (near, far) = raycastaxis(p.x, v.x, min.x, max.x, XX)
        val (ynear, yfar) = raycastaxis(p.y, v.y, min.y, max.y, YY)
        near = near max ynear
        far = far min yfar
        if (near > far) {
            None    // miss
        } else {
            val (znear, zfar) = raycastaxis(p.z, v.z, min.z, max.z, ZZ)
            near = near max znear 
            far = far min zfar 
            if (near > far) {
                None    // miss
            } else if (far < 0f) {
                None    // behind
            } else if (near <= l) {
                Some(near)
            } else {
                None    // too far
            }
        }
    }

    /**
     * Sweep an AABB toward this box and find the distance after
     * which they hit (if any).  Like raycasting but with an AABB
     * instead of a point.  If both AABBs are moving, do the sweep
     * with one stationary and the other having the relative velocity.
     *
     * @param  other The other AABB
     * @param  v     The direction vector of the other AABB
     * @return The time at which the collision begins, which is 0.0 for
     *         already overlapping, up to 1.0 for collisions within the
     *         length of v, and >1.0 if they would eventually hit later.
     *         The objects just touch if you move
     *         other by v * result.distance.
     *
     *         None if they are not touching and never touch along the
     *         given vector.
     */
    final def sweep(other: AABB, v: Vec3f): Option[AxialDistance] = {
        def starthit(axis: Axis) = AxialDistance(axis,
            if (v(axis) < 0)
                ((max(axis) - other.min(axis)) / v(axis)) max 0f
            else if (v(axis) > 0)
                ((min(axis) - other.max(axis)) / v(axis)) max 0f
            else
                Float.MaxValue  // case hidden by intersects test
        )
        def endhit(axis: Axis) = AxialDistance(axis,
            if (v(axis) < 0)
                ((min(axis) - other.max(axis)) / v(axis)) max 0f
            else if (v(axis) > 0)
                ((max(axis) - other.min(axis)) / v(axis)) max 0f
            else
                Float.MaxValue  // case hidden by intersects test
        )

        if (intersects(other)) {
            Some(AxialDistance(ZZ, 0f))
        } else {
            val start = starthit(XX) max starthit(YY) max starthit(ZZ)
            val end = endhit(XX) min endhit(YY) min endhit(ZZ)
            if (start < end)
                Some(start)
            else
                None
        }
    }

    final override def toString = ("AABB(" + min + ", " + max +")")
}

object AABB {
    /**
     * Create an AABB from a AABB.raycast-style point, direction and length
     */
    def fromRay(p: Vec3f, v: Vec3f, l: Float) = new AABB(p, p + v * l)

    /**
     * Create an AABB for a given block location
     */
    def fromBlock(p: Vec3i) = new AABB(p, p + ConstVec3i(1,1,1))
}
