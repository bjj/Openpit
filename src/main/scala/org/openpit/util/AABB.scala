package org.openpit.util

import simplex3d.math.floatm._
import simplex3d.math.floatm.FloatMath._


class AABB (a: Vec3f, b: Vec3f) {
    val min = FloatMath.min(a, b)
    val max = FloatMath.max(a, b)

    def contains(v: Vec3f) = {
        min.x <= v.x && v.x <= max.x &&
        min.y <= v.y && v.y <= max.y &&
        min.z <= v.z && v.z <= max.z
    }

    def contains(aabb: AABB) = {
        min.x <= aabb.min.x && aabb.max.x <= max.x &&
        min.y <= aabb.min.y && aabb.max.y <= max.y &&
        min.z <= aabb.min.z && aabb.max.z <= max.z
    }

    override def equals(other: Any) = other match {
        case aabb: AABB => (min == aabb.min && max == aabb.max)
        case _ => false
    }

    /**
     * Round up/down to the nearest int coordinates which enclose the
     * same volume
     */
    def rounded = new AABB(floor(min), ceil(max))

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
    def raycast(p: Vec3f, v: Vec3f, l: Float): Option[Float] = {
        def raycastaxis(px: Float, vx: Float, minx: Float, maxx: Float) = {
            if (abs(vx) < 0.00001f) {
                if (px >= minx && px <= maxx)
                    (Float.MinValue, Float.MaxValue)
                else
                    (Float.MaxValue, Float.MinValue)
            } else {
                val t1 = (minx - px) / vx
                val t2 = (maxx - px) / vx
                if (t1 > t2) (t2, t1)
                else         (t1, t2)
            }
        }
        var (near, far) = raycastaxis(p.x, v.x, min.x, max.x)
        val (ynear, yfar) = raycastaxis(p.y, v.y, min.y, max.y)
        near = near.max(ynear)
        far = far.min(yfar)
        if (near > far) {
            None    // miss
        } else {
            val (znear, zfar) = raycastaxis(p.z, v.z, min.z, max.z)
            near = near.max(znear)
            far = far.min(zfar)
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

    override def toString = ("AABB(" + min + ", " + max +")")
}

object AABB {
    /**
     * Create an AABB from a AABB.raycast-style point, direction and length
     */
    def fromRay(p: Vec3f, v: Vec3f, l: Float) = new AABB(p, p + v * l)
}
