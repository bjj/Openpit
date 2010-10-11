package org.openpit.world

import simplex3d.math.intm._
import simplex3d.math.floatm._
import simplex3d.math.floatm.FloatMath._

import org.openpit.util._
import org.openpit.ui.Input
import org.openpit.util.Axes._
import constants._

/*
class Body {
    var height = 0.0f
    var radius = 0.0f

    var yaw = 0.0f
    var pitch = 0.0f

    var loc = Vec3f(0, 0, 0)
    var prevloc = loc
    var acc = Vec3f(0, 0, 0)

    def qpitch() = quaternion(radians(pitch), Vec3f.UnitX)
    def qyaw(dyaw: Float = 0) = quaternion(radians(yaw + dyaw), Vec3f.UnitZ)

    def step(dt: Float, prevdt: Float) {
        val vel = (loc - prevloc) / prevdt
        oldloc = loc.clone()

        acc = Vec3f(0, 0, 0)
    }

    def onGround = true

    def canJump = onGround

    def move(m: Input.Move) {
        aim(m.yaw, m.pitch)
        acc += walkAcc * rotateVector(lookat, qyaw(0)) * m.walk.y +
               walkAcc * rotateVector(lookat, qyaw(-90)) * m.walk.x +
               (if (m.jump && canJump) jumpAcc else Vec3f.Zero)
    }

}
*/
