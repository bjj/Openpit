package org.openpit.ui

import org.openpit.util.AABB

import simplex3d.math.floatm._
import simplex3d.math.floatm.FloatMath._
//import java.lang.Math._

import org.lwjgl.opengl.GL11
import org.lwjgl.util.glu.GLU

object Camera {
    var yaw = 0.0f
    var pitch = 0.0f
    var height = 1.7f
    var radius = 0.3f
    var loc = Vec3f(-5f, -1f, 10f)

    val gravity = Vec3f(0f, 0f, -9.8f) // -9.8m/s^2
    val jumpForce = Vec3f(0f, 0f, 5f)  //  5.0m/s^2

    var usingGravity = false
    var jumpVector = Vec3f(0f, 0f, 0f)

    def look() {
        GL11.glMatrixMode(GL11.GL_MODELVIEW)
        GL11.glLoadIdentity()
        GLU.gluLookAt(0,0,0, 0,1,0, 0,0,1)
        GL11.glRotatef(-pitch, 1f, 0, 0)
        GL11.glRotatef(-yaw, 0, 0, 1.0f)
        GL11.glTranslatef(-loc.x, -loc.y, -loc.z-height)
    }

    def beginJump() {
        usingGravity = true
        jumpVector = jumpForce clone
    }

    def update(dyaw: Float, dpitch: Float, elapsedTime: Float) {
        if (usingGravity) {
            import org.openpit.world.World
            World.raycast(loc, jumpVector, elapsedTime) match {
                case World.Hit(blockloc, when, _) =>
                    if (jumpVector.z > 0)
                        loc.z = blockloc.z + 1
                    else
                        loc += jumpVector * when
                    jumpVector = Vec3f.Zero
                case World.Miss =>
                    loc += jumpVector * elapsedTime
                    jumpVector += gravity * elapsedTime
            }
        }
        yaw += dyaw
        pitch += dpitch
        if (pitch > 90) pitch = 90
        if (pitch < -90) pitch = -90
    }

    def strafe(d: Float) {
        loc.x += d * cos(yaw toRadians)
        loc.y += d * sin(yaw toRadians)
    }

    def walk(d: Float) {
        loc.x += d * cos(yaw + 90 toRadians)
        loc.y += d * sin(yaw + 90 toRadians)
    }

    def climb(d : Float) {
        loc.z += d
        if (d != 0.0F) usingGravity = false
    }

    def fly(d: Float) {
        loc += direction * d
    }

    def eye = loc + Vec3f(0, 0, height)

    def direction = {
        val qpitch = quaternion(radians(pitch), Vec3f.UnitX)
        val qroll = quaternion(radians(yaw), Vec3f.UnitZ)
        val lookat = Vec3f.UnitY
        rotateVector(lookat, qroll * qpitch)
    }

    def collisionBox() = new AABB(loc - ConstVec3f(1,1,0) * radius,
                                  eye + ConstVec3f(1,1,0) * radius)

}
