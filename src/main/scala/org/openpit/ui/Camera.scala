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

    def look() {
        GL11.glMatrixMode(GL11.GL_MODELVIEW)
        GL11.glLoadIdentity()
        GLU.gluLookAt(0,0,0, 0,1,0, 0,0,1)
        GL11.glRotatef(-pitch, 1f, 0, 0)
        GL11.glRotatef(-yaw, 0, 0, 1.0f)
        GL11.glTranslatef(-loc.x, -loc.y, -loc.z-height)
    }

    def update(dyaw: Float, dpitch: Float) {
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
