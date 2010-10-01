package org.openpit.ui

import simplex3d.math.floatm._
import simplex3d.math.floatm.FloatMath._
//import java.lang.Math._

import org.lwjgl.opengl.GL11
import org.lwjgl.util.glu.GLU

object Camera {
    var yaw = 0.0f
    var pitch = 0.0f
    var height = 1.7f
    var x = -5.0f
    var y = -1.0f
    var z = 10.0f

    def look() {
        GL11.glMatrixMode(GL11.GL_MODELVIEW)
        GL11.glLoadIdentity()
        GLU.gluLookAt(0,0,0, 0,1,0, 0,0,1)
        GL11.glRotatef(-pitch, 1f, 0, 0)
        GL11.glRotatef(-yaw, 0, 0, 1.0f)
        GL11.glTranslatef(-x, -y, -z-height)
    }

    def update(dyaw: Float, dpitch: Float) {
        yaw += dyaw
        pitch += dpitch
        if (pitch > 90) pitch = 90
        if (pitch < -90) pitch = -90
    }

    def strafe(d: Float) {
        x += d * cos(yaw toRadians)
        y += d * sin(yaw toRadians)
    }

    def walk(d: Float) {
        x += d * cos(yaw + 90 toRadians)
        y += d * sin(yaw + 90 toRadians)
    }

    def eye = Vec3f(x, y, z + height)

    def direction = {
        val qpitch = quaternion(radians(pitch), Vec3f.UnitX)
        val qroll = quaternion(radians(yaw), Vec3f.UnitZ)
        val lookat = Vec3f.UnitY
        rotateVector(lookat, qroll * qpitch)
    }
}
