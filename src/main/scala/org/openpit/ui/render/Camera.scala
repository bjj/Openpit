package org.openpit.ui.render

import org.lwjgl.opengl.GL11
import org.lwjgl.util.glu.GLU
import java.lang.Math._

object Camera {
    var yaw = 0.0f
    var pitch = 0.0f
    var height = 1.7f
    var x = -5.0
    var y = -1.0
    var z = 10.0

    def look() {
	GL11.glMatrixMode(GL11.GL_MODELVIEW)
	GL11.glLoadIdentity()
	GLU.gluLookAt(0,0,0, 0,1,0, 0,0,1)
	GL11.glRotatef(-pitch, 1f, 0, 0)
	GL11.glRotatef(-yaw, 0, 0, 1.0f)
	GL11.glTranslatef(-x toFloat, -y toFloat, -z-height toFloat)
    }

    def update(dyaw: Float, dpitch: Float) {
	yaw += dyaw
	pitch += dpitch
	if (pitch > 90) pitch = 90
	if (pitch < -90) pitch = -90
    }

    def strafe(d: Double) {
	x += d * cos(yaw toRadians)
	y += d * sin(yaw toRadians)
    }

    def walk(d: Double) {
	x += d * cos(yaw + 90 toRadians)
	y += d * sin(yaw + 90 toRadians)
    }
}
