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
    var loc = Vec3f(20.5f, 20.5f, 11.5f)
    var prevloc = loc clone
    var prevdt = 1.0f

    val gravity = Vec3f(0f, 0f, -9.8f) // m/s^2
    val jumpAcc = Vec3f(0f, 0f, 18f)   // m/s^2
    val walkAcc = 5f                   // m/s^2
    val friction = 0.95f

    var usingGravity = false

    def qpitch = quaternion(radians(pitch), Vec3f.UnitX)
    def qyaw(dyaw: Float = 0) = quaternion(radians(yaw + dyaw), Vec3f.UnitZ)
    val lookat = Vec3f.UnitY    // matches gluLookAt call

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
    }

    def aim(dyaw: Float, dpitch: Float) {
        yaw += dyaw
        pitch += dpitch
        if (pitch > 90) pitch = 90
        if (pitch < -90) pitch = -90
    }

    def canJump = true

    def update(elapsedTime: Float, move: Option[Input.Move] = None) {
        val userAcc = move match {
            case Some(m) =>
                aim(m.yaw, m.pitch)
                walkAcc * rotateVector(lookat, qyaw(0)) * m.dy +
                walkAcc * rotateVector(lookat, qyaw(-90)) * m.dx +
                (if (m.jump && canJump) jumpAcc else Vec3f.Zero)
            case None => Vec3f.Zero
        }
        val origloc = loc clone
        val vel = (loc - prevloc) / prevdt
        val frictionAcc = -vel * friction
        val acc = userAcc + frictionAcc + gravity
        val trymove = vel + acc

        import org.openpit.world.World
        import org.openpit.util.Axes._
        def trimAxis(full: Vec3f, partial: Vec3f, ax: Axis) = ax match {
            case XX => Vec3f(partial.x, full.y, full.z)
            case YY => Vec3f(full.x, partial.y, full.z)
            case ZZ => Vec3f(full.x, full.y, partial.z)
        }
        // XXX Arrgh this is so close to working.  The problem is that
        // it only works for one 'Axis' (the first hit) which is always
        // Z so you pass right through blocks sideways.  I don't feel
        // like putting in a loop over all axes for this hack collision...
        World.raycast(loc, trymove, elapsedTime) match {
            case World.Hit(blockloc, when, ZZ) =>
                loc += trimAxis(trymove * elapsedTime, trymove * when, ZZ)
                prevloc = trimAxis(origloc, loc, ZZ)
            case World.Hit(blockloc, when, axis) =>
                loc += trimAxis(trymove * elapsedTime, trymove * when, axis)
                prevloc = trimAxis(origloc, loc, axis)
            case World.Miss =>
                loc += trymove * elapsedTime
                prevloc = origloc
        }
        prevdt = elapsedTime
    }

    def climb(d : Float) {
        loc.z += d
        if (d != 0.0F) usingGravity = false
    }

    def fly(d: Float) {
        loc += direction * d
    }

    def eye = loc + Vec3f(0, 0, height)
    def direction = rotateVector(lookat, qyaw() * qpitch)

    def collisionBox() = new AABB(loc - ConstVec3f(1,1,0) * radius,
                                  eye + ConstVec3f(1,1,0) * radius)

}
