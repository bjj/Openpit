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
    var loc = Vec3f(20.5f, 20.5f, 12.5f)
    var prevloc = loc clone
    var prevdt = 1.0f

    val gravity = Vec3f(0f, 0f, -9.8f) // m/s^2
    val jumpAcc = Vec3f(0f, 0f, 18f)   // m/s^2
    val walkAcc = 50f                  // m/s^2 ??
    val stopTime = 0.1f // s

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
                walkAcc * rotateVector(lookat, qyaw(0)) * m.walk.y +
                walkAcc * rotateVector(lookat, qyaw(-90)) * m.walk.x +
                (if (m.jump && canJump) jumpAcc else Vec3f.Zero)
            case None => Vec3f.Zero
        }
        val origloc = loc clone
        val vel = (loc - prevloc) / prevdt
        // XXX wanted to remove vel.z here, clearly there's a bug
        val frictionAcc = -Vec3f(vel.x, vel.y, 0) / stopTime
        val acc = userAcc + frictionAcc + gravity
        val trymove = vel + acc * elapsedTime

        import org.openpit.world.World
        import org.openpit.util.Axes._
        def trimAxis(full: Vec3f, partial: Vec3f, ax: Axis) = ax match {
            case XX => Vec3f(partial.x, full.y, full.z)
            case YY => Vec3f(full.x, partial.y, full.z)
            case ZZ => Vec3f(full.x, full.y, partial.z)
        }
        // XXX tried to leave out escapecount but still hit an inf loop
        var escapecount = 0
        /**
         * Helper function:  Collide 'mybox' with world along 'trymove'.
         * If already inside a block, first "escape".
         * If anything is hit, move until that hit, then zero that movement
         * axis and try to complete the move.  World.sweep ignores hits
         * along non-moving axes
         */
        def collide(mybox: AABB, loc: Vec3f, trymove: Vec3f): Vec3f = {
            World.sweep(mybox, trymove) match {
                case World.Hit(blockloc, 0f, _) =>
                    escapecount += 1
                    if (escapecount < 10) {
                        val moved = AABB.fromBlock(blockloc).escape(mybox, 0.01f)
                        collide(mybox + moved, loc + moved, trymove - moved)
                    } else
                        loc
                case World.Hit(blockloc, when, axis) =>
                    val moved = trymove * when
                    collide(mybox + moved, loc + moved,
                            trimAxis(trymove - moved, Vec3f.Zero, axis))
                case World.Miss =>
                    loc + trymove
            }
        }

        loc = collide(collisionBox, loc, trymove * elapsedTime)
        prevdt = elapsedTime
        prevloc = origloc
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
