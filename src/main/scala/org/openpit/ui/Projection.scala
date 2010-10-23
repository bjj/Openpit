package org.openpit.ui

import org.lwjgl.opengl.GL11._
import org.lwjgl.util.glu._

import org.lwjgl.BufferUtils.createFloatBuffer
import simplex3d.math.floatm._
import simplex3d.math.floatm.FloatMath._

object Projection {
    var current: () => Unit = null

    private def setup_ortho() {
        glMatrixMode(GL_MODELVIEW)
        glLoadIdentity()
        glMatrixMode(GL_PROJECTION)
        glLoadIdentity()
        glOrtho(0, Window.width, Window.height, 0, 0, 1)
    }

    val maxRenderDistance = 250f
    private def setup_perspective() {
        import org.openpit.ui.Camera
        // The ratio of znear to zfar determines how Z-buffer values
        // will be calculated.  If znear is too low, there will be too
        // little precision in the Z-buffer for distant pixels which
        // will cause ugly twittering
        glMatrixMode(GL_PROJECTION)
        glLoadIdentity()
        GLU.gluPerspective(40, Window.aspect, 0.09f, maxRenderDistance)
        Camera.look()
    }

    def set(way: () => Unit) {
        if (way != current) {
            current = way
            current()
        }
    }

    def ortho() { set(setup_ortho) }
    def perspective() { set(setup_perspective) }

    def update() {
        current = null
        perspective()
    }

    /**
     * Get a matrix from OpenGL.  Note that the GL calls are single
     * threaded anyway, so we can re-use a floatbuffer as temporary
     * storage
     */
    lazy val matrixBuf = createFloatBuffer(16)
    lazy val matrixArray = Array.ofDim[Float](16)
    def getMatrix(kind: Int) = {
        matrixBuf.clear()
        glGetFloat(kind, matrixBuf)
        matrixBuf.get(matrixArray)
        val a = matrixArray
        /* XXX I totally fail to understand why this is wrong:
        ConstMat4f(a(0), a(4), a( 8), a(12),
                   a(1), a(5), a( 9), a(13),
                   a(2), a(6), a(10), a(14),
                   a(3), a(7), a(11), a(15))
        */
        ConstMat4f(a( 0), a( 1), a( 2), a( 3),
                   a( 4), a( 5), a( 6), a( 7),
                   a( 8), a( 9), a(10), a(11),
                   a(12), a(13), a(14), a(15))
    }

    def modelMatrix = getMatrix(GL_MODELVIEW_MATRIX)
    def projectionMatrix = getMatrix(GL_PROJECTION_MATRIX)

    /**
     * Find current view frustum as six planes of the form ax+by+cz+d = 0
     * a,b,c is normalized
     */
    def frustum = {
        def plane(a: Float, b: Float, c: Float, d: Float) = {
            var result = Vec4f(a,b,c,0)
            var l = length(result)
            result.w = d
            result /= l
            result
        }

        val pm = projectionMatrix * modelMatrix

         // Near/Far
        plane(pm.m30+pm.m20, pm.m31+pm.m21, pm.m32+pm.m22, pm.m33+pm.m23) ::
        plane(pm.m30-pm.m20, pm.m31-pm.m21, pm.m32-pm.m22, pm.m33-pm.m23) ::
         // Left/Right
        plane(pm.m30+pm.m00, pm.m31+pm.m01, pm.m32+pm.m02, pm.m33+pm.m03) ::
        plane(pm.m30-pm.m00, pm.m31-pm.m01, pm.m32-pm.m02, pm.m33-pm.m03) ::
         // Bottom/Top
        plane(pm.m30+pm.m10, pm.m31+pm.m11, pm.m32+pm.m12, pm.m33+pm.m13) ::
        plane(pm.m30-pm.m10, pm.m31-pm.m11, pm.m32-pm.m12, pm.m33-pm.m13) ::
        Nil
    }
}

