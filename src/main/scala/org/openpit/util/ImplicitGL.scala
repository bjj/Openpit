
package org.openpit.util

import java.nio.{ByteBuffer, IntBuffer, FloatBuffer, ByteOrder}


object ImplicitGL {

    implicit def FloatArrayToFloatBuffer(a: Array[Float]): FloatBuffer = {
        val fb = ByteBuffer.allocateDirect(a.length * 4).
            order(ByteOrder.nativeOrder()).asFloatBuffer()
        fb.put(a)
        fb.rewind()
        fb
    }

    implicit def IntArrayToIntBuffer(a: Array[Int]): IntBuffer = {
        val fb = ByteBuffer.allocateDirect(a.length * 4).
            order(ByteOrder.nativeOrder()).asIntBuffer()
        fb.put(a)
        fb.rewind()
        fb
    }
}
