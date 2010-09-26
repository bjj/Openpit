
package org.openpit.util

import java.nio.{ByteBuffer, FloatBuffer, ByteOrder}


object ImplicitGL {

    implicit def FloatArrayToFloatBuffer(a: Array[Float]): FloatBuffer = {
	val fb = ByteBuffer.allocateDirect(a.length * 4).
	    order(ByteOrder.nativeOrder()).asFloatBuffer()
	fb.put(a)
	fb.rewind()
	fb
    }
}
