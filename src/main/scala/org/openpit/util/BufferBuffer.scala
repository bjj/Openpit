package org.openpit.util

import scala.collection.mutable._
import scala.collection.Traversable

/**
 * A Buffer[Float] compatible class built on top of a
 * java.nio.FloatBuffer.
 *
 * This can't readily be made generic because java.nio.Buffer is a
 * terrible base class.  Also, allowing type variance in the Buffer[A]
 * we extend leads to craziness like trying to put strings in a FloatBuffer.
 */
class FloatBufferBuffer(protected val initialSize: Int)
    extends Buffer[Float]
    with IndexedSeqOptimized[Float, Buffer[Float]] {

    // this wouldn't be hard to do by hand (without lwjgl)
    import org.lwjgl.BufferUtils.createFloatBuffer

    protected var buf = createFloatBuffer(initialSize)

    def this() = this(16)

    def clear() { 
        buf.rewind()
        buf.limit(buf.capacity)
    }
    def result() = { 
        val robuf = buf.asReadOnlyBuffer()
        robuf.limit(buf.position)
        robuf.rewind()
        robuf
    }
    def length() = { buf.position }

    def update (n: Int, elem: Float) { buf.put(n, elem) }
    def apply (n: Int) = { buf.get(n) }

    final def += (elem: Float): this.type = {
        ensureSize(length + 1)
        buf.put(elem)
        this
    }

    import simplex3d.math.floatm._

    final def += (elem: inVec2f): this.type = {
        ensureSize(length + 2)
        buf.put(elem.x); buf.put(elem.y);
        this
    }

    final def += (elem: inVec3f): this.type = {
        ensureSize(length + 3)
        buf.put(elem.x); buf.put(elem.y); buf.put(elem.z)
        this
    }

    final def +=: (elem: Float): this.type = {
        val newbuf = createFloatBuffer(length + 1)
        newbuf.put(elem)
        buf.rewind()
        newbuf.put(buf)
        this
    }

    def insertAll(n: Int, seq: Traversable[Float]) {
        val xs = seq.toList
        val more = xs.length
        ensureSize(length + more)
        for (i <- (length + more - 1) to (n + more - 1) by -1) {
            buf.put(i, buf.get(i - more))
        }
        var i = n
        xs.foreach { x =>
            buf.put(i, x)
            i += 1
        }
        buf.position(length + more)
    }

    def remove(n: Int) = {
        val result = apply(n)
        val savepos = buf.position - 1
        val newbuf = createFloatBuffer(buf.capacity)
        for (i <- 0 until n)
            newbuf.put(buf.get(i))
        for (i <- n + 1 until length)
            newbuf.put(buf.get(i))
        newbuf.position(savepos)
        buf = newbuf
        result
    }

    protected final def ensureSize(n: Int) {
        if (n > buf.capacity) {
            var newsize = buf.capacity * 2
            while (n > newsize) newsize *= 2
            realloc(newsize)
        }
    }

    def sizeHint(len: Int) {
        if (len > buf.capacity && len > 0)
            realloc(len)
    }

    protected def realloc(len: Int) {
        val savepos = buf.position
        val newbuf = createFloatBuffer(len)
        buf.rewind()
        newbuf.put(buf)
        newbuf.position(savepos)
        buf = newbuf
    }

    final val sizeof = 4
    def bytes() = length * sizeof
}

object FloatBufferBuffer {
    implicit def FloatBufferBuffer2FloatBuffer(fbb: FloatBufferBuffer) = fbb.buf
}

/**
 * @see FloatBufferBuffer
 */
class IntBufferBuffer(protected val initialSize: Int)
    extends Buffer[Int]
    with IndexedSeqOptimized[Int, Buffer[Int]] {

    // this wouldn't be hard to do by hand (without lwjgl)
    import org.lwjgl.BufferUtils.createIntBuffer

    protected var buf = createIntBuffer(initialSize)

    def this() = this(16)

    def clear() {
        buf.rewind()
        buf.limit(buf.capacity)
    }
    def result() = { 
        val robuf = buf.asReadOnlyBuffer()
        robuf.limit(buf.position)
        robuf.rewind()
        robuf
    }
    def length() = { buf.position }

    def update (n: Int, elem: Int) { buf.put(n, elem) }
    def apply (n: Int) = { buf.get(n) }

    final def += (elem: Int): this.type = {
        ensureSize(length + 1)
        buf.put(elem)
        this
    }

    import simplex3d.math.intm._

    final def += (elem: inVec2i): this.type = {
        ensureSize(length + 2)
        buf.put(elem.x); buf.put(elem.y);
        this
    }

    final def += (elem: inVec3i): this.type = {
        ensureSize(length + 3)
        buf.put(elem.x); buf.put(elem.y); buf.put(elem.z)
        this
    }

    final def +=: (elem: Int): this.type = {
        val newbuf = createIntBuffer(length + 1)
        newbuf.put(elem)
        buf.rewind()
        newbuf.put(buf)
        this
    }

    def insertAll(n: Int, seq: Traversable[Int]) {
        val xs = seq.toList
        val more = xs.length
        ensureSize(length + more)
        for (i <- (length + more - 1) to (n + more - 1) by -1) {
            buf.put(i, buf.get(i - more))
        }
        var i = n
        xs.foreach { x =>
            buf.put(i, x)
            i += 1
        }
        buf.position(length + more)
    }

    def remove(n: Int) = {
        val result = apply(n)
        val savepos = buf.position - 1
        val newbuf = createIntBuffer(buf.capacity)
        for (i <- 0 until n)
            newbuf.put(buf.get(i))
        for (i <- n + 1 until length)
            newbuf.put(buf.get(i))
        newbuf.position(savepos)
        buf = newbuf
        result
    }

    protected final def ensureSize(n: Int) {
        if (n > buf.capacity) {
            var newsize = buf.capacity * 2
            while (n > newsize) newsize *= 2
            realloc(newsize)
        }
    }

    def sizeHint(len: Int) {
        if (len > buf.capacity && len > 0)
            realloc(len)
    }

    protected def realloc(len: Int) {
        val savepos = buf.position
        val newbuf = createIntBuffer(len)
        buf.rewind()
        newbuf.put(buf)
        newbuf.position(savepos)
        buf = newbuf
    }

    final val sizeof = 4
    def bytes() = length * sizeof
}

object IntBufferBuffer {
    implicit def IntBufferBuffer2IntBuffer(ibb: IntBufferBuffer) = ibb.buf
}

/**
 * @see FloatBufferBuffer
 */
class ShortBufferBuffer(protected val initialSize: Int)
    extends Buffer[Short]
    with IndexedSeqOptimized[Short, Buffer[Short]] {

    // this wouldn't be hard to do by hand (without lwjgl)
    import org.lwjgl.BufferUtils.createShortBuffer

    protected var buf = createShortBuffer(initialSize)

    def this() = this(16)

    def clear() { 
        buf.rewind()
        buf.limit(buf.capacity)
    }
    def result() = { 
        val robuf = buf.asReadOnlyBuffer()
        robuf.limit(buf.position)
        robuf.rewind()
        robuf
    }
    def length() = { buf.position }

    def update (n: Int, elem: Short) { buf.put(n, elem) }
    def apply (n: Int) = { buf.get(n) }

    final def += (elem: Short): this.type = {
        ensureSize(length + 1)
        buf.put(elem)
        this
    }

    import simplex3d.math.intm._

    final def += (elem: inVec2i): this.type = {
        ensureSize(length + 2)
        buf.put(elem.x.toShort).put(elem.y.toShort);
        this
    }

    final def += (elem: inVec3i): this.type = {
        ensureSize(length + 3)
        buf.put(elem.x.toShort).put(elem.y.toShort).put(elem.z.toShort)
        this
    }

    final def +=: (elem: Short): this.type = {
        val newbuf = createShortBuffer(length + 1)
        newbuf.put(elem)
        buf.rewind()
        newbuf.put(buf)
        this
    }

    def insertAll(n: Int, seq: Traversable[Short]) {
        val xs = seq.toList
        val more = xs.length
        ensureSize(length + more)
        for (i <- (length + more - 1) to (n + more - 1) by -1) {
            buf.put(i, buf.get(i - more))
        }
        var i = n
        xs.foreach { x =>
            buf.put(i, x)
            i += 1
        }
        buf.position(length + more)
    }

    def remove(n: Int) = {
        val result = apply(n)
        val savepos = buf.position - 1
        val newbuf = createShortBuffer(buf.capacity)
        for (i <- 0 until n)
            newbuf.put(buf.get(i))
        for (i <- n + 1 until length)
            newbuf.put(buf.get(i))
        newbuf.position(savepos)
        buf = newbuf
        result
    }

    protected final def ensureSize(n: Int) {
        if (n > buf.capacity) {
            var newsize = buf.capacity * 2
            while (n > newsize) newsize *= 2
            realloc(newsize)
        }
    }

    def sizeHint(len: Int) {
        if (len > buf.capacity && len > 0)
            realloc(len)
    }

    protected def realloc(len: Int) {
        val savepos = buf.position
        val newbuf = createShortBuffer(len)
        buf.rewind()
        newbuf.put(buf)
        newbuf.position(savepos)
        buf = newbuf
    }

    final val sizeof = 2
    def bytes() = length * sizeof
}

object ShortBufferBuffer {
    implicit def ShortBufferBuffer2ShortBuffer(fbb: ShortBufferBuffer) = fbb.buf
}
