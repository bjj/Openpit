package org.openpit.util

import org.lwjgl.opengl.GL11._

object glErrors extends Iterable[Int] {
    def iterator = new Iterator[Int] {
        var queue = 0

        def fill() {
            if (queue == 0) queue = glGetError()
        }
        def take() = {
            var result = queue
            queue = 0
            result
        }
        def hasNext() = { fill(); queue != 0 }
        def next() = { fill(); take() }
    }
}
