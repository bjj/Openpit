package org.openpit.world

import simplex3d.math.intm._
import org.openpit.world.blocks._

object World {
    val width = 100
    val height = 100
    val depth = 20
    var blocks = Array.fill[Block](width, height, depth)(Air)

    def plain {
	for (x <- 0 until width; y <- 0 until height)
	    put(x, y, 10, Grass())
    }

    def foreach(s: (Vec3i, Block) => Unit) {
	for (z <- 0 until depth;
	     y <- 0 until height;
	     x <- 0 until width) s (Vec3i(x,y,z), at(x, y, z))
    }

    def at(x: Int, y: Int, z: Int) = blocks(x)(y)(z)

    def put(x: Int, y: Int, z: Int, b: Block) = {
	blocks(x)(y)(z) = b
    }
}
