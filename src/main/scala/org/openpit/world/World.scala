package org.openpit.world

import simplex3d.math.intm._
import org.openpit.world.blocks._
import org.openpit.util._

object World extends Octree[Block](Vec3i(0,0,0), 2) {

    def plain {
	for (z <- 5 to 10; x <- z until 100-z; y <- z until 100-z)
	    put(x, y, z, Grass())
    }

    def put(x: Int, y: Int, z: Int, b: Block) = {
	this(Vec3i(x,y,z)) = b
    }
}
