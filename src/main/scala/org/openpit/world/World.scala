package org.openpit.world

import simplex3d.math.intm._
import org.openpit.world.blocks._
import org.openpit.util._

object World extends Octree[Block] {

    def generate() {
        plain()
    }

    def plain() {
        // some random test objects
        for (z <- 5 to 10; x <- z until 100-z; y <- z until 100-z)
            this(x, y, z) = Grass()
        this(10,10,11) = Stone()
        this(12,10,13) = Stone()
        this(12,10,18) = Stone()
        for (x <- 20 to 30) this(x, 20, 11) = Cobblestone()
        for (x <- 22 to 28) this(x, 20, 12) = Cobblestone()
        for (x <- 20 to 30) this(x, 50, 11) = Stone()
        for (x <- 22 to 28) this(x, 50, 12) = Stone()
        for (y <- 20 until 30; z <- 11 until 15)
            this(15, y, z) = Glass()
    }

    def put(x: Int, y: Int, z: Int, b: Block) = {
        this(Vec3i(x,y,z)) = b
    }
}
