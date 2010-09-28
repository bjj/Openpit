package org.openpit.test

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

import simplex3d.math.intm._

object Generators {
    val worldSize = choose(-100000, 100000)
    val genVec3i = for {x <- worldSize
                        y <- worldSize
                        z <- worldSize} yield Vec3i(x,y,z)

    implicit def arbVec3i: Arbitrary[Vec3i] = Arbitrary(genVec3i)
}

