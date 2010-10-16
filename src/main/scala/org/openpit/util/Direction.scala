package org.openpit.util

import simplex3d.math.intm._

object Direction {
    val `X+` = ConstVec3i(1,0,0)
    val `X-` = ConstVec3i(-1,0,0)
    val `Y+` = ConstVec3i(0,1,0)
    val `Y-` = ConstVec3i(0,-1,0)
    val `Z+` = ConstVec3i(0,0,1)
    val `Z-` = ConstVec3i(0,0,-1)

    val Up = `Z+`
    val Down = `Z-`
}

