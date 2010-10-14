package org.openpit.world.blocks

import simplex3d.math.floatm.ConstVec3f

abstract class Block

case object Air extends Block
case class Solid(color: Int) extends Block
case object Grass extends Block
case object Stone extends Block
case object Glass extends Block
case object Water extends Block
case object Cobblestone extends Block
case object Sand extends Block
case class Noise(color : ConstVec3f) extends Block
