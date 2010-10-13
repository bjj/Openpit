package org.openpit.world.blocks

import simplex3d.math.floatm.ConstVec3f

abstract class Block

case object Air extends Block
case class Solid(color: Int) extends Block
case class Grass() extends Block
case class Stone() extends Block
case class Glass() extends Block
case class Water() extends Block
case class Cobblestone() extends Block
case class Sand() extends Block
case class Noise(color : ConstVec3f) extends Block
