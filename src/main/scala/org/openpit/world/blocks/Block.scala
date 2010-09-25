package org.openpit.world.blocks

abstract class Block

case object Air extends Block
case class Solid(color: Int) extends Block
case class Grass() extends Block
case class Stone() extends Block
case class Glass() extends Block
case class Cobblestone() extends Block
