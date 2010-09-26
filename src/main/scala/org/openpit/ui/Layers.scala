package org.openpit.ui

import org.openpit.ui._
import org.lwjgl.opengl.GL11._

object TerrainLayer extends Layer3d(100, false) {
    def update() {
	val newList = Render.makeDisplayList(Render.renderOpaqueBlock)
	val oldList = displayList
	displayList = newList
	if (oldList != 0) glDeleteLists(oldList, 1)
    }
}

object GlassLayer extends Layer3d(900, true) {
    def update() {
	val newList = Render.makeDisplayList(Render.renderTranslucentBlock)
	val oldList = displayList
	displayList = newList
	if (oldList != 0) glDeleteLists(oldList, 1)
    }
}
