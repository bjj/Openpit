package org.openpit.ui

import org.newdawn.slick.openal._

object SoundEffect {
    val basepath = "/sounds/"

    lazy val KungFuPunch = loadSound("kung_fu_punch.ogg")
    lazy val Digging = loadSound("digging1.ogg")

    def loadSound(path: String) = {
        val resource = getClass.getResourceAsStream(basepath + path)
        AudioLoader.getAudio("OGG", resource)
    }

}
