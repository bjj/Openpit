package org.openpit.ui

import org.lwjgl.Sys
import org.lwjgl.input.{Keyboard, Mouse}
import org.lwjgl.opengl.{Display, DisplayMode}
import org.lwjgl.opengl.GL11._
import org.lwjgl.util.glu._
import simplex3d.math.intm._
import simplex3d.math.floatm._

import org.openpit.util._
import ImplicitGL._


object Window {

    val framerate = 60

    var width = 1
    var height = 1
    var aspect = 1.0f

    def paint() {
        import console.FPS

        if (Display.isVisible()) {
            // Uncomment this sync to ignore the repaint speed
            //Display.sync(framerate)
            glClear(GL_COLOR_BUFFER_BIT |
                    GL_DEPTH_BUFFER_BIT)
            for (layer <- Layer.all.sorted if layer.visible) layer.paint()
            Display.update()
            // Uncomment this update to measure the redraw speed
            //update()
            FPS ! FPS.Frame
            checkErrors()
        } else {
            Thread.sleep(100)
        }
    }

    def checkErrors() {
        glGetError() match {
            case 0 => Unit
            case err: Int =>
                error("GL ERROR: " + GLU.gluErrorString(err))
        }
    }

    def resize(w: Int, h: Int) {
        width = w
        height = h
        aspect = (width toFloat) / height
    }

    def init() {
        val desktop = Display.getDesktopDisplayMode()
        resize((desktop.getWidth() * 90 / 100) min 1680,
               (desktop.getHeight() * 90 / 100) min 1050)
        Display.setDisplayMode(new DisplayMode(width, height))
        Display.setTitle("Openpit")
        //Display.setVSyncEnabled(true)
        Display.setFullscreen(false)
        Display.create()
        println("Using GL_RENDERER: " + glGetString(GL_RENDERER))

        glEnable(GL_DEPTH_TEST)
        glEnable(GL_CULL_FACE)
        glEnable(GL_TEXTURE_2D)
        glShadeModel(GL_SMOOTH)
        glAlphaFunc(GL_GREATER, 0.1f)
        glEnable(GL_ALPHA_TEST)
        glClearColor(135f/255f, 205f/255f, 222f/255f, 1.0f)

        glFogi(GL_FOG_MODE, GL_LINEAR)
        glFog(GL_FOG_COLOR, Array(135f/255f, 205f/255f, 222f/255f, 1.0f))
        //glFog(GL_FOG_COLOR, Array(198f/255f, 215f/255f, 216f/255f, 1.0f))
        glFogf(GL_FOG_START, 0.65f * Projection.maxRenderDistance)
        glFogf(GL_FOG_END, Projection.maxRenderDistance)
        glEnable(GL_FOG)
    }

    def update() {
    }
}
