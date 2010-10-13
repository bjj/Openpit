package org.openpit.ui

import org.lwjgl.Sys
import org.lwjgl.input.{Keyboard, Mouse}
import org.lwjgl.opengl.{Display, DisplayMode}
import org.lwjgl.opengl.GL11._
import org.lwjgl.util.glu._
import simplex3d.math.intm._
import simplex3d.math.floatm._

import org.openpit.util.ImplicitGL._
import org.openpit.util.glErrors


object Window {

    val framerate = 60

    var width = 1
    var height = 1
    var aspect = 1.0f

    // XXX I wanted an insertion-sorting collection based on Z depth
    // but I didn't find it offhand
    import org.openpit.ui.hud.Crosshair
    val layers = Array(TerrainLayer, SelectLayer, GlassLayer, Crosshair)

    def paint() {
        import console.FPS

        if (Display.isVisible()) {
            // Uncomment this sync to ignore the repaint speed
            //Display.sync(framerate)
            glClear(GL_COLOR_BUFFER_BIT |
                    GL_STENCIL_BUFFER_BIT |
                    GL_DEPTH_BUFFER_BIT)
            for (layer <- layers if layer.visible) layer.paint()
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
        for (err <- glErrors) {
            printf("GL ERROR: %s\n", GLU.gluErrorString(err))
        }
    }

    def resize(w: Int, h: Int) {
        width = w
        height = h
        aspect = (width toFloat) / height
    }

    def init() {
        //resize(800, 450)
        resize(1680, 1050)
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
        glFogf(GL_FOG_START, 50f)
        glFogf(GL_FOG_END, Projection.maxRenderDistance)
        glEnable(GL_FOG)

        for (layer <- layers) layer.start()
    }

    // XXX update should be in "Layers" or something along with layers collection
    def update() {
        import org.openpit.ui.Layer
        // XXX NullPointerException in glGenLists -- not thread safe??
        //for (layer <- layers) layer ! Layer.Update
        for (layer <- layers) layer.update()
    }
}
