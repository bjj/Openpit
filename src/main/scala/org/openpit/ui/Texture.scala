package org.openpit.ui

import scala.collection.mutable.HashMap

import java.io.File;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder._;
import java.nio.IntBuffer;
import java.awt.Transparency;
import java.awt.color.ColorSpace;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.ComponentColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.Raster;
import javax.imageio.ImageIO;
import java.util.Hashtable; // only to instantiate BufferedImage

import org.lwjgl.opengl.GL11._
import org.lwjgl.util.glu.GLU

class Texture(val id:Int, val width:Int, val height:Int) {
    def bind() {
        if (Texture.lastTextureBound != this) {
            glBindTexture(GL_TEXTURE_2D, id)
            Texture.lastTextureBound = this
        }
    }
    def unbind() {
        glBindTexture(GL_TEXTURE_2D, 0)
        Texture.lastTextureBound = null
    }
}

object Texture {
    val texturePath = "/textures/"

    lazy val GrassTop = loadGrid("terrain.png", 0)
    lazy val GrassSide = loadGrid("terrain.png", 2)
    lazy val Stone = loadGrid("terrain.png", 16)

    lazy val Terrain = load("terrain.png")
    lazy val Crosshair = load("crosshair.png")

    private var lastTextureBound: Texture = null

    private val images = new HashMap[String, BufferedImage]

    lazy val GL_RGBA_ColorModel = {
        new ComponentColorModel(ColorSpace.getInstance(ColorSpace.CS_sRGB),
            Array(8,8,8,8),
            true,
            false,
            Transparency.TRANSLUCENT,
            DataBuffer.TYPE_BYTE);
    }

    def loadImage(path: String) = {
        try {
            images(path)
        } catch {
            case _ =>
                var resource = getClass.getResource(texturePath + path)
                println("reading image " + resource.toString)
                val orig = ImageIO.read(resource)
                // Copy this into another image with GL byte ordering
                val raster = Raster.createInterleavedRaster(
                                    DataBuffer.TYPE_BYTE,
                                    orig.getWidth, orig.getHeight,
                                    4, // bytes per pixel
                                    null)
                val image = new BufferedImage(GL_RGBA_ColorModel,
                                    raster, false, new Hashtable())
                // This is much faster than us repacking by hand
                image.getGraphics.drawImage(orig, 0, 0, null)
                images(path) = image
                image
        }
    }

    val useMipmaps = false

    def load(path: String, x: Int, y: Int, width: Int, height: Int) = {
        val image = loadImage(path)
        val bpp = image.getColorModel().getPixelSize() / 8

        // Copy region into scratch buffer
        val generic_data = image.getRaster().getDataBuffer()
        val data = generic_data.asInstanceOf[DataBufferByte].getData()
        val scratch = ByteBuffer.allocateDirect(width * height * bpp) order nativeOrder
        for (row <- 0 until height) {
            scratch.put(data, (x + (y + row) * image.getWidth) * bpp,
                              width * bpp)
        }
        scratch.rewind()

        val id = allocTextureId()

        glBindTexture(GL_TEXTURE_2D, id)
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
            if (useMipmaps) GL_NEAREST_MIPMAP_LINEAR else GL_NEAREST)
        if (useMipmaps) {
            GLU.gluBuild2DMipmaps(GL_TEXTURE_2D, 4, width, height,
                if (bpp == 4) GL_RGBA else GL_RGB, GL_UNSIGNED_BYTE, scratch)
        } else {
            glTexImage2D(GL_TEXTURE_2D, 0, 4, width, height, 0,
                if (bpp == 4) GL_RGBA else GL_RGB, GL_UNSIGNED_BYTE, scratch)
        }
        new Texture(id, width, height)
    }

    def load(path: String): Texture = {
        val image = loadImage(path)
        load(path, 0, 0, image.getWidth, image.getHeight)
    }

    def loadGrid(path: String, n: Int) = {
        val dim = 16
        val row = n / dim
        val col = n % dim
        val image = loadImage(path)
        val size = image.getWidth / dim
        load(path, col * size, row * size, size, size)
    }

    def allocTextureId() = {
        val idbuf = ByteBuffer.allocateDirect(4) order nativeOrder()
        glGenTextures(idbuf asIntBuffer)
        idbuf.get(0)
    }

    def unbind() {
        glBindTexture(GL_TEXTURE_2D, 0)
        Texture.lastTextureBound = null
    }
}
