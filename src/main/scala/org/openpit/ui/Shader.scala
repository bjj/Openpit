package org.openpit.ui

import org.lwjgl.opengl.GL11._

import org.lwjgl.opengl.ARBFragmentShader._
import org.lwjgl.opengl.ARBShaderObjects._
import org.lwjgl.opengl.ARBShadingLanguage100._
import org.lwjgl.opengl.ARBVertexShader._
import org.lwjgl.BufferUtils._

import simplex3d.math.intm._
import simplex3d.math.floatm._
import simplex3d.math.floatm.FloatMath._

import org.openpit.util._
import ImplicitGL._

/**
 */
class Shader(vertexSrc: String, fragmentSrc: String) {
    lazy val programId = load()

    /**
     * Check the compile status of the given id.  Laboriously extract the
     * infoLog and raise it as an error.
     */
    private def checkError(id: Int, param: Int) {
        val result = createIntBuffer(1)
        glGetObjectParameterARB(id, param, result)
        if (result.get(0) == 0) {
            result.rewind()
            glGetObjectParameterARB(id, GL_OBJECT_INFO_LOG_LENGTH_ARB, result)
            val length = result.get(0)
            val infoLogBuf = createByteBuffer(length)
            result.rewind()
            glGetInfoLogARB(id, result, infoLogBuf)
            var infoLogArray = Array.ofDim[Byte](length)
            infoLogBuf.rewind()
            infoLogBuf.get(infoLogArray)
            var infoLog = new String(infoLogArray)
            error(infoLog)
        }
    }

    private def compile(id: Int, programId: Int, src: String) {
        val bytes = src.getBytes
        val buf = createByteBuffer(bytes.length)
        buf.put(bytes)
        buf.flip()
        glShaderSourceARB(id, buf)
        glCompileShaderARB(id)
        checkError(id, GL_OBJECT_COMPILE_STATUS_ARB)
        glAttachObjectARB(programId, id)
    }

    private def load()  = {
        val vertexId = glCreateShaderObjectARB(GL_VERTEX_SHADER_ARB)
        val fragmentId = glCreateShaderObjectARB(GL_FRAGMENT_SHADER_ARB)
        val programId = glCreateProgramObjectARB()
        vertexSrc match {
            case null => {}
            case s    => compile(vertexId, programId, s)
        }
        fragmentSrc match {
            case null => {}
            case s    => compile(fragmentId, programId, s)
        }
        glLinkProgramARB(programId)
        checkError(programId, GL_OBJECT_LINK_STATUS_ARB)
        glDeleteObjectARB(vertexId)
        glDeleteObjectARB(fragmentId)
        programId
    }

    def bind() = glUseProgramObjectARB(programId)
    def unbind() = glUseProgramObjectARB(0)

    /**
     * Set a uniform variable by string name.  This is not very
     * efficient:  The location should be cached!
     */
    def set(name: String, v: Float) {
        val loc = glGetUniformLocationARB(programId, name)
        if (loc != -1) glUniform1fARB(loc, v)
    }
    def set(name: String, v: Vec3f) {
        val loc = glGetUniformLocationARB(programId, name)
        if (loc != -1) glUniform3fARB(loc, v.x, v.y, v.z)
    }
    def set(name: String, v: Vec4f) {
        val loc = glGetUniformLocationARB(programId, name)
        if (loc != -1) glUniform4fARB(loc, v.x, v.y, v.z, v.w)
    }
}
