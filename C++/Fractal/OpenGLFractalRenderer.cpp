#include "Stdafx.h"

#include <string>

#include <gl/glew.h>

#include "FractalRenderer.h"

extern std::string ReadFile(char const *fileName);

class GLUniformLocation
{
public:
    GLUniformLocation()
        :mLocation(0)
    {
    }

    GLUniformLocation(GLuint program, char const *name)
        : mLocation(glGetUniformLocation(program, name))
    {
    }

    void Set(GLfloat value)
    {
        glUniform1f(mLocation, value);
    }

    void Set(GLdouble value)
    {
        glUniform1d(mLocation, value);
    }

    void Set(GLfloat v1, GLfloat v2, GLfloat v3, GLfloat v4)
    {
        glUniform4f(mLocation, v1, v2, v3, v4);
    }

    void Set(GLdouble v1, GLdouble v2, GLdouble v3, GLdouble v4)
    {
        glUniform4d(mLocation, v1, v2, v3, v4);
    }

private:
    GLint mLocation;
};

class OpenGLFractalRenderer : public IFractalRenderer
{
public:
    OpenGLFractalRenderer() :
        mMandelbrotProgram(CreateGLProgram("rect.vert", "mandelbrot.frag")),
        mJuliasetProgram(CreateGLProgram("rect.vert", "julia.frag"))
    {
        mMandelbrotRange = GLUniformLocation(mMandelbrotProgram, "gRange");
        mMandelbrotMaxIteration = GLUniformLocation(mMandelbrotProgram, "gMaxIteration");
        mMandelbrotWindowSize = GLUniformLocation(mMandelbrotProgram, "gWindowSize");\

        mJuliasetRange = GLUniformLocation(mJuliasetProgram, "gRange");
        mJuliasetMaxIteration = GLUniformLocation(mJuliasetProgram, "gMaxIteration");
        mJuliasetWindowSize = GLUniformLocation(mJuliasetProgram, "gWindowSize");
    }

    ~OpenGLFractalRenderer()
    {
        glDeleteProgram(mMandelbrotProgram);
        glDeleteProgram(mJuliasetProgram);
    }

    virtual bool RenderToBuffer() override 
    {
        return false;
    }

    virtual void ResetBuffer(int width, int height) override
    {
    }

    virtual void RenderMandelbrot(
        int *buffer, int width, int height, int maxIteration,
        TFloat minX, TFloat maxX, TFloat minY, TFloat maxY) override
    {
        glUseProgram(mMandelbrotProgram);

        mMandelbrotRange.Set(minX, maxX, minY, maxY);
        mMandelbrotMaxIteration.Set((TFloat)maxIteration);
        mMandelbrotWindowSize.Set((TFloat)width, (TFloat)height, 0, 0);

        glBegin(GL_TRIANGLE_STRIP);
        glVertex2f(-1, 1);
        glVertex2f(1, 1);
        glVertex2f(-1, -1);
        glVertex2f(1, -1);
        glEnd();

        glUseProgram(0);
    }

    virtual void RenderJuliaSet(
        int *buffer, int width, int height, int maxIteration, TFloat cx, TFloat cy,
        TFloat minX, TFloat maxX, TFloat minY, TFloat maxY) override
    {
        glUseProgram(mJuliasetProgram);

        mJuliasetRange.Set(minX, maxX, minY, maxY);
        mJuliasetMaxIteration.Set((TFloat)maxIteration);
        mJuliasetWindowSize.Set((TFloat)width, (TFloat)height, cx, cy);

        glBegin(GL_TRIANGLE_STRIP);
        glVertex2f(-1, 1);
        glVertex2f(1, 1);
        glVertex2f(-1, -1);
        glVertex2f(1, -1);
        glEnd();

        glUseProgram(0);
    }

private:
    static GLuint CreateGLProgram(char const *vsName, char const *fsName)
    {
        auto vs = CreateGLShader(GL_VERTEX_SHADER, vsName);
        auto fs = CreateGLShader(GL_FRAGMENT_SHADER, fsName);

        auto program = glCreateProgram();
        glAttachShader(program, vs);
        glAttachShader(program, fs);

        glLinkProgram(program);

        return program;
    }

    static GLuint CreateGLShader(GLenum shaderType, char const *fileName)
    {
        auto shader = glCreateShader(shaderType);

        auto source = ReadFile(fileName);
        char const *sourceArray[] = {
#if USE_DOUBLE
            "#version 400 compatibility\n",
            "#define USE_DOUBLE  1\n",
#else
            "",
#endif
            source.c_str(),
        };
        glShaderSource(shader, sizeof(sourceArray) / sizeof(sourceArray[0]), sourceArray, nullptr);
        glCompileShader(shader);

        GLint len;
        glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &len);
        if (len > 0)
        {
            std::string log(len, 0);
            glGetInfoLogARB(shader, len, &len, &log[0]);
            if (len > 0)
            {
                cout << "Compile shader log: [" << fileName << "]\n" << log << endl;
            }
        }

        GLint compiled;
        glGetObjectParameterivARB(shader, GL_COMPILE_STATUS, &compiled);
        if (!compiled)
        {
            throw std::exception("Failed to compile shader!!");
        }

        return shader;
    }

private:
    GLuint mMandelbrotProgram;
    GLUniformLocation mMandelbrotRange;
    GLUniformLocation mMandelbrotMaxIteration;
    GLUniformLocation mMandelbrotWindowSize;
    GLuint mJuliasetProgram;
    GLUniformLocation mJuliasetRange;
    GLUniformLocation mJuliasetMaxIteration;
    GLUniformLocation mJuliasetWindowSize;
};

extern IFractalRenderer* CreateOpenGLFractalRenderer()
{
    return new OpenGLFractalRenderer();
}