
#include "stdafx.h"

#include <ctime>

#include <exception>

#include <gl/glew.h>
#include <gl/glut.h>

#include <windows.h>

#include "RenderWindow.h"

static RenderWindow *gWindow = nullptr;

GLuint gPboArray[2] = {0};
int gPboIndex = 0;
GLuint gTexture = 0;

static void SetupTextureAndPbo(int width, int height)
{
    if (gTexture != 0)
    {
        glDeleteTextures(1, &gTexture);
        glDeleteBuffers(sizeof(gPboArray) / sizeof(gPboArray[0]), gPboArray);
        gTexture = 0;
    }

    glGenTextures(1, &gTexture);
    glBindTexture(GL_TEXTURE_2D, gTexture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, nullptr);
    glBindTexture(GL_TEXTURE_2D, 0);

    glGenBuffers(sizeof(gPboArray) / sizeof(gPboArray[0]), gPboArray);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, gPboArray[0]);
    glBufferData(GL_PIXEL_UNPACK_BUFFER, width * height * 4, nullptr, GL_STREAM_DRAW);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, gPboArray[1]);
    glBufferData(GL_PIXEL_UNPACK_BUFFER, width * height * 4, nullptr, GL_STREAM_DRAW);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
}

static void OnDisplay()
{
    if (gWindow->RenderToBuffer())
    {
        int prevIndex = gPboIndex;
        gPboIndex = 1 - gPboIndex;

        glBindTexture(GL_TEXTURE_2D, gTexture);
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, gPboArray[prevIndex]);
        glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, gWindow->GetWidth(), gWindow->GetHeight(), GL_BGRA, GL_UNSIGNED_BYTE, nullptr);

        glClear(GL_COLOR_BUFFER_BIT);

        glColor4f(1, 1, 1, 1);
        glBegin(GL_QUADS);
        glNormal3f(0, 0, 1);
        glTexCoord2f(0.0f, 0.0f);   glVertex3f(-1.0f, -1.0f, 0.0f);
        glTexCoord2f(1.0f, 0.0f);   glVertex3f(1.0f, -1.0f, 0.0f);
        glTexCoord2f(1.0f, 1.0f);   glVertex3f(1.0f, 1.0f, 0.0f);
        glTexCoord2f(0.0f, 1.0f);   glVertex3f(-1.0f, 1.0f, 0.0f);
        glEnd();

        int size = gWindow->GetWidth() * gWindow->GetHeight() * 4;
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, gPboArray[gPboIndex]);
        glBufferData(GL_PIXEL_UNPACK_BUFFER, size, nullptr, GL_STREAM_DRAW);
        auto ptr = glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY);
        if (ptr)
        {
            gWindow->Render(reinterpret_cast<int*>(ptr));
            glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER); // release pointer to mapping buffer
        }

        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
        glBindTexture(GL_TEXTURE_2D, 0);
    }
    else
    {
        gWindow->Render(nullptr);
    }

    glutSwapBuffers();

    GLenum err = glGetError();
    if (err != GL_NO_ERROR)
    {
        GLubyte const *message = gluErrorString(err);
        throw std::exception(reinterpret_cast<char const *>(message));
    }
}

static clock_t gLastTick = clock();
static void OnIdle()
{
    clock_t now = clock();
    float elapse = float(now - gLastTick) / CLOCKS_PER_SEC;
    gLastTick = now;

    gWindow->Update(elapse);

    glutPostRedisplay();
}

static void OnReshape(int width, int height)
{
    width = max(width, 1);
    height = max(height, 1);

    glViewport(0, 0, width, height);
    glMatrixMode(GL_PROJECTION);
    gluOrtho2D(-1, 1, -1, 1);
    glMatrixMode(GL_MODELVIEW);

    gWindow->Resize(width, height);
    SetupTextureAndPbo(width, height);
}

static void OnKeyDown(unsigned char key, int x, int y)
{
    gWindow->KeyDown(key >= 'a' && key <= 'z' ? key - 'a' + 'A' : key);
}

static void OnKeyUp(unsigned char key, int x, int y)
{
    // esc
    if (key == 0x1b)
    {
        gWindow->Cleanup();
        exit(0);
    }
    gWindow->KeyUp(key >= 'a' && key <= 'z' ? key - 'a' + 'A' : key);
}

static void OnMouseFunc(int button, int state, int x, int y)
{
    float fx = x / float(gWindow->GetWidth()), fy = y / float(gWindow->GetHeight());
    if (state == GLUT_UP)
    {
        if (button == GLUT_LEFT_BUTTON)
        {
            gWindow->MouseButtonUp(RenderWindow::MouseButton::Left, fx, fy);
        }
        else if (button == GLUT_RIGHT_BUTTON)
        {
            gWindow->MouseButtonUp(RenderWindow::MouseButton::Right, fx, fy);
        }
    }
    else if (state == GLUT_DOWN)
    {
        if (button == GLUT_LEFT_BUTTON)
        {
            gWindow->MouseButtonDown(RenderWindow::MouseButton::Left, fx, fy);
        }
        else if (button == GLUT_RIGHT_BUTTON)
        {
            gWindow->MouseButtonDown(RenderWindow::MouseButton::Right, fx, fy);
        }
    }
}

static void OnMouseMove(int x, int y)
{
    gWindow->MouseMove(x / float(gWindow->GetWidth()), y / float(gWindow->GetHeight()));
}

RenderWindow::RenderWindow(char const *title, int width, int height)
: mTitle(title), mWidth(width), mHeight(height)
{
    gWindow = this;
}

RenderWindow::~RenderWindow()
{
    gWindow = nullptr;
}

void RenderWindow::Run(char *argv[], int argc)
{
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGBA);
    glutInitWindowSize(mWidth, mHeight);
    glutInitWindowPosition(
        (GetSystemMetrics(SM_CXSCREEN) - mWidth) / 2,
        (GetSystemMetrics(SM_CYSCREEN) - mHeight) / 2);

    glutCreateWindow(mTitle.c_str());

    cout << "GL Version: " << glGetString(GL_VERSION) << endl;
    cout << "GL Shading Language Version: " << glGetString(GL_SHADING_LANGUAGE_VERSION) << endl;

    glewInit();

    glutDisplayFunc(OnDisplay);
    glutIdleFunc(OnIdle);
    glutReshapeFunc(OnReshape);
    glutKeyboardFunc(OnKeyDown);
    glutKeyboardUpFunc(OnKeyUp);
    glutMouseFunc(OnMouseFunc);
    glutMotionFunc(OnMouseMove);

    typedef void(__stdcall *PFNWGLEXTSWAPCONTROLPROC) (int);
    PFNWGLEXTSWAPCONTROLPROC wglSwapIntervalEXT = NULL;
    if (strstr(const_cast<char*>(reinterpret_cast<char const*>(glGetString(GL_EXTENSIONS))), "WGL_EXT_swap_control") != nullptr)
    {
        wglSwapIntervalEXT = reinterpret_cast<PFNWGLEXTSWAPCONTROLPROC>(wglGetProcAddress("wglSwapIntervalEXT"));
        wglSwapIntervalEXT(0);
    }

    glEnable(GL_TEXTURE_2D);

    SetupTextureAndPbo(mWidth, mHeight);

    Setup();

    try
    {
        glutMainLoop();
    }
    catch (std::exception const &e)
    {
        cout << "Found exception : " << e.what() << endl;
        cin.get();
    }

    Cleanup();

    glDeleteTextures(1, &gTexture);
    glDeleteBuffers(sizeof(gPboArray) / sizeof(gPboArray[0]), gPboArray);
    gTexture = 0;
}