
#include "stdafx.h"

#include <ctime>

#include <exception>

#include <gl/glut.h>

#include <windows.h>

#include "RenderWindow.h"

static RenderWindow *gWindow = nullptr;

static void OnDisplay()
{
    glClear(GL_COLOR_BUFFER_BIT);

    gWindow->Paint();
    glDrawPixels(gWindow->GetWidth(), gWindow->GetHeight(), GL_BGRA_EXT, GL_UNSIGNED_BYTE, gWindow->GetFrameBufferPtr());

    glutSwapBuffers();

    GLenum err = glGetError();
    if (err != GL_NO_ERROR)
    {
        throw std::exception(reinterpret_cast<char const *>(gluErrorString(err)));
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
: mTitle(title), mWidth(width), mHeight(height), mFrameBuffer(mWidth * mHeight * 4)
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

    glutDisplayFunc(OnDisplay);
    glutIdleFunc(OnIdle);
    glutReshapeFunc(OnReshape);
    glutKeyboardFunc(OnKeyDown);
    glutKeyboardUpFunc(OnKeyUp);
    glutMouseFunc(OnMouseFunc);
    glutMotionFunc(OnMouseMove);

    glClearColor(0, 0, 0, 0);

    typedef void(__stdcall *PFNWGLEXTSWAPCONTROLPROC) (int);
    PFNWGLEXTSWAPCONTROLPROC wglSwapIntervalEXT = NULL;
    if (strstr(const_cast<char*>(reinterpret_cast<char const*>(glGetString(GL_EXTENSIONS))), "WGL_EXT_swap_control") != nullptr)
    {
        wglSwapIntervalEXT = reinterpret_cast<PFNWGLEXTSWAPCONTROLPROC>(wglGetProcAddress("wglSwapIntervalEXT"));
        wglSwapIntervalEXT(0);
    }

    glRasterPos2f(-1, 1);
    glPixelZoom(1, -1);

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
}