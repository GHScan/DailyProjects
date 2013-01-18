// vim:fileencoding=gbk

#include "pch.h"

#include <ctime>

#include <exception>
#include <vector>

#include <gl/glut.h>

#include <windows.h>

#include "VirtualPlatform.h"

int g_w = 800, g_h = 600;
std::vector<char> g_buf;

void GL_detectError()
{
    GLenum err = glGetError();
    if (err != GL_NO_ERROR) {
        throw std::exception((const char *)gluErrorString(err));
    }
}

void GL_setup()
{
    glClearColor(0, 0, 0, 0);

    typedef void (__stdcall *PFNWGLEXTSWAPCONTROLPROC) (int);
    PFNWGLEXTSWAPCONTROLPROC wglSwapIntervalEXT = NULL;
    if (strstr((char*)glGetString(GL_EXTENSIONS),"WGL_EXT_swap_control")) {
        wglSwapIntervalEXT =
            (PFNWGLEXTSWAPCONTROLPROC)wglGetProcAddress("wglSwapIntervalEXT");
        wglSwapIntervalEXT(0); // 关闭垂直同步
    }

    glRasterPos2f(-1, 1);
    glPixelZoom(1, -1);

    setupScene();
}

void GL_cleanup()
{
    cleanupScene();
}

void GL_onDisplay()
{
    glClear(GL_COLOR_BUFFER_BIT);

    if (!g_buf.empty()) {
        onDrawBuffer(&g_buf[0], g_w, g_h, g_w * 4);
        glDrawPixels(g_w, g_h, GL_BGRA_EXT, GL_UNSIGNED_BYTE, &g_buf[0]);
    }

    glutSwapBuffers();
    GL_detectError();
}

void GL_onIdle()
{
    static clock_t s_lastTick = clock();
    clock_t now = clock();
    float elapse = (now - s_lastTick) / 1000.f;
    s_lastTick = now;
    onUpdate(elapse);

    glutPostRedisplay();
}

void GL_onReshape(int w, int h)
{
    if (w < 1) w = 1;
    if (h < 1) h = 1;
    glViewport(0, 0, w, h);
    glMatrixMode(GL_PROJECTION);
    gluOrtho2D(-1, 1, -1, 1);
    glMatrixMode(GL_MODELVIEW);

    g_w = w, g_h = h;
    g_buf.resize(g_w * g_h * 4);
}

void GL_onKeyDown(unsigned char k, int x, int y)
{
    if (k >= 'a' && k <= 'z') {
        k = k - 'a' + 'A'; // 做一个适配
    }

    onKeyDown(k);
}

void GL_onKeyUp(unsigned char k, int x, int y)
{
    if (k == 0x1b) { // esc
        GL_cleanup();
        exit(0);
    }
    if (k >= 'a' && k <= 'z') {
        k = k - 'a' + 'A'; // 做一个适配
    }

    onKeyUp(k);
}

void GL_onMouseFunc(int btn, int state, int x, int y)
{
    float fx = x / float(g_w), fy = y / float(g_h);
    if (state == GLUT_UP) {
        if (btn == GLUT_LEFT_BUTTON) {
            onMouseButtonUp(MOUSE_LBUTTON, fx, fy);
        }
        else if (btn == GLUT_RIGHT_BUTTON) {
            onMouseButtonUp(MOUSE_RBUTTON, fx, fy);
        }
    }
    else if (state == GLUT_DOWN) {
        if (btn == GLUT_LEFT_BUTTON) {
            onMouseButtonDown(MOUSE_LBUTTON, fx, fy);
        }
        else if (btn == GLUT_RIGHT_BUTTON) {
            onMouseButtonDown(MOUSE_RBUTTON, fx, fy);
        }
    }
}

void GL_onMouseMove(int x, int y)
{
    onMouseMove(x / float(g_w), y / float(g_h));
}

int main(int argc, char *argv[])
{
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
    glutInitWindowSize(g_w, g_h);
    glutInitWindowPosition(
            (GetSystemMetrics(SM_CXSCREEN) - g_w) / 2, 
            (GetSystemMetrics(SM_CYSCREEN) - g_h) / 2);

    glutCreateWindow("GLUT App");

    glutDisplayFunc(GL_onDisplay);
    glutIdleFunc(GL_onIdle);
    glutReshapeFunc(GL_onReshape);
    glutKeyboardFunc(GL_onKeyDown);
    glutKeyboardUpFunc(GL_onKeyUp);
    glutMouseFunc(GL_onMouseFunc);
    glutMotionFunc(GL_onMouseMove);

    GL_setup();

    try {
        glutMainLoop();
    } catch(const std::exception &e) {
        cout << "exception : " << e.what() << endl;
    }

    GL_cleanup();
}
