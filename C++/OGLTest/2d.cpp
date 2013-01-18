// vim:fileencoding=gbk

#include "pch.h"

#include <exception>

#include <gl/glut.h>

void detectGLError()
{
    GLenum err = glGetError();
    if (err != GL_NO_ERROR) {
        throw std::exception((const char *)gluErrorString(err));
    }
}

void setupEnv()
{
    glClearColor(0, 0, 0, 0);
}

void onDisplay()
{
    glClear(GL_COLOR_BUFFER_BIT);

    glBegin(GL_TRIANGLES);
    glColor3f(1, 0, 0);
    glVertex2f(0, 1);
    glColor3f(0, 1, 0);
    glVertex2f(-1, -1);
    glColor3f(0, 0, 1);
    glVertex2f(1, -1);
    glEnd();

    glutSwapBuffers();
    detectGLError();
}

void onIdle()
{
    glutPostRedisplay();
}

void onReshape(int w, int h)
{
    if (w < 1) w = 1;
    if (h < 1) h = 1;
    glViewport(0, 0, w, h);
    glMatrixMode(GL_PROJECTION);
    float fw = w / 400.0f, fh = h / 300.0f;
    gluOrtho2D(-fw, fw, -fh, fh);
    glMatrixMode(GL_MODELVIEW);
}

void onKeyUp(unsigned char k, int x, int y)
{
    exit(0);
}

int main(int argc, char *argv[])
{
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
    glutInitWindowPosition(100, 100);
    glutInitWindowSize(400, 300);

    glutCreateWindow("GLUT App");

    glutDisplayFunc(onDisplay);
    glutIdleFunc(onIdle);
    glutReshapeFunc(onReshape);
    glutKeyboardUpFunc(onKeyUp);

    setupEnv();

    try {
        glutMainLoop();
    } catch(const std::exception &e) {
        cout << "exception : " << e.what() << endl;
    }
}
