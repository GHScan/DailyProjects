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
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    glEnable(GL_DEPTH_TEST);
}

void onDisplay()
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glutSolidTeapot(2);

    glutSwapBuffers();
    detectGLError();
}

void onIdle()
{
    glRotatef(3.14/4, 0, 0, 1);

    glutPostRedisplay();
}

void onReshape(int w, int h)
{
    if (w < 1) w = 1;
    if (h < 1) h = 1;
    glViewport(0, 0, w, h);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(60, w / float(h), 0.5f, 10);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glTranslatef(0, 0, -5);
}

void onKeyUp(unsigned char k, int x, int y)
{
    exit(0);
}

int main(int argc, char *argv[])
{
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
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
