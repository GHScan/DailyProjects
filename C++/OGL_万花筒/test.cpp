// vim:fileencoding=gbk

#include "pch.h"

#include "util.h"

GLuint g_commTex;

const int MASK_TEX_CNT = 3;
GLuint g_maskTexs[MASK_TEX_CNT];
int g_curMaskTex;
int g_curMaskAlpha;

void detectGLError()
{
    GLenum err = glGetError();
    if (err != GL_NO_ERROR) {
        throw std::exception((const char *)gluErrorString(err));
    }
}

void buildRandomMaskTex(GLuint tid)
{
    glBindTexture(GL_TEXTURE_2D, tid);
    const int W = 16, H = 16;
    unsigned char data[W * H];
    unsigned char *p = data;
    for (int y = 0; y < H; ++y) {
        for (int x = 0; x < W; ++x) {
            *p++ = rand() % 256;
        }
    }
    glTexImage2D(GL_TEXTURE_2D, 0, 4, 
            W, H, 0, GL_ALPHA, GL_UNSIGNED_BYTE, data);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
}

void buildFanMaskTex(GLuint tid)
{
    glBindTexture(GL_TEXTURE_2D, tid);
    const int W = 400, H = 300;
    unsigned char data[W * H];
    unsigned char *p = data;
    for (int y = 0; y < H; ++y) {
        for (int x = 0; x < W; ++x) {
            float _x = x - W / 2;
            float _y = H / 2 - y;
            float _z = sqrt(_x * _x + _y * _y);
            *p++ = atan2f(_y / _z, _x / _z) / 6.28f * 255;
        }
    }
    glTexImage2D(GL_TEXTURE_2D, 0, 4, 
            W, H, 0, GL_ALPHA, GL_UNSIGNED_BYTE, data);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
}

void buildRainMaskTex(GLuint tid)
{
    glBindTexture(GL_TEXTURE_2D, tid);
    const int W = 32, H = 32;
    unsigned char data[W * H];
    unsigned char *p = data;
    for (int y = 0; y < H; ++y) {
        for (int x = 0; x < W; ++x) {
            float f = (x * H + y) / float(W * H);
            *p++ = 255 * f;
        }
    }
    glTexImage2D(GL_TEXTURE_2D, 0, 4, 
            W, H, 0, GL_ALPHA, GL_UNSIGNED_BYTE, data);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
}

void setupEnv()
{
    srand(time(NULL));

    glClearColor(0, 0, 0, 0);

    glGenTextures(1, &g_commTex);
    glBindTexture(GL_TEXTURE_2D, g_commTex);
    if (!loadTexture("1.bmp")) {
        throw std::exception("load 1.bmp failed!");
    }
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

    glGenTextures(2, g_maskTexs);
    buildRandomMaskTex(g_maskTexs[0]);
    buildFanMaskTex(g_maskTexs[1]);
    buildRainMaskTex(g_maskTexs[2]);

    glEnable(GL_TEXTURE_2D);

    detectGLError();
}

void onDisplay()
{
    glClear(GL_COLOR_BUFFER_BIT);

    drawTexQuad(g_commTex, -1, -1, 1, 1);

    {
        if (++g_curMaskAlpha >= 256) {
            g_curMaskAlpha = 0;
            if (++g_curMaskTex >= MASK_TEX_CNT) {
                g_curMaskTex = 0;
            }
        }

        glEnable(GL_ALPHA_TEST);
        glAlphaFunc(GL_GREATER, g_curMaskAlpha / 255.0f);
        glEnable(GL_BLEND);
        glBlendFunc(GL_DST_COLOR, GL_ZERO);

        drawTexQuad(g_maskTexs[g_curMaskTex], -1, -1, 1, 1);

        glDisable(GL_BLEND);
        glDisable(GL_ALPHA_TEST);
    }

    detectGLError();
    glutSwapBuffers();
}

void onIdle()
{
    sleep(0.015);
    glutPostRedisplay();
}

void onReshape(int w, int h)
{
    if (w < 1) w = 1;
    if (h < 1) h = 1;
    glViewport(0, 0, w, h);
    glMatrixMode(GL_PROJECTION);
    gluOrtho2D(-1, 1, -1, 1);
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
