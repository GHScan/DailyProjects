#include <pch.h>

#include <gl/glaux.h>
#include <gl/glut.h>

#include <windows.h>

#include <io.h>

#pragma comment(lib, "glaux")

#include "util.h"

void sleep(float seconds)
{
    ::Sleep(seconds * 1000);
}

bool loadTexture(const char *fname, bool mipmap)
{
    if (_access(fname, 0) != 0) return false;

    AUX_RGBImageRec* rec = auxDIBImageLoad(fname);
    for (int lbegin = 0, lend = rec->sizeY - 1; lbegin < lend; ++lbegin, --lend) {
        char *data1 = (char*)rec->data + lbegin * rec->sizeX * 3;
        char *data2 = (char*)rec->data + lend * rec->sizeX * 3;
        char tmp[3];
        for (int i = 0; i < rec->sizeX; ++i) {
            memcpy(tmp, data1, 3);
            memcpy(data1, data2, 3);
            memcpy(data2, tmp, 3);
            data1 += 3;
            data2 += 3;
        }
    }

    if (mipmap) {
        gluBuild2DMipmaps(
                GL_TEXTURE_2D, 3, rec->sizeX, rec->sizeY, 
                GL_RGB, GL_UNSIGNED_BYTE, rec->data);
    }
    else {
        glTexImage2D(
                GL_TEXTURE_2D, 0, 3, rec->sizeX, rec->sizeY, 
                0, GL_RGB, GL_UNSIGNED_BYTE, rec->data);
    }

    free(rec->data);
    free(rec);
    return true;
}

void drawTexQuad(GLuint tid, float x1, float y1, float x2, float y2)
{
    glBindTexture(GL_TEXTURE_2D, tid);
    glBegin(GL_QUADS);
    glTexCoord2f(0, 1); glVertex2f(x1, y1);
    glTexCoord2f(1, 1); glVertex2f(x2, y1);
    glTexCoord2f(1, 0); glVertex2f(x2, y2);
    glTexCoord2f(0, 0); glVertex2f(x1, y2);
    glEnd();
}
