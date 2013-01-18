// vim: fileencoding=gbk
#include "pch.h"

#include <stdio.h>

#include <limits>

#include "Util.h"

#pragma warning(disable : 4996) // 'strcpy' was declared deprecated

extern const float PI = 3.1415926535897932384626433832795f;
extern const float PI2 = 3.1415926535897932384626433832795f * 2;

extern const float EPSILON = 1e-5f;
extern const float EPSILON4 = 1e-4f;
extern const float EPSILON5 = 1e-5f;

extern const float MIN_FLOAT = std::numeric_limits<float>::min();
extern const float MAX_FLOAT = std::numeric_limits<float>::max();

bool solveEquations2(const float ls[4], const float rs[2], float ret[2])
{
    float det = ls[0] * ls[3] - ls[1] * ls[2];
    if (fequal(det, 0)) return false;
    float invDet = 1 / det;
    ret[0] = (ls[3] * rs[0] + -ls[1] * rs[1]) * invDet;
    ret[1] = (-ls[2] * rs[0] + ls[0] * rs[1]) * invDet;
    return true;
}

bool isFileExist(const char *fname)
{
    FILE *f = fopen(fname, "rb");
    if (f != NULL) {
        fclose(f);
        return true;
    }
    return false;
}

std::string getNotExistFileName(const char *fname)
{
    char name[512] = "";
    char ext[32] = "";
    strcpy(name, fname);
    strcpy(ext, strrchr(name, '.') + 1);
    strrchr(name, '.')[0] = 0;
    if (isFileExist(fname)) {
        char buf[512] = "";
        int i = 0;
        for (;;) {
            sprintf(buf, "%s_%d.%s", name, ++i, ext);
            if (!isFileExist(buf)) {
                sprintf(buf, "_%d", i);
                strcat(name, buf);
                break;
            }
        }
    }
    strcat(name, ".");
    strcat(name, ext);
    return name;
}
