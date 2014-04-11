#ifndef INTERNAL_OS_H
#define INTERNAL_OS_H

#include <stdio.h>

#include "Type.h"

int     byOS_coloredFprintf(byColor color, FILE *file, const char *fmt, ...);
char*   byOS_trackStack(int skipTopFrames);

#endif
