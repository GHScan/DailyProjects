#include "Config.h"

#if defined(by_LINUX)

#define _POSIX_SOURCE

#include "Assert.h"
#include "Mem.h"
#include "Internal_OS.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>

int byOS_coloredFprintf(byColor color, FILE *file, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    if (isatty(fileno(file))) {
        int code;
        switch (color) {
            case byColor_Red: code = 31; break;
            case byColor_Green: code = 32; break;
            case byColor_Blue: code = 34; break;
            case byColor_Black: code = 30; break;
            case byColor_White: code = 37; break;
            default: by_RASSERT(0, ""); break;
        }
        fprintf(file, "\x1b[01;40;%dm", code);
    } 

    int r = vfprintf(file, fmt, args);

    if (isatty(fileno(file))) {
        fprintf(file, "\x1b[0m");
    } 

    va_end(args);
    return r;
}

char* byOS_trackStack(int skipTopFrames) {
    const char *message = "Linux not support stack traceback...";
    char *s;
    by_ALLOC(s, strlen(message) + 1);
    strcpy(s, message);
    return s;
}

#endif
