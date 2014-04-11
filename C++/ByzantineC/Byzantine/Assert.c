#include <stdlib.h>
#include <stdio.h>

#include "Assert.h"
#include "Mem.h"
#include "Internal_OS.h"

int by_abort(const char *e, const char *message, const char *file, int line) {
    byOS_coloredFprintf(byColor_Red, stderr, "%s(%d): assert faile: %s, %s\n", file, line, e, message);

    char *s = byOS_trackStack(2);
    byOS_coloredFprintf(byColor_Red, stderr, "Stack traceback:\n%s", s);
    by_FREE(s);

    fflush(stdout);
    fflush(stderr);
    abort();
    return 0;
}
