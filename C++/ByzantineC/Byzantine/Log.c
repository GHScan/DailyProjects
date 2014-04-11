#include <stdio.h>

#include "Assert.h"
#include "Log.h"
#include "Internal_OS.h"

static int _byLogger_stdout(const char *message, const char *file, int line) {
    return fprintf(stdout, "%s(%d): %s\n", file, line, message);
}

static int _byLogger_stderr(const char *message, const char *file, int line) {
    return byOS_coloredFprintf(byColor_Red, stderr, "%s(%d): %s\n", file, line, message);
}

int g_byLogger_messageSuppressed = 0;
byLoggerT g_byLogger_messageLogger = _byLogger_stdout;
byLoggerT g_byLogger_errorLogger = _byLogger_stderr;

byLoggerT byLogger_installMessageLogger(byLoggerT logger) {
    by_RASSERT(logger != NULL, "");

    byLoggerT old = g_byLogger_messageLogger;
    g_byLogger_messageLogger = logger;
    return old;
}

byLoggerT byLogger_installErrorLogger(byLoggerT logger) {
    by_RASSERT(logger != NULL, "");

    byLoggerT old = g_byLogger_errorLogger;
    g_byLogger_errorLogger = logger;
    return old;
}

void byLogger_suppressMessage(int suppress) {
    g_byLogger_messageSuppressed = suppress;
}
