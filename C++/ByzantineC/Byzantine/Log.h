#ifndef LOG_H
#define LOG_H

typedef int (*byLoggerT)(const char *message, const char *file, int line);

extern int g_byLogger_messageSuppressed;
extern byLoggerT g_byLogger_messageLogger;
extern byLoggerT g_byLogger_errorLogger;

byLoggerT   byLogger_installMessageLogger(byLoggerT logger);
byLoggerT   byLogger_installErrorLogger(byLoggerT logger);
void        byLogger_suppressMessage(int suppress);
#define     by_LOG_ERROR(message)      ((void)g_byLogger_errorLogger(message, __FILE__, __LINE__))
#define     by_LOG_MESSAGE(message)    ((void)(g_byLogger_messageSuppressed || g_byLogger_messageLogger(message, __FILE__, __LINE__)))

#endif
