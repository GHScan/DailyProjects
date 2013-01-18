#pragma once

#include "CompileEnvironment.h"

namespace Scan
{
    struct ILogFile
    {
        virtual ~ILogFile() = 0 {}
        virtual void log(const char* msg, const char *file, int line, const char *func) = 0;
        virtual void close() = 0;
    };

    class LogFile_Console:
        public ILogFile
    {
    public:
        LogFile_Console(){}
        virtual void log(const char* msg, const char *file, int line, const char *func);
        virtual void close();

    private:
        LogFile_Console(const LogFile_Console&);
        LogFile_Console& operator = (const LogFile_Console&);
    };

    class LogFile_VCDebug:
        public ILogFile
    {
    public:
        LogFile_VCDebug(){}
        virtual void log(const char* msg, const char *file, int line, const char *func);
        virtual void close();

    private:
        LogFile_VCDebug(const LogFile_VCDebug&);
        LogFile_VCDebug& operator = (const LogFile_VCDebug&);
    };

    class Logger
    {
    public:
        Logger(ILogFile *logFile, bool isAutoDeleteFile = true);
        ~Logger(void);

        void log(const char *msg, const char *file, int line, const char *func);

    private:
        Logger(const Logger&);
        Logger& operator = (const Logger&);

    private:
        bool         m_isAutoDeleteFile;
        ILogFile    *m_logFile;
    };

    Logger* getConsoleLogger();
    Logger* getVCDebugLogger();

#define SCAN_LOG(msg)  getVCDebugLogger()->log(msg, __FILE__, __LINE__, __FUNCTION)
#define SCAN_LOG_ASSERT(b, msg)  if (!b) { LOG(msg); } else {}
}