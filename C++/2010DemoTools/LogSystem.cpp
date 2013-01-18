#include "StdAfx.h"

#include <iostream>

#include "LogSystem.h"
#include "Utility.h"
#include "PlatformDepends.h"

namespace Scan
{
    void LogFile_Console::log(const char* msg, const char *file, int line, const char *func)
    {
        OutStream os;
        std::cout << os.format("%s,%d,%s : %s", file, line, func, msg).getStream().str();
    }

    void LogFile_Console::close()
    {
        
    }

    void LogFile_VCDebug::log(const char* msg, const char *file, int line, const char *func)
    {
        printVCDebug(msg, file, line, func);
    }

    void LogFile_VCDebug::close()
    {

    }

    Logger::Logger(ILogFile *logFile, bool isAutoDeleteFile):
    m_logFile(logFile), m_isAutoDeleteFile(isAutoDeleteFile)
    {

    }

    Logger::~Logger(void)
    {
        if (m_isAutoDeleteFile)
        {
            safe_delete(m_logFile);
        }
    }

    void Logger::log(const char *msg, const char *file, int line, const char *func)
    {
        m_logFile->log(msg, file, line, func);
    }

    Logger* getConsoleLogger()
    {
        static Logger ls_logger(new LogFile_Console);
        return &ls_logger;
    }

    Logger* getVCDebugLogger()
    {
        static Logger ls_logger(new LogFile_VCDebug);
        return &ls_logger;
    }
}