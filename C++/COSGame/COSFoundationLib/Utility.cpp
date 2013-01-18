#include "stdafx.h"

#include <stdarg.h>
#include <ctime>

#include <iostream>
#include <fstream>

#include <boost/static_assert.hpp>

#include <windows.h>

#include "Utility.h"

namespace Scan
{

NativeLocale::NativeLocale()
{
    m_lastLocale = setlocale(LC_ALL, "");
}

NativeLocale::~NativeLocale()
{
    setlocale(LC_ALL, m_lastLocale);
}

void messageBox(const String& msg, const String& title)
{
    ::MessageBox(NULL, msg.c_str(), title.c_str(), MB_OK);
}

void breakDebuger()
{
    __asm int 3;
}

static unsigned long long _getCpuFrequency()
{
    unsigned long long freq = 0;
    BOOST_STATIC_ASSERT(sizeof(freq) == sizeof(LARGE_INTEGER));
    return ::QueryPerformanceFrequency((LARGE_INTEGER*)&freq) == TRUE ? freq : 1;
}

unsigned long long getCpuFrequency()
{
    static unsigned long long ls_freq = _getCpuFrequency();
    return ls_freq;
}

StringPrinter& StringPrinter::format(const char *fmt, ...)
{
    va_list arg;
    va_start(arg, fmt);

    int len = _vscprintf(fmt, arg) + 1;
    char *buf = new char[len];
    vsprintf_s(buf, len, fmt, arg);
    m_os.str(buf);
    delete[] buf;

    va_end(arg);

    return *this;
}

StringPrinter& StringPrinter::pushFormat(const char *fmt, ...)
{
    va_list arg;
    va_start(arg, fmt);

    int len = _vscprintf(fmt, arg) + 1;
    char *buf = new char[len];
    vsprintf_s(buf, len, fmt, arg);
    m_os << buf;
    delete[] buf;

    va_end(arg);

    return *this;
}

StringPrinter& StringPrinter::printToFile(const String& fileName)
{
    String s = getString();
    if (!s.empty()) CachedOutputFileManager::getSingletonPtr()->getFile(fileName) << s;
    return *this;
}

StringPrinter& StringPrinter::printToDebugger()
{
    String s = getString();
    if (!s.empty()) ::OutputDebugString(s.c_str());
    return *this;
}

StringPrinter& StringPrinter::printToStdout()
{
    String s = getString();
    if (!s.empty()) std::cout << s;
    return *this;
}

StringPrinter& StringPrinter::printToStderr()
{
    String s = getString();
    if (!s.empty()) std::cerr << s;
    return *this;
}

CachedOutputFileManager* CachedOutputFileManager::getSingletonPtr()
{
    static CachedOutputFileManager ls_ins;
    return &ls_ins;
}

std::ofstream& CachedOutputFileManager::getFile(const String& fileName)
{
    for (int i = 0; i < CACHED_OUTPUT_FILE_SIZE; ++i)
    {
        if (fileName == m_fileNames[i])
        {
            m_lastVisit[i] = clock();
            // 这个文件可能是没有打开的
            return m_files[i];
        }
    }

    clock_t minVisit = m_lastVisit[0];
    int minIdx = 0;
    for (int i = 0; i < CACHED_OUTPUT_FILE_SIZE; ++i)
    {
        if (m_fileNames[i].empty()) 
        {
            minIdx = i; break;
        }
        if (m_lastVisit[i] < minVisit)
        {
            minVisit = m_lastVisit[i];
            minIdx = i;
        }
    }

    m_files[minIdx].close();
    m_files[minIdx].clear();

    m_fileNames[minIdx] = fileName;
    {
        // 确保能打开中文文件
        NativeLocale local;
        // 其他原因打不开用户负责
        m_files[minIdx].open(fileName.c_str());
    }
    m_lastVisit[minIdx] = clock();

    return m_files[minIdx];
}

float Timer::getElapseSeconds() const
{
    unsigned long long counter = getElapseCounter();
    return float(counter) / getCpuFrequency();
}

unsigned long long Timer::getElapseCounter() const
{
    unsigned long long endCounter = 0;
    BOOST_STATIC_ASSERT(sizeof(endCounter) == sizeof(unsigned long long));
    ::QueryPerformanceCounter((LARGE_INTEGER*)&endCounter);
    return endCounter - m_beginCounter;
}

void Timer::reset()
{
    BOOST_STATIC_ASSERT(sizeof(m_beginCounter) == sizeof(unsigned long long));
    ::QueryPerformanceCounter((LARGE_INTEGER*)&m_beginCounter);
}

ScopeTimer::~ScopeTimer()
{
    if (m_s.empty()) return;
    if (m_print2File == NULL && !m_print2Debugger) return;
    
    String s = m_s + " : " + toString(getElapseSeconds()) + "\n";

    if (m_print2File != NULL) fprintf(m_print2File, "%s", s.c_str());
    if (m_print2Debugger) ::OutputDebugString(s.c_str());
}

}