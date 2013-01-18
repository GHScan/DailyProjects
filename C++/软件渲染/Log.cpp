// vim: fileencoding=gbk
#include "pch.h"

#include <stdarg.h>
#include <cassert>

#include "Log.h"

#pragma warning(disable: 4996) // 'vsprintf' was declared deprecated

Log* Log::instance()
{
    static Log s_ins;
    return &s_ins;
}
void Log::addMsg(const char *fmt, ...)
{
    static char s_buf[512] = "";
    va_list args;
    va_start(args, fmt);
    vsprintf(s_buf, fmt, args);
    va_end(args);
    s_buf[sizeof(s_buf) - 1] = 0;
    m_msgs.push_back(s_buf);
    assert(m_msgs.size() < 512);
}
void Log::clearMsg()
{
    m_msgs.clear();
}
void Log::flushMsgToConsole()
{
    flushMsgToFile(stdout);
}
void Log::flushMsgToFile(const char *fname)
{
    FILE *f = fopen(fname, "w");
    flushMsgToFile(f);
    fclose(f);
}
void Log::flushMsgToFile(FILE *f)
{
    assert(f != NULL);
    for (int i = 0; i < (int)m_msgs.size(); ++i) { 
        fprintf(f, "[%3d] - %s\n", i + 1, m_msgs[i].c_str());
    }
    fprintf(f, "%s", "\n");
}
