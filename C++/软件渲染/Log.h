// vim: fileencoding=gbk

#ifndef LOG_H
#define LOG_H

#include <vector>
#include <string>

class Log
{
public:
    static Log* instance();
    void addMsg(const char *fmt, ...);
    void clearMsg();
    void flushMsgToConsole();
    void flushMsgToFile(const char *fname);
    void flushMsgToFile(FILE *f);
private:
    std::vector<std::string>    m_msgs;
};

#endif // #ifndef LOG_H
