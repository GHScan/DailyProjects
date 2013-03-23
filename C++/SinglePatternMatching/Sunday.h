
#ifndef SUNDAY_H
#define SUNDAY_H

#include <string.h>

class SundayFinder
{
public:
    SundayFinder(const char *pattern):
        m_pattern(pattern)
    {
        for (int i = 0; i < 256; ++i) m_jumpTable[i] = (int)m_pattern.size() + 1;
        for (int i = 0; i < (int)m_pattern.size(); ++i) {
            m_jumpTable[(int)m_pattern[i]] = (int)m_pattern.size() - i;
        }
    }
    int find(const char *src)
    {
        const char *pattern = m_pattern.c_str();
        const char *end = src + strlen(src) - m_pattern.size() + 1;
        int patternSize = (int)m_pattern.size();
        int r = 0;
        while (src < end) {
            if (strncmp(src, pattern, patternSize) == 0) ++r;
            src += m_jumpTable[(int)src[patternSize]];
        }
        return r;
    }
private:
    string m_pattern;
    int m_jumpTable[256];
};

#endif
