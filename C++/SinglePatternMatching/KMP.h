
#ifndef KMP_H
#define KMP_H

class KMPFinder
{
public:
    KMPFinder(const char *pattern):
        m_pattern(pattern)
    {
        m_prefixTable.resize(m_pattern.size(), -1);
        for (int i = 0; i < (int)m_prefixTable.size(); ++i) {
            int j = i - 1;
            while (j != -1 && m_pattern[m_prefixTable[j] + 1] != m_pattern[i]) j = m_prefixTable[j];
            if (j == -1) m_prefixTable[i] = -1;
            else m_prefixTable[i] = m_prefixTable[j] + 1;
        }
    }
    int find(const char *src)
    {
        const char *pattern = m_pattern.c_str();
        int patternSize = (int)m_pattern.size();
        const int *prefixTable = &m_prefixTable[0];

        int r = 0;
        int i = 0;
        while (*src) {
            if (*src == pattern[i]) ++i, ++src;
            else {
                if (i == patternSize) ++r;
                if (i > 0) i = prefixTable[i - 1] + 1;
                else ++src;
            }
        }
        if (i == patternSize) ++r;
        return r;
    }
    void printPrefixTable()
    {
        for (int i = 0; i < (int)m_prefixTable.size(); ++i) printf("%d,", m_prefixTable[i]);
        puts("");
    }
private:
    string m_pattern;
    vector<int> m_prefixTable;
};

#endif
