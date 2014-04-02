
#ifndef COMMON_H
#define COMMON_H

#include <assert.h>

#include <vector>
#include <string>
#include <exception>
#include <fstream>

struct Token
{
    int type;
    std::string value;
    int line : 22;
    int col : 10;
};

class MyException:
    public std::exception
{
public:
    MyException(const char *file, int line, const char *exp, const std::string& pfname, const Token& t)
    {
        char buf[256];
        sprintf(buf, "%s(%d): %s ========== %s(%d,%d) - %s", file, line, exp, pfname.c_str(), t.line, t.col, t.value.c_str());
        m_s = buf;
#if !defined(NDEBUG)
        cout << m_s << endl;
        assert(0);
#endif
    }
    const char* what() const throw() { return m_s.c_str();}
private:
    std::string m_s;
};
#define PARSE_ASSERT(b) if (b); else throw MyException(__FILE__, __LINE__, #b, m_fname, m_tokens[m_curToken])
#define ASSERT assert

template<int>
const char* matchLexeme(const char* src);

template<int base, int n>
struct RLexemeMarcher
{
    static const char* match(const char* src, int &t)
    {
        t = base + n;
        if (const char *p = matchLexeme<base + n>(src)) return p;
        return RLexemeMarcher<base, n - 1>::match(src, t);
    }
};
template<int base>
struct RLexemeMarcher<base, 0>
{
    static const char* match(const char* src, int &t)
    {
        t = base;
        if (const char *p = matchLexeme<base>(src)) return p;
        return NULL;
    }
};

class LexicalAnalysiser
{
protected:
    template<int tt1, int tt2>
    void lexicalAnalysis(const std::string& fname, const std::string& _src, bool enableComment)
    {
        m_fname = fname;
        m_curToken = 0;
        m_tokens.clear();
        int line = 1;
        const char *lineHead = _src.c_str();
        bool isComment = false;
        for (const char *src = _src.c_str();;) {
            while (isspace(src[0])) {
                if (src[0] == '\n') {
                    ++line;
                    isComment = false;
                    lineHead = src + 1;
                }
                ++src;
            }
            if (src[0] == 0) break;

            if (src[0] == '/' && src[1] == '/' && enableComment) {
                isComment = true;
                ++src;
            }
            if (isComment) {
                ++src;
                continue;
            }

            int type;
            const char *end = RLexemeMarcher<tt1, tt2 - tt1>::match(src, type);
            ASSERT(end != NULL);
            Token t = {type, std::string(src, end), line, int(src - lineHead) + 1};
            m_tokens.push_back(t);
            src = end;
        }
    }
    bool hasMoreToken() const { return m_curToken < m_tokens.size(); }
    bool tryConsumeToken(int t, const std::string& v)
    {
        if (hasMoreToken() && 
                t == m_tokens[m_curToken].type && v == m_tokens[m_curToken].value) {
            ++m_curToken;
            return true;
        }
        return false;
    }
    bool tryConsumeToken(int t)
    {
        if (hasMoreToken() && t == m_tokens[m_curToken].type) {
            ++m_curToken;
            return true;
        }
        return false;
    }
    void consumeToken(int t, const std::string& v)
    {
        PARSE_ASSERT(tryConsumeToken(t, v));
    }
    void consumeToken(int t)
    {
        PARSE_ASSERT(tryConsumeToken(t));
    }
    void backupTokenPos() { m_backupTokenPos.push_back(m_curToken); }
    void restoreTokenPos() { m_curToken = m_backupTokenPos.back(); m_backupTokenPos.pop_back(); }
    void discardBackupTokenPos() { m_backupTokenPos.pop_back(); }
    const Token& getCurToken() const 
    { 
        ASSERT(m_curToken >= 0 && m_curToken < m_tokens.size());
        return m_tokens[m_curToken]; 
    }
    const Token& getPreviewToken() const 
    {
        ASSERT(m_curToken >= 1 && m_curToken <= m_tokens.size());
        return m_tokens[m_curToken - 1];
    }

protected:
    int m_curToken;
    std::string m_fname;
    std::vector<Token> m_tokens;
    std::vector<int> m_backupTokenPos;
};

inline std::string readFile(const std::string &fname)
{
    std::ifstream fi(fname.c_str());
    std::string r;
    for (std::string line; getline(fi, line); r += line + '\n');
    return r;
}

#endif
