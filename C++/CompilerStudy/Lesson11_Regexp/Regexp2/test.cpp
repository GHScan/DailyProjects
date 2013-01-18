#include "pch.h" 

#include <time.h>
#include <string.h>

#include "RegParser.h"
#include "NFAMatch.h"
#include "DFAMatch.h"

#ifdef _DEBUG
#define PERF_LOOP 100
#else
#define PERF_LOOP 3000000
#endif

class Timer
{
public:
    Timer(const std::string& name): m_name(name), m_start(clock()){}
    ~Timer()
    {
        cout << "Timer : " << m_name << " - " << float(clock() - m_start) / CLOCKS_PER_SEC << endl;
    }
private:
    std::string m_name;
    clock_t m_start;
};

struct IReg
{
    virtual ~IReg(){}
    virtual bool compile(const std::string& reg) = 0;
    virtual bool match(const std::string& src) = 0;
};
typedef std::shared_ptr<IReg> RegPtr;
class DFAReg:
    public IReg
{
public:
    DFAReg(){}
    DFAReg(DFAInstancePtr dfa): m_dfa(dfa){}
    bool compile(const std::string& reg)
    {
        RegParser parser(reg);
        return ::compile(m_dfa, parser.getNode());
    }
    bool match(const std::string& src) 
    {
        return ::match(m_dfa, src);
    }
    int optimize()
    {
        m_dfa->optimize();
        return getStateCount();
    }
    int getStateCount() { return m_dfa->getStateCount();}
private:
    DFAInstancePtr m_dfa;
};
class NFAReg:
    public IReg
{
public:
    bool compile(const std::string& reg)
    {
        RegParser parser(reg);
        return ::compile(m_nfa, parser.getNode());
    }
    bool match(const std::string& src) 
    {
        return ::match(m_nfa, src);
    }
    DFAReg* toDFA() 
    {
        DFAInstancePtr dfa;
        convertToDFA(dfa, m_nfa);
        return new DFAReg(dfa);
    }
private:
    NFANodePtr m_nfa;
};

RegPtr createReg(const std::string& name, const std::string& pattern)
{
    if (name == "NFA") {
        NFAReg *p = new NFAReg();
        p->compile(pattern);
        return RegPtr(p);
    }
    else if (name == "DFA") {
        DFAReg *p = new DFAReg();
        p->compile(pattern);
        printf("(DFA: %d)\n", p->getStateCount());
        return RegPtr(p);
    }
    else if (name == "DFAop") {
        DFAReg *p = new DFAReg();
        p->compile(pattern);
        int os = p->getStateCount();
        printf("(DFA optimize: %d -> %d)\n", os, p->optimize());
        return RegPtr(p);
    }
    else if (name == "NFA2DFA") {
        NFAReg *p = new NFAReg();
        p->compile(pattern);
        DFAReg *pd = p->toDFA();
        printf("(DFA: %d)\n", pd->getStateCount());
        RegPtr _(p);
        return RegPtr(pd);
    }
    else if (name == "NFA2DFAop") {
        NFAReg *p = new NFAReg();
        p->compile(pattern);
        DFAReg *pd = p->toDFA();
        int os = pd->getStateCount();
        printf("(DFA optimize: %d -> %d)\n", os, pd->optimize());
        RegPtr _(p);
        return RegPtr(pd);
    }
    else return RegPtr();
}

void regTest1(const char* name)
{
    RegPtr reg(createReg(name, "\\a+://[^\\s]*"));
    assert(reg->match("http://abc"));
    assert(reg->match("ftp://"));
    assert(!reg->match("ftp:://"));
    assert(!reg->match("ftp:/"));
    assert(!reg->match("3ftp:/"));
}
void regTest2(const char* name)
{
    RegPtr reg(createReg(name, "\\d+\\.\\d+\\.\\d+\\.\\d+"));
    assert(reg->match("23.3.1.1"));
    assert(reg->match("1.0.0.1234"));
    assert(!reg->match("1.1.1"));
    assert(!reg->match(".1"));
    assert(!reg->match("123...3"));
}
void regTest3(const char* name)
{
    RegPtr reg(createReg(name, "[\\a_][\\w_]*|[1-9][0-9]*\\.?[0-9]*"));
    assert(reg->match("rew"));
    assert(reg->match("rew234"));
    assert(reg->match("_rew234"));
    assert(!reg->match("#rew234"));
    assert(!reg->match("rew@34"));
    assert(reg->match("234"));
    assert(reg->match("234.324"));
    assert(reg->match("234."));
    assert(!reg->match("2a34."));
    assert(!reg->match("234a"));
}
void regTest4(const char* name)
{
    RegPtr reg(createReg(name, "(a|b)*abb"));
    assert(reg->match("aabb"));
    assert(reg->match("bbbabb"));
    assert(reg->match("abababb"));
    assert(!reg->match("ab"));
    assert(!reg->match("aabbb"));
}
void regTest5(const char* name)
{
    RegPtr reg(createReg(name, "[\\a_][\\w_]*|[1-9][0-9]*\\.?[0-9]*"));
    Timer _t(std::string(name) + "," + TO_STRING(PERF_LOOP));
    for (int i = 0; i < PERF_LOOP; ++i) {
        reg->match("_rew234");
        reg->match("#rew234");
        reg->match("234.324");
        reg->match("2a34.");
        reg->match("12345454.32423423423");
        reg->match(".123123124489893048a");
        reg->match("_fdsjklfsdjklfj");
        reg->match("_fdsjklfsdjklfjfjdkslfjklsd#");
    }
}

void test(const char *name)
{
    void (*funcs[])(const char*) = {
        &regTest1, &regTest2, &regTest3, &regTest4, &regTest5,
    };
    for (int i = 0; i < sizeof(funcs) / sizeof(funcs[0]); ++i) funcs[i](name);
}

int main()
{
    test("NFA");
    test("DFA");
    test("NFA2DFA");
    test("DFAop");
    test("NFA2DFAop");
}
