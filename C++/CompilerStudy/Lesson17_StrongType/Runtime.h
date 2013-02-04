
#ifndef RUNTIME_H
#define RUNTIME_H

class Thread
{
public:
    Thread(): m_top(0)
    {
    }
    void pushFrame()
    {
        m_frameTops.push_back(m_top);
    }
    void popFrame()
    {
        m_top = m_frameTops.back();
        m_frameTops.pop_back();
    }

    int top()
    {
        return m_top;
    }
    int frameBottom() { return m_frameTops.empty() ? 0 : m_frameTops.back();}
    char *threadBase() { return &m_stackDatas[0];}
    char *frameBase() { return threadBase() + frameBottom();}
    void pushValue(int size)
    {
        m_top += size;
        m_stackDatas.resize(m_top);
    }
    void popValue(int size)
    {
        m_top -= size;
    }
private:
    vector<char> m_stackDatas;
    vector<int> m_frameTops;
    int m_top;
};

struct IFunction
{
    virtual ~IFunction(){}
    virtual void call(Thread* thread) = 0;
};
typedef shared_ptr<IFunction> FunctionPtr;

class ASTFunction:
    public IFunction
{
public:
    ASTFunction(StmtNodePtr stmt);
private:
    virtual void call(Thread* thread);
private:
    StmtNodePtr m_stmt;
};

template<typename RetT, typename ArgT0, typename ArgT1, typename ArgT2>
class CFunction3:
    public IFunction
{
private:
    typedef RetT(*FuncT)(ArgT0, ArgT1, ArgT2);
public:
    CFunction3(FuncT f): m_f(f){}
private:
    virtual void call(Thread* thread)
    {
        char *base = thread->frameBase();
        char *arg0 = (RetT*)base + 1;
        char *arg1 = (ArgT0*)arg0 + 1;
        char *arg2 = (ArgT1*)arg1 + 1;
        (RetT&)*base = m_f((ArgT0&)*arg0, (ArgT1&)*arg1, (ArgT2&)*arg2);
    }
private:
    FuncT m_f;
};
template<typename RetT>
class CFunction0:
    public IFunction
{
private:
    typedef RetT(*FuncT)();
public:
    CFunction0(FuncT f): m_f(f){}
private:
    virtual void call(Thread* thread)
    {
        char *base = thread->frameBase();
        (RetT&)*base = m_f();
    }
private:
    FuncT m_f;
};
template<typename RetT, typename ArgT0>
class CFunction1:
    public IFunction
{
private:
    typedef RetT(*FuncT)(ArgT0);
public:
    CFunction1(FuncT f): m_f(f){}
private:
    virtual void call(Thread* thread)
    {
        char *base = thread->frameBase();
        char *arg0 = (char*)((RetT*)base + 1);
        (RetT&)*base = m_f((ArgT0&)*arg0);
    }
private:
    FuncT m_f;
};
template<typename RetT, typename ArgT0, typename ArgT1>
class CFunction2:
    public IFunction
{
private:
    typedef RetT(*FuncT)(ArgT0, ArgT1);
public:
    CFunction2(FuncT f): m_f(f){}
private:
    virtual void call(Thread* thread)
    {
        char *base = thread->frameBase();
        char *arg0 = (RetT*)base + 1;
        char *arg1 = (ArgT0*)arg0 + 1;
        (RetT&)*base = m_f((ArgT0&)*arg0, (ArgT1&)*arg1);
    }
private:
    FuncT m_f;
};

class GlobalEnvironment
{
public:
    GlobalEnvironment():
        m_funcPreMain(StmtNodePtr(new StmtNode_Block()))
    {
    }
    void registerFunction(const string& name, FunctionPtr func)
    {
        ASSERT(m_funcs.count(name) == 0);
        m_funcs[name] = func;
    }
    FunctionPtr getFunc(const string& name) { return m_funcs[name]; }
    static GlobalEnvironment* instance()
    {
        static GlobalEnvironment s_ins;
        return &s_ins;
    }
    ASTFunction* getFuncPreMain() { return &m_funcPreMain; }
private:
    ASTFunction m_funcPreMain;
    map<string, FunctionPtr> m_funcs;
};

class StringPool
{
public:
    static StringPool* instance()
    {
        static StringPool s_ins;
        return &s_ins;
    }

    const char* get(const string& s)
    {
        auto iter = m_pool.lower_bound(s);
        if (iter == m_pool.end() || *iter != s) {
            return m_pool.insert(iter, s)->c_str();
        }
        return iter->c_str();
    }
private:
    set<string> m_pool;
};

void run();

#endif
