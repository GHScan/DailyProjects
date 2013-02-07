
#ifndef RUNTIME_H
#define RUNTIME_H

class ByteCodeSeq;
typedef shared_ptr<ByteCodeSeq> ByteCodeSeqPtr;
struct FunctionType;

class RuntimeEnv
{
public:
    RuntimeEnv(): m_top(0), pc(0)
    {
        m_data.resize(1024 * 1024);
    }

    int pc;

    void pushFrame(int retArgSize)
    {
        m_frameBases.push_back(m_top - retArgSize);
        m_pcs.push_back(pc);
        pc = 0;
    }
    void popFrame(int retSize)
    {
        pc = m_pcs.back();
        m_pcs.pop_back();
        m_top = m_frameBases.back() + retSize;
        m_frameBases.pop_back();
    }
    void reserveFrame(int frameSize)
    {
        m_top = m_frameBases.back() + frameSize;
    }
    char* frameBase()
    {
        return &m_data[m_frameBases.back()];
    }
    template<typename T>
    T& localVariable(int off)
    {
        return (T&)m_data[m_frameBases.back() + off];
    }

    template<typename T>
    void pushValue(T v)
    {
        (T&)m_data[m_top] = v;
        m_top += sizeof(T);
    }
    template<typename T>
    void popValue()
    {
        m_top -= sizeof(T);
    }
    template<typename T>
    void popValue(T& v)
    {
        m_top -= sizeof(T);
        v = (T&)m_data[m_top];
    }
    template<typename T>
    T& topValue(int idx)
    {
        assert(idx < 0);
        return (T&)m_data[m_top + idx * sizeof(T)];
    }

    void reserveGlobal(int size)
    {
        m_globalData.resize(size);
    }
    template<typename T>
    T& globalVariable(int off)
    {
        return (T&)m_data[off];
    }
    char *globalBase()
    {
        return &m_globalData[0];
    }
private:
    vector<int> m_pcs;
    vector<char> m_globalData;
    vector<char> m_data;
    vector<int> m_frameBases;
    int m_top;
};

struct IFunction
{
    virtual ~IFunction(){}
    virtual void call(RuntimeEnv *env) = 0;
};
typedef shared_ptr<IFunction> FunctionPtr;

class ASTFunction:
    public IFunction
{
public:
    ASTFunction(StmtNodePtr stmt);
    StmtNodePtr& getStmt() { return m_stmt; }
    ByteCodeSeqPtr& getCodeSeq() { return m_codeSeq; }
    void emitCode();
    virtual void call(RuntimeEnv *env);
private:
    StmtNodePtr m_stmt;
    ByteCodeSeqPtr m_codeSeq;
};

template<typename RetT, typename ArgT0, typename ArgT1, typename ArgT2>
class CFunction3:
    public IFunction
{
private:
    typedef RetT(*FuncT)(ArgT0, ArgT1, ArgT2);
public:
    CFunction3(FuncT f): m_f(f){}
    virtual void call(RuntimeEnv *env)
    {
        char *base = env->frameBase();
        char *arg0 = (char*)((RetT*)base + 1);
        char *arg1 = (char*)((ArgT0*)arg0 + 1);
        char *arg2 = (char*)((ArgT1*)arg1 + 1);
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
    virtual void call(RuntimeEnv *env)
    {
        char *base = env->frameBase();
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
    virtual void call(RuntimeEnv *env)
    {
        char *base = env->frameBase();
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
    virtual void call(RuntimeEnv *env)
    {
        char *base = env->frameBase();
        char *arg0 = (char*)((RetT*)base + 1);
        char *arg1 = (char*)((ArgT0*)arg0 + 1);
        (RetT&)*base = m_f((ArgT0&)*arg0, (ArgT1&)*arg1);
    }
private:
    FuncT m_f;
};

class CVarlengFunction:
    public IFunction
{
public:
    typedef int(*FuncT)(const char *fmt, char *args);
public:
    CVarlengFunction(FuncT f): m_f(f){}
    virtual void call(RuntimeEnv *env)
    {
        char *base = env->frameBase();
        char *arg0 = (char*)((int*)base + 1);
        char *argN = (char*)((const char**)arg0 + 1);
        (int&)*base = m_f((const char*&)*arg0, argN);
    }
private:
    FuncT m_f;
};

class CodeManager
{
public:
    static CodeManager* instance()
    {
        static CodeManager s_ins;
        return &s_ins;
    }

    CodeManager();
    void registerFunction(const string& name, FunctionPtr func)
    {
        ASSERT(m_funcs.count(name) == 0);
        m_funcs[name] = func;
    }
    FunctionPtr getFunc(const string& name) { return m_funcs[name]; }
    ASTFunction* getFuncPreMain() { return dynamic_cast<ASTFunction*>(m_funcPreMain.get()); }
    void emitAll();
    vector<string> getFuncNames()
    {
        vector<string> r;
        for (auto p : m_funcs) r.push_back(p.first);
        return r;
    }
private:
    FunctionPtr m_funcPreMain;
    map<string, FunctionPtr> m_funcs;
};

#endif
