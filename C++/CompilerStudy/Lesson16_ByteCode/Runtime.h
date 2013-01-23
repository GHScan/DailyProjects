#ifndef RUNTIME_H
#define RUNTIME_H

#include "pch.h"

struct IStmtNode;
typedef shared_ptr<IStmtNode> StmtNodePtr;

class Value
{
public:
    enum Type
    {
        T_Null,
        T_Int,
        T_String,
    };

    Value():
        m_type(T_Null)
    { 
        m_value.i = 0;
    }
    Value(const Value& o):
        m_type(o.m_type), m_value(o.m_value)
    {
        if (m_type == T_String) {
            m_value.str = new char[strlen(m_value.str) + 1];
            strcpy(m_value.str, o.m_value.str);
        }
    }
    Value& operator = (const Value& o)
    {
        Value t(o);
        swap(t);
        return *this;
    }
    ~Value()
    {
        if (m_type == T_String) delete[] m_value.str;
    }

    bool operator == (const Value& o) const
    {
        if (m_type != o.m_type) return false;
        switch (m_type) {
            case T_String:
                return strcmp(m_value.str, o.m_value.str) == 0;
            case T_Int:
                return m_value.i == o.m_value.i;
            case T_Null:
                return true;
            default:
                ASSERT(0);
                return false;
        }
    }
    bool operator < (const Value& o) const
    {
        if (m_type != o.m_type) return false;
        switch (m_type) {
            case T_String:
                return strcmp(m_value.str, o.m_value.str) < 0;
            case T_Int:
                return m_value.i < o.m_value.i;
            case T_Null:
                return false;
            default:
                ASSERT(0);
                return false;
        }
    }
    Value operator + (const Value& o) const
    {
        if (m_type == T_String) {
            return createString(string(m_value.str) + o.toString());
        }
        else if (m_type == T_Int) {
            ASSERT(o.m_type == T_Int);
            return createInt(m_value.i + o.m_value.i);
        }
        else {
            ASSERT(0);
            return Value();
        }
    }
    Value operator - (const Value& o) const
    {
        ASSERT(m_type == T_Int && o.m_type == T_Int);
        return createInt(m_value.i - o.m_value.i);
    }
    Value operator * (const Value& o) const
    {
        if (m_type == T_String) {
            string r = m_value.str;
            for (int i = 1; i < o.m_value.i; ++i) {
                r += m_value.str;
            }
            return createString(r);
        }
        else if (m_type == T_Int) {
            ASSERT(o.m_type == T_Int);
            return createInt(m_value.i * o.m_value.i);
        }
        else {
            ASSERT(0);
            return Value();
        }
    }
    Value operator / (const Value& o) const
    {
        ASSERT(m_type == T_Int && o.m_type == T_Int);
        return createInt(m_value.i / o.m_value.i);
    }
    Value operator % (const Value& o) const
    {
        ASSERT(m_type == T_Int && o.m_type == T_Int);
        return createInt(m_value.i % o.m_value.i);
    }

    void swap(Value& o)
    {
        std::swap(m_type, o.m_type);
        std::swap(m_value, o.m_value);
    }
    string toString() const
    {
        switch (m_type) {
            case T_String:
                return m_value.str;
            case T_Null:
                return "(null)";
            case T_Int:
                return format("%d", m_value.i);
            default:
                ASSERT(0);
                return "";
        }
    }
    string toReprString() const
    {
        if (m_type == T_String) return "'" + toString() + "'";
        return toString();
    }
    bool toBoolean() const
    {
        ASSERT(m_type == T_Int);
        return m_value.i != 0;
    }

    static Value createString(const string& s)
    {
        Value v;
        v.m_type = T_String;
        v.m_value.str = new char[s.size() + 1];
        strcpy(v.m_value.str, s.c_str());
        return v;
    }
    static Value createInt(int i)
    {
        Value v;
        v.m_type = T_Int;
        v.m_value.i = i;
        return v;
    }
    static Value createBoolean(bool b)
    {
        Value v;
        v.m_type = T_Int;
        v.m_value.i = b ? 1 : 0;
        return v;
    }
private:
    Type m_type;
    union {
        char *str;
        int i;
    }m_value;
};

struct IFunction;
typedef shared_ptr<IFunction> FunctionPtr;

class GlobalEnvironment
{
public:
    static GlobalEnvironment* instance()
    {
        static GlobalEnvironment s_ins;
        return &s_ins;
    }
    const Value& getValue(const string& name) { return m_vars[name]; }
    void setValue(const string& name, const Value& val){ m_vars[name] = val;}
    const FunctionPtr& getFunc(const string& name) { return m_funcMap[name]; }
    void registerFunc(const string& name, const FunctionPtr& func) 
    {
        ASSERT(m_funcMap.count(name) == 0);
        m_funcMap[name] = func;
    }
    vector<string> getFuncNames() const 
    {
        vector<string> r;
        for (auto &p : m_funcMap) r.push_back(p.first);
        return r;
    }
private:
    GlobalEnvironment(){}
private:
    map<string, Value> m_vars;
    map<string, FunctionPtr> m_funcMap;
};
class StackFrame
{
public:
    void beginBlock()
    {
        m_blocks.push_back(map<string, Value>());
    }
    void endBlock()
    {
        m_blocks.pop_back();
    }
    void declareLocal(const string& name)
    {
        m_blocks.back()[name] = Value();
    }
    void setValue(const string& name, const Value& val)
    {
        for (int i = (int)m_blocks.size() - 1; i >= 0; --i) {
            auto iter = m_blocks[i].find(name);
            if (iter != m_blocks[i].end())  {
                iter->second = val;
                return;
            }
        }
        GlobalEnvironment::instance()->setValue(name, val);
    }
    const Value& getValue(const string& name)
    {
        for (int i = (int)m_blocks.size() - 1; i >= 0; --i) {
            auto iter = m_blocks[i].find(name);
            if (iter != m_blocks[i].end())  {
                return iter->second;
            }
        }
        return GlobalEnvironment::instance()->getValue(name);
    }
private:
    vector<map<string, Value> > m_blocks;
};

struct IFunction
{
    virtual ~IFunction(){}
    virtual Value call(const vector<Value>& args) = 0;
};
class CFunction:
    public IFunction
{
public:
    typedef Value (*FuncT)(const vector<Value>& args);
    CFunction(FuncT f): func(f){}
private:
    virtual Value call(const vector<Value>& args)
    {
        return func(args);
    }
    FuncT func;
};
class ASTFunction:
    public IFunction
{
public:
    ASTFunction(): argc(0){}
    StmtNodePtr stmt;
    int argc;
private:
    virtual Value call(const vector<Value>& args);
};

#endif
