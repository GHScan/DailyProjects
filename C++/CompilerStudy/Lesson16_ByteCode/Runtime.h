#ifndef RUNTIME_H
#define RUNTIME_H

#include "pch.h"

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
            m_value.str = (char*)malloc(strlen(m_value.str) + 1);
            strcpy(m_value.str, o.m_value.str);
        }
    }
    Value(Value&& o):
        m_type(o.m_type), m_value(o.m_value)
    {
        o.m_type = T_Null;
        o.m_value.i = 0;
    }
    Value& operator = (const Value& o)
    {
        Value t(o);
        swap(t);
        return *this;
    }
    Value& operator = (Value&& o)
    {
        Value t(std::forward<Value&&>(o));
        swap(t);
        return *this;
    }
    ~Value()
    {
        if (m_type == T_String) free(m_value.str);
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
    bool operator != (const Value& o) const
    {
        return !(*this == o);
    }
    bool operator < (const Value& o) const
    {
        if (m_type == o.m_type) {
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
        return m_type < o.m_type;
    }
    bool operator > (const Value& o) const
    {
        return o < *this;
    }
    bool operator <= (const Value& o) const
    {
        return !(*this > o);
    }
    bool operator >= (const Value& o) const
    {
        return !(*this < o);
    }

    Value& operator += (const Value& o)
    {
        if (m_type == T_String) {
            string r = o.toString();
            m_value.str = (char*)realloc(m_value.str, strlen(m_value.str) + r.size() + 1);
            strcat(m_value.str, r.c_str());
        }
        else if (m_type == T_Int) {
            ASSERT(o.m_type == T_Int);
            m_value.i += o.m_value.i;
        }
        else ASSERT(0);
        return *this;
    }
    Value& operator -= (const Value& o)
    {
        ASSERT(m_type == T_Int && o.m_type == T_Int);
        m_value.i -= o.m_value.i;
        return *this;
    }
    Value& operator *= (const Value& o)
    {
        if (m_type == T_String) {
            ASSERT(o.m_type == T_Int);
            string r = m_value.str;
            m_value.str = (char*)realloc(m_value.str, r.size() * o.m_value.i + 1);
            for (int i = 1; i < o.m_value.i; ++i) {
                strcat(m_value.str, r.c_str());
            }
        }
        else if (m_type == T_Int) {
            ASSERT(o.m_type == T_Int);
            m_value.i *= o.m_value.i;
        }
        else ASSERT(0);
        return *this;
    }
    Value& operator /= (const Value& o)
    {
        ASSERT(m_type == T_Int && o.m_type == T_Int);
        m_value.i /= o.m_value.i;
        return *this;
    }
    Value& operator %= (const Value& o)
    {
        ASSERT(m_type == T_Int && o.m_type == T_Int);
        m_value.i %= o.m_value.i;
        return *this;
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
    Value& _not()
    {
        ASSERT(m_type == T_Int);
        m_value.i = !m_value.i;
        return *this;
    }

    static Value createString(const string& s)
    {
        Value v;
        v.m_type = T_String;
        v.m_value.str = (char*)malloc(s.size() + 1);
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
    static Value INT_0;
    static Value INT_1;
private:
    Type m_type;
    union {
        char *str;
        int i;
    }m_value;
};
inline Value operator + (const Value& l, const Value& r)
{
    return Value(l) += r;
}
inline Value operator - (const Value& l, const Value& r)
{
    return Value(l) -= r;
}
inline Value operator * (const Value& l, const Value& r)
{
    return Value(l) *= r;
}
inline Value operator / (const Value& l, const Value& r)
{
    return Value(l) /= r;
}
inline Value operator % (const Value& l, const Value& r)
{
    return Value(l) %= r;
}

struct IFunction
{
    virtual ~IFunction(){}
    virtual Value call(const vector<Value>& args) = 0;
};
typedef shared_ptr<IFunction> FunctionPtr;
class GlobalEnvironment
{
public:
    static GlobalEnvironment* instance()
    {
        static GlobalEnvironment s_ins;
        return &s_ins;
    }
    void registerFunc(const string& name, FunctionPtr func)
    {
        ASSERT(m_funcMap.count(name) == 0);
        m_funcMap[name] = func;
    }
    const FunctionPtr& getFunc(const string& name)
    {
        return m_funcMap[name];
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
    map<string, FunctionPtr> m_funcMap;
};

class StackFrame
{
public:
    StackFrame(const vector<Value>& args): m_locals(args), ip(0){}
    Value& local(int i)
    {
        if (i >= m_locals.size()) {
            m_locals.resize(i + 1);
        }
        return m_locals[i];
    }
    int ip;
    Value retValue;
    vector<Value> evalStack;
private:
    vector<Value> m_locals;
};

class CFunction:
    public IFunction
{
private:
    typedef Value(*FuncT)(const vector<Value>& args);
public:
    CFunction(FuncT f): m_func(f){}
    virtual Value call(const vector<Value>& args)
    {
        return m_func(args);
    }
private:
    FuncT m_func;
};

class ByteCodeSeq;
class ByteCodeFunction:
    public IFunction
{
public:
    ByteCodeFunction(int argc, ByteCodeSeq *seq): m_seq(seq), m_argc(argc){}
    ~ByteCodeFunction();
    const ByteCodeSeq* getCodeSeq() const { return m_seq; }
    virtual Value call(const vector<Value>& args);
private:
    ByteCodeSeq *m_seq;
    int m_argc;
};

#endif
