#ifndef COMMON_H
#define COMMON_H

#include <string>
#include <vector>
#include <exception>
#include <unordered_map>
#include <memory>

class MyException: 
    public std::exception
{
public:
    MyException(const char *s): m_s(s){}
    ~MyException() throw() {}
    const char * what() const throw() { return m_s.c_str(); }
private:
    std::string m_s;
};
#define _TO_STRING(s) #s
#define TO_STRING(s) _TO_STRING(s)
#define FILE_LINE __FILE__ "(" TO_STRING(__LINE__) ")"
#define ASSERT(b) if (b){} else throw MyException(FILE_LINE ":" #b)

struct IValue
{
    virtual ~IValue(){}
    virtual void add(const IValue* o) = 0;
    virtual void sub(const IValue* o) = 0;
    virtual void mul(const IValue* o) = 0;
    virtual void div(const IValue* o) = 0;
    virtual void mod(const IValue* o) = 0;
    virtual bool greater(const IValue* o) = 0;
    virtual bool greaterEqual(const IValue* o) = 0;
    virtual bool less(const IValue* o) = 0;
    virtual bool lessEqual(const IValue* o) = 0;
    virtual bool equal(const IValue* o) = 0;
    virtual bool toBoolean() const = 0;
    virtual std::string toString() const = 0;
    virtual IValue* clone() = 0;
};
typedef std::shared_ptr<IValue> ValuePtr;
class NumValue:
    public IValue
{
public:
    NumValue(int i): m_value(i){}
    NumValue(const std::string& s): m_value(atoi(s.c_str())){}
    int getValue() const { return m_value; }
private:
    virtual void add(const IValue* o)
    {
        if (const NumValue* p = dynamic_cast<const NumValue*>(o)) {
            m_value += p->m_value;
        } else ASSERT(0);
    }
    virtual void sub(const IValue* o)
    {
        if (const NumValue* p = dynamic_cast<const NumValue*>(o)) {
            m_value -= p->m_value;
        } else ASSERT(0);
    }
    virtual void mul(const IValue* o)
    {
        if (const NumValue* p = dynamic_cast<const NumValue*>(o)) {
            m_value *= p->m_value;
        } else ASSERT(0);
    }
    virtual void div(const IValue* o) 
    {
        if (const NumValue* p = dynamic_cast<const NumValue*>(o)) {
            m_value /= p->m_value;
        } else ASSERT(0);
    }
    virtual void mod(const IValue* o)
    {
        if (const NumValue* p = dynamic_cast<const NumValue*>(o)) {
            m_value %= p->m_value;
        } else ASSERT(0);
    }
    virtual bool greater(const IValue* o)
    {
        if (const NumValue* p = dynamic_cast<const NumValue*>(o)) {
            return m_value > p->m_value;
        } else ASSERT(0);
    }
    virtual bool greaterEqual(const IValue* o)
    {
        if (const NumValue* p = dynamic_cast<const NumValue*>(o)) {
            return m_value >= p->m_value;
        } else ASSERT(0);
    }
    virtual bool less(const IValue* o)
    {
        if (const NumValue* p = dynamic_cast<const NumValue*>(o)) {
            return m_value < p->m_value;
        } else ASSERT(0);
    }
    virtual bool lessEqual(const IValue* o)
    {
        if (const NumValue* p = dynamic_cast<const NumValue*>(o)) {
            return m_value <= p->m_value;
        } else ASSERT(0);
    }
    virtual bool equal(const IValue* o)
    {
        if (const NumValue* p = dynamic_cast<const NumValue*>(o)) {
            return m_value == p->m_value;
        } else ASSERT(0);
    }
    virtual bool toBoolean() const
    {
        return m_value != 0;
    }
    virtual std::string toString() const
    {
        char buf[32];
        sprintf(buf, "%d", m_value);
        return buf;
    }
    virtual IValue* clone() { return new NumValue(m_value); }
private:
    int m_value;
};
class StringValue:
    public IValue
{
public:
    StringValue(const std::string& s): m_value(s){}
private:
    virtual void add(const IValue* o) { m_value += o->toString();}
    virtual void sub(const IValue* o) { ASSERT(0); }
    virtual void mul(const IValue* o) 
    { 
        if (const NumValue* p = dynamic_cast<const NumValue*>(o)) {
            std::string s = m_value;
            for (int i = 1; i < p->getValue(); ++i) {
                m_value += s;
            }
        } else ASSERT(0);
    }
    virtual void div(const IValue* o) { ASSERT(0); }
    virtual void mod(const IValue* o) { ASSERT(0); }
    virtual bool greater(const IValue* o) 
    {
        if (const StringValue* p = dynamic_cast<const StringValue*>(o)) {
            return m_value > p->m_value;
        } else ASSERT(0);
    }
    virtual bool greaterEqual(const IValue* o)
    {
        if (const StringValue* p = dynamic_cast<const StringValue*>(o)) {
            return m_value >= p->m_value;
        } else ASSERT(0);
    }
    virtual bool less(const IValue* o) 
    {
        if (const StringValue* p = dynamic_cast<const StringValue*>(o)) {
            return m_value < p->m_value;
        } else ASSERT(0);
    }
    virtual bool lessEqual(const IValue* o)
    {
        if (const StringValue* p = dynamic_cast<const StringValue*>(o)) {
            return m_value <= p->m_value;
        } else ASSERT(0);
    }
    virtual bool equal(const IValue* o) 
    {
        if (const StringValue* p = dynamic_cast<const StringValue*>(o)) {
            return m_value == p->m_value;
        } else ASSERT(0);
    }
    virtual bool toBoolean() const { ASSERT(0); }
    virtual std::string toString() const { return m_value; }
    virtual IValue* clone() { return new StringValue(m_value); }
private:
    std::string m_value;
};

#endif
