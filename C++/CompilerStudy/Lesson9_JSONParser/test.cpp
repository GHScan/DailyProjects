#include "pch.h" 

#include <memory>
#include <vector>
#include <unordered_map>
#include <typeinfo>
#include <sstream>

#include "common.h"

// ==================== lexical
enum TokenType
{
    TT_notation = 1, 
    TT_id, 
    TT_str, 
    TT_number,
};

template<>
const char* matchLexeme<TT_number>(const char*src)
{
    if (!isdigit(src[0])) return NULL;
    while (isdigit(*++src));
    return src;
}
template<>
const char* matchLexeme<TT_str>(const char*src)
{
    if (src[0] != '\'') return NULL;
    while (*++src != '\'');
    return ++src;
}
template<>
const char* matchLexeme<TT_id>(const char*src)
{
    if (!(src[0] == '_' || isalpha(src[0]))) return NULL;
    ++src;
    while (isdigit(src[0]) || isalpha(src[0]) || src[0] == '_') ++src;
    return src;
}
template<>
const char* matchLexeme<TT_notation>(const char*src)
{
    return ++src;
}

// ==================== syntax
/*
   data = dict | array
   dict = {kvpairs optcomma}
   kvpairs = kvpair | kvpair , kvpairs
   kvpair = factor : factor
   array = [tuple optcomma]
   tuple = factor | factor , tuple
   optcomma = e | ,
   factor = bool | num | str | array | dict
   bool = true | false
   num = [1-9][0-9]*
   str = '[^']*'
*/

struct NumValue;
struct StringValue;
struct BooleanValue;
struct ArrayValue;
struct DictValue;
struct IValueVisitor
{
    virtual ~IValueVisitor() {}
    virtual void visit(NumValue *v) = 0;
    virtual void visit(StringValue *v) = 0;
    virtual void visit(BooleanValue *v) = 0;
    virtual void visit(ArrayValue *v) = 0;
    virtual void visit(DictValue *v) = 0;
};

struct IValue
{
    virtual ~IValue() {}
    virtual void acceptVisitor(IValueVisitor *v) = 0;
    virtual size_t hash() = 0;
    virtual bool equal(IValue *o) = 0;
};
typedef std::shared_ptr<IValue> ValuePtr;

namespace std
{
    template<>
    struct hash<ValuePtr>
    {
        size_t operator () (const ValuePtr &v) const 
        {
            if (v == NULL) return 0;
            return v->hash();
        }
    };
    template<>
    struct equal_to<ValuePtr>
    {
        bool operator () (const ValuePtr& l, const ValuePtr& r) const
        {
            if (l != NULL && r != NULL) {
                return l->equal(r.get());
            }
            return l == NULL && r == NULL;
        }
    };
}

struct NumValue:
    public IValue
{
    int num;
    NumValue(const std::string& s): num(atoi(s.c_str())){}
    virtual void acceptVisitor(IValueVisitor *v) { v->visit(this); }
    virtual size_t hash() { return std::hash<int>()(num); }
    virtual bool equal(IValue *o) { return static_cast<NumValue*>(o)->num == num; }
};
struct StringValue:
    public IValue
{
    std::string s;
    StringValue(const std::string& s){ this->s.assign(s.begin() + 1, s.begin() + s.size() - 1); }
    virtual void acceptVisitor(IValueVisitor *v) { v->visit(this); }
    virtual size_t hash() { return std::hash<std::string>()(s); }
    virtual bool equal(IValue *o) { return static_cast<StringValue*>(o)->s == s; }
};
struct BooleanValue:
    public IValue
{
    bool b;
    BooleanValue(const std::string& s):b(s == "true"){}
    virtual void acceptVisitor(IValueVisitor *v) { v->visit(this); }
    virtual size_t hash() { return std::hash<bool>()(b); }
    virtual bool equal(IValue *o) { return static_cast<BooleanValue*>(o)->b == b; }
};
struct ArrayValue:
    public IValue
{
    std::vector<ValuePtr> values;
    virtual void acceptVisitor(IValueVisitor *v) { v->visit(this); }
    virtual size_t hash() { return std::hash<ArrayValue*>()(this);}
    virtual bool equal(IValue *o) { return o == this; }
};
struct DictValue:
    public IValue
{
    std::unordered_map<ValuePtr, ValuePtr> values;
    virtual void acceptVisitor(IValueVisitor *v) { v->visit(this); }
    virtual size_t hash() { return std::hash<DictValue*>()(this);}
    virtual bool equal(IValue *o) { return o == this; }
};

class Parser:
    public LexicalAnalysiser
{
public:
    Parser(const std::string& fname, const std::string& src)
    {
        lexicalAnalysis<TT_notation, TT_number>(fname, src);
        data();
        m_tokens.clear();
    }
    ValuePtr getData() const { return m_data; }

private:
    void data()
    {
        if (m_data = dict());
        else m_data = array();
    }
    ValuePtr dict()
    {
        if (tryConsumeToken(TT_notation, "{")) {
            DictValue *d = new DictValue();
            ValuePtr r(d);
            ValuePtr k, v;
            while (kvpair(k, v)) {
                d->values[k] = v;
                tryConsumeToken(TT_notation, ",");
            }
            consumeToken(TT_notation, "}");
            return r;
        }
        return ValuePtr();
    }
    ValuePtr array()
    {
        if (tryConsumeToken(TT_notation, "[")) {
            ArrayValue *a = new ArrayValue();
            ValuePtr r(a);
            while (ValuePtr f = factor()) {
                a->values.push_back(f);
                tryConsumeToken(TT_notation, ",");
            }
            consumeToken(TT_notation, "]");
            return r;
        }
        return ValuePtr();
    }
    bool kvpair(ValuePtr& k, ValuePtr &v)
    {
        backupTokenPos();
        if ((k = factor()) && tryConsumeToken(TT_notation, ":") && (v = factor())) {
            discardBackupTokenPos();
            return true;
        }
        restoreTokenPos();
        return false;
    }
    ValuePtr factor()
    {
        ValuePtr p = array();
        if (p != NULL) return p;
        if (p = dict()) return p;
        if (p = _bool()) return p;
        if (p = num()) return p;
        if (p = str()) return p;
        return p;
    }
    ValuePtr _bool()
    {
        if (tryConsumeToken(TT_id, "true") || tryConsumeToken(TT_id, "false")) {
            return ValuePtr(new BooleanValue(getPreviewToken().value));
        }
        return ValuePtr();
    }
    ValuePtr num()
    {
        if (tryConsumeToken(TT_number)) {
            return ValuePtr(new NumValue(getPreviewToken().value));
        }
        return ValuePtr();
    }
    ValuePtr str()
    {
        if (tryConsumeToken(TT_str)) {
            return ValuePtr(new StringValue(getPreviewToken().value));
        }
        return ValuePtr();
    }

private:
    ValuePtr m_data;
};

void printTab(std::ostream& so, int n)
{
    for (int i = 0; i < n; ++i) so << '\t';
}

class LuaCodeGen:
    public IValueVisitor
{
public:
    std::string apply(ValuePtr d)
    {
        m_so.str("");
        m_d = 0;
        d->acceptVisitor(this);
        return m_so.str();
    }

private:
    virtual void visit(NumValue *v) 
    {
        m_so << v->num;
    }
    virtual void visit(StringValue *v) 
    {
        m_so << '\'' << v->s << '\'';
    }
    virtual void visit(BooleanValue *v) 
    {
        m_so << v->b ? "true" : "false";
    }
    virtual void visit(ArrayValue *v) 
    {
        m_so << "{\n";
        ++m_d;
        for (std::vector<ValuePtr>::const_iterator iter = v->values.begin();
                iter != v->values.end();
                ++iter) {
            printTab(m_so, m_d);
            (*iter)->acceptVisitor(this);
            m_so << ",\n";
        }
        --m_d;
        printTab(m_so, m_d);
        m_so << "}";
    }
    virtual void visit(DictValue *v) 
    {
        m_so << "{\n";
        ++m_d;
        for (std::unordered_map<ValuePtr, ValuePtr>::const_iterator iter = v->values.begin();
                iter != v->values.end();
                ++iter) {
            printTab(m_so, m_d);
            m_so << '['; 
            iter->first->acceptVisitor(this);
            m_so << "] = ";
            iter->second->acceptVisitor(this);
            m_so << ",\n";
        }
        --m_d;
        printTab(m_so, m_d);
        m_so << "}";
    }
private:
    std::ostringstream m_so;
    int m_d;
};

int main()
{
    try
    {
        std::string fname = "1.txt";
        cout << LuaCodeGen().apply(Parser(fname, readFile(fname)).getData());
    }
    catch(const std::exception& e) {
        cout << "Exception : " << e.what() << endl;
    }
}
