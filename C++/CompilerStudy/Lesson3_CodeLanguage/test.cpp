#include "pch.h" 

#include <stdarg.h>
#include <stdlib.h>

#include <string>
#include <vector>
#include <map>
#include <fstream>
#include <exception>
#include <memory>

#define _TO_STRING(s) #s
#define TO_STRING(s) _TO_STRING(s)
#define FILE_LINE __FILE__ "(" TO_STRING(__LINE__) ")"

class MyException:
    public std::exception
{
public:
    MyException(const std::string& s): m_s(s){}
    virtual ~MyException() throw() {}
    const char *what() const throw() { return m_s.c_str(); }
private:
    std::string m_s;
};
#define Assert(b) if (b); else { throw MyException(FILE_LINE " - assert failed: " #b); }

std::string format(const char* fmt, ...)
{
    static std::vector<char> buf(1);
    va_list args;
    va_start(args, fmt);
    for (;;) {
        int n = vsnprintf(&buf[0], buf.size(), fmt, args);
        if (n < buf.size()) break;
        buf.resize(buf.size() * 2);
    }
    va_end(args);
    return &buf[0];
}

class IntValue;
class StringValue;
struct IValue
{
    virtual ~IValue() {}
    virtual void add(IValue* v) = 0;
    virtual void sub(IValue* v) = 0;
    virtual void mul(IValue* v) = 0;
    virtual void div(IValue* v) = 0;
    virtual bool less(IValue* v) = 0;
    virtual bool equal(IValue* v) = 0;
    virtual void addFrom(IntValue* v) = 0;
    virtual void subFrom(IntValue* v) = 0;
    virtual void mulFrom(IntValue* v) = 0;
    virtual void divFrom(IntValue* v) = 0;
    virtual bool less(IntValue* v) = 0;
    virtual bool equal(IntValue* v) = 0;
    virtual void addFrom(StringValue* v) = 0;
    virtual void subFrom(StringValue* v) = 0;
    virtual void mulFrom(StringValue* v) = 0;
    virtual void divFrom(StringValue* v) = 0;
    virtual bool less(StringValue* v) = 0;
    virtual bool equal(StringValue* v) = 0;
    virtual std::shared_ptr<IValue> clone() = 0;
    virtual std::string toString() = 0;
};
typedef std::shared_ptr<IValue> ValuePtr;
class IntValue:
    public IValue
{
public:
    IntValue(int v = 0): m_v(v) {}
    virtual void add(IValue* v) { v->addFrom(this); }
    virtual void sub(IValue* v) { v->subFrom(this); }
    virtual void mul(IValue* v) { v->mulFrom(this); }
    virtual void div(IValue* v) { v->divFrom(this); }
    virtual bool less(IValue* v) { return !v->equal(this) && !v->less(this); }
    virtual bool equal(IValue* v) { return v->equal(this); }
    virtual void addFrom(IntValue* v) { v->m_v += m_v; }
    virtual void subFrom(IntValue* v) { v->m_v -= m_v; } 
    virtual void mulFrom(IntValue* v) { v->m_v *= m_v; } 
    virtual void divFrom(IntValue* v) { v->m_v /= m_v; } 
    virtual bool less(IntValue* v) { return m_v < v->m_v; }
    virtual bool equal(IntValue* v) { return m_v == v->m_v; }
    virtual void addFrom(StringValue* v) ;
    virtual void subFrom(StringValue* v) { Assert(0); }
    virtual void mulFrom(StringValue* v) ;
    virtual void divFrom(StringValue* v) { Assert(0); }
    virtual bool less(StringValue* v) { Assert(0); return false; }
    virtual bool equal(StringValue* v) { Assert(0); return false; }
    virtual ValuePtr clone() { return ValuePtr(new IntValue(m_v)); }
    virtual std::string toString() { return format("%d", m_v); }
private:
    int m_v;
};
class StringValue:
    public IValue
{
public:
    StringValue(const std::string& v = ""): m_v(v) {}
    virtual void add(IValue* v) { v->addFrom(this); }
    virtual void sub(IValue* v) { v->subFrom(this); }
    virtual void mul(IValue* v) { v->mulFrom(this); }
    virtual void div(IValue* v) { v->divFrom(this); }
    virtual bool less(IValue* v) { return !v->equal(this) && !v->less(this); }
    virtual bool equal(IValue* v) { return v->equal(this); }
    virtual void addFrom(IntValue* v) { Assert(0); } 
    virtual void subFrom(IntValue* v) { Assert(0); } 
    virtual void mulFrom(IntValue* v) { Assert(0); } 
    virtual void divFrom(IntValue* v) { Assert(0); } 
    virtual bool less(IntValue* v) { Assert(0); return false; }
    virtual bool equal(IntValue* v) { Assert(0); return false; }
    virtual void addFrom(StringValue* v) { v->m_v += m_v; }
    virtual void subFrom(StringValue* v) { Assert(0); }  
    virtual void mulFrom(StringValue* v) { Assert(0); }  
    virtual void divFrom(StringValue* v) { Assert(0); }  
    virtual bool less(StringValue* v) { return m_v < v->m_v; }
    virtual bool equal(StringValue* v) { return m_v == v->m_v; }
    virtual ValuePtr clone() { return ValuePtr(new StringValue(m_v)); }
    virtual std::string toString() { return m_v; }
private:
    friend class IntValue;
    std::string m_v;
};
void IntValue::addFrom(StringValue* v) 
{ 
    v->m_v += toString();
}
void IntValue::mulFrom(StringValue* v) 
{
    std::string s = v->m_v;
    for (int i = 1; i < m_v; ++i) v->m_v += s;
}

struct Token
{
    enum Type
    {
        T_ConstValue,
        T_Identify,
        T_Notation,
    };
    Type type;
    ValuePtr value;
};

struct GlobalEvrionment;

struct Context
{
    std::map<std::string, ValuePtr> vars;
};

class Command
{
public:
    enum Type
    {
        T_Add, T_Sub, T_Mul, T_Div,
        T_Assign, T_Local, T_Call,
        T_Goto, T_LessGoto, T_EqualGoto,
    };
    void apply(GlobalEvrionment &g);
    static std::shared_ptr<Command> create(Token* begin, Token *end);
private:
    Command(Type t, Token* begin, Token* end): m_type(t), m_params(begin, end){}
private:
    Type m_type;
    std::vector<Token> m_params;
};
typedef std::shared_ptr<Command> CommandPtr;

struct IFunction
{
    virtual ~IFunction(){}
    virtual void Call(GlobalEvrionment& g, const std::vector<ValuePtr>& args) = 0;
};
typedef std::shared_ptr<IFunction> FunctionPtr;

struct CommandFunction:
    public IFunction
{
    std::string name;
    std::vector<CommandPtr> cmds;
    std::map<std::string, int> labels;
    virtual void Call(GlobalEvrionment& g, const std::vector<ValuePtr>& args);
};

struct CFunction:
    public IFunction
{
    void (*pfunc)(GlobalEvrionment& g, const std::vector<ValuePtr>& args);
    CFunction(void (*f)(GlobalEvrionment& g, const std::vector<ValuePtr>& args)): pfunc(f){}
    virtual void Call(GlobalEvrionment& g, const std::vector<ValuePtr>& args)
    {
        pfunc(g, args);
    }
};

struct Stack
{
    std::vector<Context> contexts;
    std::vector<FunctionPtr> funcs;
    std::vector<int> pcs;
};

struct GlobalEvrionment
{
    Stack stack;
    Context globalVars;
    std::map<std::string, FunctionPtr> funcs;

    ValuePtr getValue(Token t)
    {
        if (t.type == Token::T_ConstValue) return t.value;
        std::string vname = t.value->toString();
        ValuePtr v = getLocal(vname);
        if (v != NULL) return v;
        return getGlobal(vname);
    }
    void setValue(Token t, ValuePtr v)
    {
        std::string vname = t.value->toString();
        if (!setLocal(vname, v)) setGlobal(vname, v);
    }

    void declareLocal(const std::string &vname, ValuePtr v)
    {
        stack.contexts.back().vars[vname] = v;
    }
    ValuePtr getLocal(const std::string &vname)
    {
        Context &locals = stack.contexts.back();
        if (locals.vars.count(vname) > 0) return locals.vars[vname];
        return ValuePtr();
    }
    bool setLocal(const std::string& vname, ValuePtr v)
    {
        Context &locals = stack.contexts.back();
        if (locals.vars.count(vname) > 0) {
            locals.vars[vname] = v;
            return true;
        }
        return false;
    }
    ValuePtr getGlobal(const std::string &vname)
    {
        return globalVars.vars[vname];
    }
    void setGlobal(const std::string &vname, ValuePtr v)
    {
        globalVars.vars[vname] = v;
    }
};

void CommandFunction::Call(GlobalEvrionment& g, const std::vector<ValuePtr>& args)
{
    Stack &s = g.stack;
    s.pcs.push_back(0);
    s.funcs.push_back(g.funcs[name]);
    s.contexts.push_back(Context());
    for (int i = 0; i < args.size(); ++i) {
        g.declareLocal(format("arg%d", i), args[i]);
    }
}
CommandPtr Command::create(Token* begin, Token *end)
{
    std::string ctype = begin->value->toString();
    Command *p = NULL;
    if (ctype == "add") p = new Command(T_Add, begin + 1, end);
    else if (ctype == "sub") p = new Command(T_Sub, begin + 1, end);
    else if (ctype == "mul") p = new Command(T_Mul, begin + 1, end);
    else if (ctype == "div") p = new Command(T_Div, begin + 1, end);
    else if (ctype == "assign") p = new Command(T_Assign, begin + 1, end);
    else if (ctype == "local") p = new Command(T_Local, begin + 1, end);
    else if (ctype == "call") p = new Command(T_Call, begin + 1, end);
    else if (ctype == "goto") p = new Command(T_Goto, begin + 1, end);
    else if (ctype == "lgoto") p = new Command(T_LessGoto, begin + 1, end);
    else if (ctype == "egoto") p = new Command(T_EqualGoto, begin + 1, end);
    else Assert(0);
    return CommandPtr(p);
}
void Command::apply(GlobalEvrionment &g)
{
    switch (m_type) {
        case T_Add:
            Assert(m_params[0].type == Token::T_Identify);
            g.getValue(m_params[0])->add(g.getValue(m_params[1]).get());
            break;
        case T_Sub:
            Assert(m_params[0].type == Token::T_Identify);
            g.getValue(m_params[0])->sub(g.getValue(m_params[1]).get());
            break;
        case T_Mul:
            Assert(m_params[0].type == Token::T_Identify);
            g.getValue(m_params[0])->mul(g.getValue(m_params[1]).get());
            break;
        case T_Div:
            Assert(m_params[0].type == Token::T_Identify);
            g.getValue(m_params[0])->div(g.getValue(m_params[1]).get());
            break;
        case T_Assign:
            Assert(m_params[0].type == Token::T_Identify);
            g.setValue(m_params[0], g.getValue(m_params[1])->clone());
            break;
        case T_Local:
            Assert(m_params[0].type == Token::T_Identify);
            g.declareLocal(m_params[0].value->toString(), g.getValue(m_params[1])->clone());
            break;
        case T_Call:
            {
                Assert(m_params[0].type == Token::T_Identify);
                std::vector<ValuePtr> args;
                for (int i = 1; i < m_params.size(); ++i) {
                    args.push_back(g.getValue(m_params[i])->clone());
                }
                g.funcs[m_params[0].value->toString()]->Call(g, args);
            }
            break;
        case T_Goto:
            {
                Assert(m_params[0].type == Token::T_Identify);
                CommandFunction* func = static_cast<CommandFunction*>(g.stack.funcs.back().get());
                g.stack.pcs.back() = func->labels[m_params[0].value->toString()];
            }
            break;
        case T_LessGoto:
            {
                if (g.getValue(m_params[0])->less(g.getValue(m_params[1]).get())) {
                    Assert(m_params[2].type == Token::T_Identify);
                    CommandFunction* func = static_cast<CommandFunction*>(g.stack.funcs.back().get());
                    g.stack.pcs.back() = func->labels[m_params[2].value->toString()];
                }
            }
            break;
        case T_EqualGoto:
            {
                if (g.getValue(m_params[0])->equal(g.getValue(m_params[1]).get())) {
                    Assert(m_params[2].type == Token::T_Identify);
                    CommandFunction* func = static_cast<CommandFunction*>(g.stack.funcs.back().get());
                    g.stack.pcs.back() = func->labels[m_params[2].value->toString()];
                }
            }
            break;
        default:
            Assert(0);
    }
}

void lexicalAnalysis(const std::string& _src, std::vector<Token>& tokens)
{
    for (const char *src = _src.c_str();;) {
        while (isspace(src[0])) ++src;
        if (src[0] == 0) break;
        if (isdigit(src[0])) {
            char *end;
            Token t = {Token::T_ConstValue, ValuePtr(new IntValue((int)strtod(src, &end)))};
            tokens.push_back(t);
            src = end;
        }
        else if (src[0] == '\'') {
            const char *end = ++src;
            while (end[0] != '\'') ++end;
            Token t = {Token::T_ConstValue, ValuePtr(new StringValue(std::string(src, end)))};
            tokens.push_back(t);
            src = end + 1;
        }
        else if (src[0] == '_' || isalpha(src[0])) {
            const char *end = src;
            do { ++end; } while(end[0] == '_' || isalpha(end[0]) || isdigit(end[0]));
            Token t = {Token::T_Identify, ValuePtr(new StringValue(std::string(src, end)))};
            tokens.push_back(t);
            src = end;
        }
        else {
            switch (src[0]) {
                case ';':
                case ':':
                case '{':
                case '}':
                    {
                        Token t = {Token::T_Notation, ValuePtr(new StringValue(std::string(src, src + 1)))};
                        tokens.push_back(t);
                        ++src;
                    }
                    break;
                default:
                    Assert(0);
            }
        }
    }
}
void syntaxAnalysis(GlobalEvrionment& g, std::vector<Token>& tokens)
{
    CommandFunction *buildingFunc = NULL;

    std::vector<Token> params;
    for (int i = 0; i < tokens.size(); ++i) {
        Token t = tokens[i];
        if (t.type == Token::T_Notation) {
            std::string tv = t.value->toString();
            if (tv == "{") {
                Assert(params.size() == 1 && params[0].type == Token::T_Identify);
                std::string fname = params[0].value->toString();
                buildingFunc = new CommandFunction();
                Assert(g.funcs.count(fname) == 0);
                g.funcs[fname].reset(buildingFunc);
                buildingFunc->name = fname;
            }
            else if (tv == "}") {
                buildingFunc = NULL;
            }
            else if (tv == ":") {
                Assert(params.size() == 1 && params[0].type == Token::T_Identify);
                std::string labelName = params[0].value->toString();
                Assert(buildingFunc->labels.count(labelName) == 0);
                buildingFunc->labels[labelName] = buildingFunc->cmds.size();
            }
            else if (tv == ";") {
                Assert(params.size() > 1 && params[0].type == Token::T_Identify);
                buildingFunc->cmds.push_back(Command::create(&params[0], &params[0] + params.size()));
            }
            else Assert(0);
            params.clear();
        }
        else params.push_back(t);
    }
}

void parseSource(const std::string& src, GlobalEvrionment &g)
{
    std::vector<Token> tokens;
    lexicalAnalysis(src, tokens);
    syntaxAnalysis(g, tokens);
}
void runMain(GlobalEvrionment &g)
{
    g.funcs["main"]->Call(g, std::vector<ValuePtr>());
    for (;;) {
        Stack& s = g.stack;
        if (s.pcs.empty()) break;
        int pc = s.pcs.back();
        CommandFunction* func = static_cast<CommandFunction*>(s.funcs.back().get());
        if (pc == func->cmds.size()) {
            s.pcs.pop_back();
            s.contexts.pop_back();
            s.funcs.pop_back();
            continue;
        }
        CommandPtr cmd = func->cmds[s.pcs.back()++];
        cmd->apply(g);
    }
}

std::string readFile(const std::string& fname)
{
    std::string r;
    std::ifstream fi(fname.c_str());
    for (std::string line; getline(fi, line); ) {
        r += line + '\n';
    }
    return r;
}

void buildinFunc_print(GlobalEvrionment& g, const std::vector<ValuePtr>& args)
{
    for (int i = 0; i < args.size(); ++i) cout << args[i]->toString() << '\t';
}
void buildinFunc_println(GlobalEvrionment& g, const std::vector<ValuePtr>& args)
{
    buildinFunc_print(g, args);
    cout << endl;
}
void registerBuildinFunctions(GlobalEvrionment &g)
{
    g.funcs["print"] = FunctionPtr(new CFunction(&buildinFunc_print));
    g.funcs["println"] = FunctionPtr(new CFunction(&buildinFunc_println));
}

int main()
{
    try
    {
        GlobalEvrionment g;
        parseSource(readFile("1.txt"), g);
        registerBuildinFunctions(g);
        runMain(g);
    }
    catch (const std::exception& e) {
        cout << "Exception : " << e.what() << endl;
    }
}
