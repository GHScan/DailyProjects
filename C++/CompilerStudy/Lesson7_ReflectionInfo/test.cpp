
#include "pch.h" 

#include <assert.h>

#include <vector>
#include <string>
#include <exception>
#include <fstream>
#include <map>

/*
   program = programunit program | programunit
   programunit = func | class | struct
   class = class type { fieldorfuncs };
   fieldorfuncs = fieldorfunc fieldorfuncs | fieldorfunc | e
   fieldorfunc = func | field
   struct = struct type { fields };
   func = typeval(args) funcbody
   funcbody = { funcbodys nobrace }
   funcbodys = nobrace funcbody funcbodys | e
   args = typeval , args | typeval | e
   fields = field | field fields | e
   field = typeval ;
   typeval = type id
   types = type | type , types
   type = id rtype 
   rtype = e | ::type | <types>
   nobrace = [^{}]*
   id = [_a-zA-Z]\w*
*/

struct TypeVal
{
    std::string tname, valname;
};
struct Func
{
    TypeVal ret_name;
    std::vector<TypeVal> args;
};
struct Struct
{
    std::string name;
    std::vector<TypeVal> fields;
};
struct Class
{
    std::string name;
    std::vector<TypeVal> fields;
    std::vector<Func> funcs;
};
struct Program
{
    std::map<std::string, Func> funcs;
    std::map<std::string, Struct> structs;
    std::map<std::string, Class> classs;
};

enum TokenType{
    TT_id = 1,
    TT_notation,
};
struct Token
{
    TokenType type;
    std::string value;
    int line : 22;
    int col : 10;
};

class MyException:
    public std::exception
{
public:
    MyException(const char *fname, int line, const std::string& pfname, const Token& t)
    {
        char buf[256];
        sprintf(buf, "%s(%d) : %s(%d,%d) -> %s", fname, line, pfname.c_str(), t.line, t.col, t.value.c_str());
        m_s = buf;
    }
    ~MyException() throw() {}
    const char *what() const throw() { return m_s.c_str(); }
private:
    std::string m_s;
};
#define PARSE_ASSERT(b) if (b); else throw MyException(__FILE__, __LINE__, m_fname, m_tokens[m_curToken])
#define ASSERT(b) assert(b)

class Parser
{
public:
    Parser(const std::string& fname, const std::string& src):
        m_fname(fname)
    {
        lexicalAnalysis(src);
        program();
    }
    const Program& getProgram() const { return m_program; }
private:
    void lexicalAnalysis(const std::string& _src)
    {
        m_tokens.clear();
        m_curToken = 0;
        const char *lineHead = _src.c_str();
        bool isComment = false;
        int line = 1;
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

            if (src[0] == '/' && src[1] == '/') {
                isComment = true;
                ++src;
            }
            if (isComment) {
                ++src;
                continue;
            }

            int col = src - lineHead + 1;
            if (isalpha(src[0]) || src[0] == '_') {
                const char *end = src + 1;
                while (isalpha(end[0]) || end[0] == '_' || isdigit(end[0])) ++end;
                Token t = {TT_id, std::string(src, end), line, col};
                m_tokens.push_back(t);
                src = end;
            }
            else {
                Token t = {TT_notation, std::string(src, src + 1), line, col};
                m_tokens.push_back(t);
                ++src;
            }
        }
    }
private:
    void program()
    {
        while (hasMoreToken()) {
            PARSE_ASSERT(programunit());
        }
    }
    bool programunit()
    {
        Class c;
        if (_class(c)) {
            m_program.classs[c.name] = c;
            return true;
        }
        Struct s;
        if (_struct(s)) {
            m_program.structs[s.name] = s;
            return true;
        }
        Func f;
        if (func(f)) {
            m_program.funcs[f.ret_name.valname] = f;
            return true;
        }
        return false;
    }
    bool _class(Class& c)
    {
        if (!tryConsumeToken(TT_id, "class")) return false;
        consumeToken(TT_id);
        c.name = getPreToken().value;
        consumeToken(TT_notation, "{");
        fieldorfuncs(c);
        consumeToken(TT_notation, "}");
        consumeToken(TT_notation, ";");
    }
    bool _struct(Struct &s)
    {
        if (!tryConsumeToken(TT_id, "struct")) return false;
        consumeToken(TT_id);
        s.name = getPreToken().value;
        consumeToken(TT_notation, "{");
        TypeVal tv;
        while (field(tv)) {
            s.fields.push_back(tv);
        }
        consumeToken(TT_notation, "}");
        consumeToken(TT_notation, ";");
    }
    void fieldorfuncs(Class& c)
    {
        while (fieldorfunc(c));
    }
    bool fieldorfunc(Class& c)
    {
        Func f;
        if (func(f)) {
            c.funcs.push_back(f);
            return true;
        }
        TypeVal tv;
        if (field(tv)) {
            c.fields.push_back(tv);
            return true;
        }
        return false;
    }
    bool func(Func& f)
    {
        backupTokenPos();
        if (typeval(f.ret_name) && tryConsumeToken(TT_notation, "(")) {
            discardTokenPosBackup();
            args(f.args);
            consumeToken(TT_notation, ")");
            funcbody(false);
            return true;
        }
        restoreTokenPos();
        return false;
    }
    void args(std::vector<TypeVal>& args)
    {
        TypeVal tv;
        while (typeval(tv)) {
            args.push_back(tv);
            if (!tryConsumeToken(TT_notation, ",")) break;
        }
    }
    bool funcbody(bool test)
    {
        if (tryConsumeToken(TT_notation, "{")) {
            funcbodys();
            nobrace();
            consumeToken(TT_notation, "}");
            return true;
        }
        else {
            if (!test) PARSE_ASSERT(0);
            return false;
        }
    }
    void funcbodys()
    {
        do {
            nobrace();
        } while(funcbody(true));
    }
    bool field(TypeVal &tv)
    {
        backupTokenPos();
        if (typeval(tv) && tryConsumeToken(TT_notation, ";")) {
            discardTokenPosBackup();
            return true;
        }
        restoreTokenPos();
        return false;
    }
    bool typeval(TypeVal &tv)
    {
        backupTokenPos();
        if (type(tv.tname)) {
            if (tryConsumeToken(TT_id)) {
                tv.valname = getPreToken().value;
                discardTokenPosBackup();
                return true;
            }
        }
        restoreTokenPos();
        return false;
    }
    void types(std::string& name)
    {
        type(name);
        while (tryConsumeToken(TT_notation, ",")) {
            std::string rname;
            type(rname);
            name += "," + rname;
        }
    }
    bool type(std::string& tname)
    {
        if (tryConsumeToken(TT_id)) {
            tname = getPreToken().value;
            rtype(tname);
            return true;
        }
        return false;
    }
    bool rtype(std::string& tname)
    {
        if (tryConsumeToken(TT_notation, ":")) {
            consumeToken(TT_notation, ":");
            std::string name;
            type(name);
            tname += "::" + name;
            return true;
        }
        else if (tryConsumeToken(TT_notation, "<")) {
            std::string name;
            types(name);
            tname += "<" + name + ">";
            consumeToken(TT_notation, ">");
            return true;
        }
        return false;
    }
    void nobrace()
    {
        while (hasMoreToken() && 
                !tryConsumeToken(TT_notation, "{") && !tryConsumeToken(TT_notation, "}")) {
            ++m_curToken;
        }
        --m_curToken;
    }

private:
    bool tryConsumeToken(TokenType t)
    {
        if (hasMoreToken() && t == m_tokens[m_curToken].type) {
            ++m_curToken;
            return true;
        }
        return false;
    }
    bool tryConsumeToken(TokenType t, const std::string& str)
    {
        if (hasMoreToken() &&
                t == m_tokens[m_curToken].type && str == m_tokens[m_curToken].value) {
            ++m_curToken;
            return true;
        }
        return false;
    }
    void consumeToken(TokenType t) { PARSE_ASSERT(tryConsumeToken(t));}
    void consumeToken(TokenType t, const std::string& str) { PARSE_ASSERT(tryConsumeToken(t, str));}
    const Token& getPreToken() const 
    { 
        PARSE_ASSERT(hasMoreToken());
        return m_tokens[m_curToken - 1]; 
    }
    bool hasMoreToken() const { return m_curToken < (int)m_tokens.size();}
    void backupTokenPos() { m_backupTokenPos.push_back(m_curToken); }
    void discardTokenPosBackup() { m_backupTokenPos.pop_back(); }
    void restoreTokenPos() { m_curToken = m_backupTokenPos.back(); m_backupTokenPos.pop_back() ;}

private:
    std::vector<Token> m_tokens;
    std::vector<int> m_backupTokenPos;
    int m_curToken;
    std::string m_fname;
    Program m_program;
};

void printTab(int n) 
{
    for (int i = 0; i < n; ++i) cout << '\t';
}
void visitTypeVal(const TypeVal& tv, int ntab)
{
    printTab(ntab);
    cout << tv.tname << " , " << tv.valname << endl;
}
void visitFunc(const Func& f, int ntab)
{
    printTab(ntab);
    cout << "--------------------\n";
    printTab(ntab);
    cout << "func : " << f.ret_name.valname << "\n";
    printTab(ntab);
    cout << "ret : " << f.ret_name.tname << "\n";
    printTab(ntab);
    cout << "args : \n";
    for (int i = 0; i < f.args.size(); ++i) {
        visitTypeVal(f.args[i], ntab + 1);
    }
}
void visitClass(const Class& c, int ntab)
{
    cout << "==============================\n";
    printTab(ntab);
    cout << "class :" << c.name << '\n';
    printTab(ntab);
    cout << "fields :\n";
    for (int i = 0; i < c.fields.size(); ++i) {
        visitTypeVal(c.fields[i], ntab + 1);
    }
    printTab(ntab);
    cout << "methods :\n";
    for (int i = 0; i < c.funcs.size(); ++i) {
        visitFunc(c.funcs[i], ntab + 1);
    }
}
void visitStruct(const Struct& s, int ntab)
{
    cout << "==============================\n";
    printTab(ntab);
    cout << "struct :" << s.name << '\n';
    printTab(ntab);
    cout << "fields :\n";
    for (int i = 0; i < s.fields.size(); ++i) {
        visitTypeVal(s.fields[i], ntab + 1);
    }
}
void visitProgram(const Program& program)
{
    cout << "funcs:\n";
    for (std::map<std::string, Func>::const_iterator iter = program.funcs.begin();
            iter != program.funcs.end(); 
            ++iter) {
        visitFunc(iter->second, 1);
    }
    cout << "structs:\n";
    for (std::map<std::string, Struct>::const_iterator iter = program.structs.begin();
            iter != program.structs.end(); 
            ++iter) {
        visitStruct(iter->second, 1);
    }
    cout << "classs:\n";
    for (std::map<std::string, Class>::const_iterator iter = program.classs.begin();
            iter != program.classs.end(); 
            ++iter) {
        visitClass(iter->second, 1);
    }
}

std::string readFile(const std::string& fname)
{
    std::ifstream fi(fname.c_str());
    std::string r;
    for (std::string line; getline(fi, line); r += line + '\n');
    return r;
}

int main()
{
    try
    {
        std::string fname = "1.txt";
        Parser p(fname, readFile(fname));
        visitProgram(p.getProgram());
    }
    catch(const std::exception& e) {
        cout << "Exception : " << e.what() << endl;
    }
}
