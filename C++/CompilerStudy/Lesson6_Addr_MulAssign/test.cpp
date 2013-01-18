#include "pch.h" 

#include <assert.h>

#include <memory>
#include <string>
#include <fstream>
#include <vector>
#include <map>

class Value
{
public:
    Value(int i): m_i(i){}
    Value(const std::string &str):m_i(atoi(str.c_str())) {}
    void add(const Value* o){ m_i += o->m_i; }
    void sub(const Value* o){ m_i -= o->m_i; }
    int getValue() const { return m_i; }
    void setValue(int i) { m_i = i; }
    Value* clone() { return new Value(m_i); }
private:
    int m_i;
};
typedef std::shared_ptr<Value> ValuePtr;

struct ExprNode_Num;
struct ExprNode_Addr;
struct ExprNode_BinOp;
struct StmtNode_Assign;
struct StmtNode_Call;
struct ISyntaxNodeVisitor
{
    virtual ~ISyntaxNodeVisitor(){}
    virtual void visit(ExprNode_Num *node) = 0;
    virtual void visit(ExprNode_Addr *node) = 0;
    virtual void visit(ExprNode_BinOp *node) = 0;
    virtual void visit(StmtNode_Assign *node) = 0;
    virtual void visit(StmtNode_Call *node) = 0;
};

struct IExprNode
{
    virtual ~IExprNode(){}
    virtual void acceptVisitor(ISyntaxNodeVisitor* v) = 0;
};
typedef std::shared_ptr<IExprNode>  ExprNodePtr;
struct IStmtNode
{
    virtual ~IStmtNode(){}
    virtual void acceptVisitor(ISyntaxNodeVisitor* v) = 0;
};
typedef std::shared_ptr<IStmtNode> StmtNodePtr;

struct ExprNode_Num:
    public IExprNode
{
    ValuePtr v;
    ExprNode_Num(ValuePtr v) { this->v = v; }
    virtual void acceptVisitor(ISyntaxNodeVisitor* v) { v->visit(this) ;}
};
struct ExprNode_Addr:
    public IExprNode
{
    std::string name;
    bool lval;
    ExprNodePtr pos;
    ExprNode_Addr(const std::string& name, bool lval) { this->name = name; this->lval = lval; }
    virtual void acceptVisitor(ISyntaxNodeVisitor* v) { v->visit(this) ;}
};
struct ExprNode_BinOp:
    public IExprNode
{
    std::string op;
    ExprNodePtr left, right;
    ExprNode_BinOp(const std::string& op){ this->op = op; }
    virtual void acceptVisitor(ISyntaxNodeVisitor* v) { v->visit(this) ;}
};
struct StmtNode_Assign:
    public IStmtNode
{
    std::vector<ExprNodePtr> ltuple;
    std::vector<ExprNodePtr> rtuple;
    virtual void acceptVisitor(ISyntaxNodeVisitor* v) { v->visit(this) ;}
};
struct StmtNode_Call:
    public IStmtNode
{
    std::string fname;
    std::vector<ExprNodePtr> rtuple;
    StmtNode_Call(const std::string& fname) {this->fname = fname;}
    virtual void acceptVisitor(ISyntaxNodeVisitor* v) { v->visit(this) ;}
};

enum TokenType
{
    TT_notation = 1,
    TT_id,
    TT_number,
};
struct Token
{
    TokenType type;
    std::string lexeme;
    int line : 22;
    int col : 10;
};

class ParseException:
    public std::exception
{
public:
    ParseException(const char *fname, int line, const std::string& pfile, const Token& t)
    {
        char buf[256];
        sprintf(buf, "%s(%d) : %s(%d,%d) -> %s", fname, line, pfile.c_str(), t.line, t.col, t.lexeme.c_str());
        m_s = buf;
    }
    ~ParseException() throw() {}
    const char* what() const throw() { return m_s.c_str(); }
private:
    std::string m_s;
};
#define PARSE_ASSERT(b) if(b); else throw ParseException(__FILE__, __LINE__, m_fname, m_tokens[m_curToken])

/*
    stmts = stmts stmt
    stmt =  
        id(rtuple);
        ltuple = rtuple;
    rtuple = rval , rtuple | rval
    ltuple = lval , ltuple | lval
    lval = addr
    rval = add
    add = add + factor | add - factor | factor
    factor = addr | num
    addr = id[rval]
    id = [a-zA-Z]+
    num = [1-9][0-9]*
*/
/*
    stmts = stmt stmts | e
    stmt = 
        id(rtuple);
        ltuple = rtuple;
    rtuple = rval , rtuple | rval
    ltuple = lval , ltuple | lval
    lval = laddr
    rval = add
    add = factor _add
    _add = + factor _add | - factor _add | e
    factor = raddr | num
    laddr = id[rval]
    raddr = id[rval]
    id = [a-zA-Z]+
    num = [1-9][0-9]*
*/

class Parser
{
public:
    bool parse(const std::string& fname, const std::string& src)
    {
        m_fname = fname;
        lexicalAnalysis(src);
        stmts();
        m_tokens.clear();
        return true;
    }
    void acceptVisitor(ISyntaxNodeVisitor* v)
    {
        for (size_t i = 0; i < m_stmts.size(); ++i) m_stmts[i]->acceptVisitor(v);
    }

private:
    void lexicalAnalysis(const std::string& _src)
    {
        m_curToken = 0;
        m_tokens.clear();
        int line = 1;
        bool isComment = false;
        const char *lineHead = _src.c_str();
        for (const char* src = _src.c_str();;) {
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
                ++src;
                isComment = true;
            }
            if (isComment) {
                ++src;
                continue;
            }

            int col = int(src - lineHead) + 1;
            if (isdigit(src[0])) {
                const char *end = src;
                while (isdigit(*++end));
                Token t = {TT_number, std::string(src, end), line, col};
                m_tokens.push_back(t);
                src = end;
            }
            else if (isalpha(src[0])) {
                const char *end = src;
                while (isalpha(*++end));
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
    void stmts()
    {
        StmtNodePtr p = stmt();
        if (p != NULL) {
            m_stmts.push_back(p);
            stmts();
        }
    }
    StmtNodePtr stmt()
    {
        StmtNodePtr r;
        if (!hasMoreToken()) return r;

        backupTokenPos();
        std::string name = getCurToken().lexeme;
        if (tryConsumeToken(TT_id) && tryConsumeToken(TT_notation, "(")) {
            discardTokenPosBackup();
            StmtNode_Call *p = new StmtNode_Call(name);
            r.reset(p);
            rtuple(p->rtuple);
            consumeToken(TT_notation, ")");
            consumeToken(TT_notation, ";");
            return r;
        }
        restoreTokenPos();

        StmtNode_Assign *p = new StmtNode_Assign();
        r.reset(p);
        ltuple(p->ltuple);
        if (p->ltuple.empty()) {
            r.reset();
        }
        else {
            consumeToken(TT_notation, "=");
            rtuple(p->rtuple);
            consumeToken(TT_notation, ";");
        }
        return r;
    }
    void rtuple(std::vector<ExprNodePtr>& rvals)
    {
        ExprNodePtr p = rval();
        if (p != NULL) {
            rvals.push_back(p);
            if (tryConsumeToken(TT_notation, ",")) {
                rtuple(rvals);
            }
        }
    }
    void ltuple(std::vector<ExprNodePtr>& lvals)
    {
        ExprNodePtr p = lval();
        if (p != NULL) {
            lvals.push_back(p);
            if (tryConsumeToken(TT_notation, ",")) {
                ltuple(lvals);
            }
        }
    }
    ExprNodePtr lval()
    {
        return addr(true);
    }
    ExprNodePtr rval()
    {
        return add();
    }
    ExprNodePtr add()
    {
        ExprNodePtr p = factor();
        if (p != NULL) return _add(p);
        return p;
    }
    ExprNodePtr _add(ExprNodePtr left)
    {
        if (tryConsumeToken(TT_notation, "+") || tryConsumeToken(TT_notation, "-")) {
            std::string op = getPreToken().lexeme;
            ExprNodePtr p = factor();
            ExprNode_BinOp *node = new ExprNode_BinOp(op);
            ExprNodePtr r(node);
            node->left = left;
            node->right = p;
            return _add(r);
        }
        else return left;
    }
    ExprNodePtr factor()
    {
        ExprNodePtr p = num();
        if (p != NULL) return p;
        return addr(false);
    }
    ExprNodePtr addr(bool l)
    {
        backupTokenPos();
        std::string name = getCurToken().lexeme;
        if (tryConsumeToken(TT_id) && tryConsumeToken(TT_notation, "[")) {
            discardTokenPosBackup();
            ExprNode_Addr *p = new ExprNode_Addr(name, l);
            ExprNodePtr r(p);
            p->pos = rval();
            PARSE_ASSERT(p->pos != NULL);
            consumeToken(TT_notation, "]");
            return r;
        }
        restoreTokenPos();
        return ExprNodePtr();
    }
    ExprNodePtr num()
    {
        if (tryConsumeToken(TT_number)) {
            return ExprNodePtr(
                    new ExprNode_Num(ValuePtr(new Value(getPreToken().lexeme))));
        }
        return ExprNodePtr();
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
                t == m_tokens[m_curToken].type && str == m_tokens[m_curToken].lexeme) {
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
    const Token& getCurToken() const 
    {
        PARSE_ASSERT(hasMoreToken());
        return m_tokens[m_curToken] ;
    }
    bool hasMoreToken() const { return m_curToken < (int)m_tokens.size();}
    void backupTokenPos() { m_backupTokenPos.push_back(m_curToken); }
    void discardTokenPosBackup() { m_backupTokenPos.pop_back(); }
    void restoreTokenPos() { m_curToken = m_backupTokenPos.back(); m_backupTokenPos.pop_back() ;}

private:
    std::string m_fname;
    std::vector<Token> m_tokens;
    int  m_curToken;
    std::vector<int> m_backupTokenPos;
    std::vector<StmtNodePtr> m_stmts;
};

typedef std::map<int, ValuePtr> Array;

typedef void(*FunctionT)(const std::vector<ValuePtr>& params);

struct GlobalEnv
{
    std::map<std::string, Array> arrays;
    std::map<std::string, FunctionT> funcs;
};

class ASTVisitor:
    public ISyntaxNodeVisitor
{
public:
    ASTVisitor(GlobalEnv &g): m_g(g){}
private:
    virtual void visit(ExprNode_Num *node)
    {
        m_valueStack.push_back(ValuePtr(node->v->clone()));
    }
    virtual void visit(ExprNode_Addr *node)
    {
        ValuePtr r = eval(node->pos);
        ValuePtr &v = m_g.arrays[node->name][r->getValue()];
        if (v == NULL) v.reset(new Value(0));
        if (node->lval) r = v;
        else r = ValuePtr(v->clone());
        m_valueStack.push_back(r);
    }
    virtual void visit(ExprNode_BinOp *node)
    {
        ValuePtr l = eval(node->left);
        ValuePtr r = eval(node->right);
        if (node->op == "+") {
            l->add(r.get());
            m_valueStack.push_back(l);
        }
        else if (node->op == "-") {
            l->sub(r.get());
            m_valueStack.push_back(l);
        }
        else assert(0);
    }
    virtual void visit(StmtNode_Assign *node)
    {
        m_valueStack.clear();
        std::vector<ValuePtr> lvals;
        std::vector<ValuePtr> rvals;
        for (size_t i = 0; i < node->ltuple.size(); ++i) {
            lvals.push_back(eval(node->ltuple[i]));
        }
        for (size_t i = 0; i < node->rtuple.size(); ++i) {
            rvals.push_back(eval(node->rtuple[i]));
        }
        for (size_t i = 0; i < lvals.size(); ++i) {
            if (i < rvals.size()) lvals[i]->setValue(rvals[i]->getValue());
            else lvals[i]->setValue(0);
        }
    }
    virtual void visit(StmtNode_Call *node)
    {
        m_valueStack.clear();
        std::vector<ValuePtr> args;
        for (size_t i = 0; i < node->rtuple.size(); ++i) {
            args.push_back(eval(node->rtuple[i]));
        }
        m_g.funcs[node->fname](args);
    }
private:
    ValuePtr eval(ExprNodePtr p)
    {
        p->acceptVisitor(this);
        ValuePtr r = m_valueStack.back();
        m_valueStack.pop_back();
        return r;
    }

private:
    GlobalEnv &m_g;
    std::vector<ValuePtr> m_valueStack;
};

std::string readFile(const std::string& fname)
{
    std::ifstream fi(fname.c_str());
    std::string r;
    for (std::string line; getline(fi, line); r += line + '\n');
    return r;
}

void buildin_println(const std::vector<ValuePtr>& args)
{
    for (size_t i = 0; i < args.size(); ++i) cout << args[i]->getValue() << '\t';
    cout << endl;
}

void registerFunction(GlobalEnv &g)
{
    g.funcs["println"] = &buildin_println;
}

int main()
{
    try
    {
        Parser p;
        std::string fname("1.txt");
        p.parse(fname, readFile(fname));

        GlobalEnv g;
        registerFunction(g);
        ASTVisitor v(g);
        p.acceptVisitor(&v);
    }
    catch (const std::exception& e) {
        cout << "Exception : " << e.what() << endl;
    }
}
