#include "pch.h"

#include <algorithm>

#include "common.h"
#include "parser.h"

/*
    program = func program | func
    func = function id(args) { stmts }
    args = validargs | e
    validargs = id, validargs | id
    stmts = validstmts | e
    validstmts = validstmts stmt | stmt
    stmt = expr;
           { stmts }
           if (validexpr) stmt
           if (validexpr) stmt else stmt
           while (validexpr) stmt
           do stmt while(validexpr);
           for (expr; expr; expr) stmt
           local id;
           local id = bin_assign;
           return expr;
           break;
           continue;
    expr = validexpr | e
    validexpr = bin_assign
    bin_assign = id = bin_assign | id *= bin_assign | id /= bin_assign | id %= bin_assign | id += bin_assign | id -= bin_assign | bin_and
    bin_and = bin_and && bin_eql | bin_and || bin_eql | bin_eql
    bin_eql = bin_eql == bin_cmp | bin_eql != bin_cmp | bin_cmp
    bin_cmp = bin_cmp < bin_add | bin_cmp <= bin_add | bin_cmp > bin_add | bin_cmp >= bin_add | bin_add
    bin_add = bin_add + bin_mul | bin_add - bin_mul | bin_mul
    bin_mul = bin_mul * unary | bin_mul / unary | bin_mul % unary | unary
    unary = !factor | --id | ++id | factor
    factor = id | const | (validexpr) | call
    call = id(params)
    params = validparams | e
    validparams = validexpr, validparams | validexpr
    id = [_a-zA-Z]\w*
    const = string | number
    string = '.*'
    number = [1-9][0-9]*
*/

/*
    program = func program | func
    func = function id(args) { stmts }
    args = validargs | e
    validargs = id, validargs | id
    stmts = validstmts | e
    validstmts = stmt rvalidstmts
    rvalidstmts = stmt rvalidstmts | e
    stmt = { stmts }
           if (validexpr) stmt
           if (validexpr) stmt else stmt
           while (validexpr) stmt
           do stmt while(validexpr);
           for (expr; expr; expr) stmt
           local id;
           local id = bin_assign;
           return expr;
           break;
           continue;
           expr;
    expr = validexpr | e
    validexpr = bin_assign
    bin_assign = id = bin_assign | id *= bin_assign | id /= bin_assign | id %= bin_assign | id += bin_assign | id -= bin_assign | bin_and
    bin_and = bin_eql rbin_and
    rbin_and = && bin_eql rbin_and | || bin_eql rbin_and | e
    bin_eql = bin_cmp rbin_eql
    rbin_eql = == bin_cmp rbin_eql | != bin_cmp rbin_eql | e
    bin_cmp = bin_add rbin_cmp
    rbin_cmp = < bin_add rbin_cmp | <= bin_add rbin_cmp | > bin_add rbin_cmp | >= bin_add rbin_cmp | e
    bin_add = bin_mul rbin_add
    rbin_add = + bin_mul rbin_add | - bin_mul rbin_add | e
    bin_mul = unary rbin_mul
    rbin_mul = * unary rbin_mul | / unary rbin_mul | % unary rbin_mul
    unary = !factor | --id | ++id | factor
    factor =  (validexpr) | call | id | const
    call = id(params)
    params = validparams | e
    validparams = validexpr, validparams | validexpr
    id = [_a-zA-Z]\w*
    const = string | number
    string = '.*'
    number = [1-9][0-9]*
*/

enum TokenType 
{
    TT_notation = 1,
    // TT_keyword,
    TT_id,
    TT_string,
    TT_number,
};
struct Token
{
    Token(TokenType t, const char *begin, const char *end, int l):
        type(t), value(begin, end), line(l){}
    TokenType type;
    std::string value;
    int line;
};

class ParserException:
    public std::exception
{
public:
    ParserException(const char *fname, int line, const char* pfname, const Token& t)
    {
        char buf[512];
        sprintf(buf, "%s(%d) : %s-%d, %s", fname, line, pfname, t.line, t.value.c_str());
        m_s = buf;
    }
    ~ParserException() throw() {}
    const char* what() const throw() { return m_s.c_str(); }
private:
    std::string m_s;
};
#define PARSER_ASSERT(b) if (b); else throw ParserException(__FILE__, __LINE__, m_fname.c_str(), m_tokens[m_curToken])
#undef ASSERT
#define ASSERT PARSER_ASSERT

class ParserImpl
{
public:
    ParserImpl(){}
    ~ParserImpl(){}
    bool parse(const std::string& fname, const std::string& src)
    {
        m_fname = fname;
        lexicalAnalysis(src);
        program();
        return true;
    }
    const std::vector<FuncSyntaxPtr> &getFuncs() const { return m_funcs; }
private:
    void lexicalAnalysis(const std::string& _src)
    {
        m_tokens.clear();
        m_curToken = 0;
        int line = 1;
        bool isComment = false;
        for (const char *src = _src.c_str();;) {
            while (isspace(src[0])) {
                line += src[0] == '\n' ? 1 : 0;
                if (isComment && src[0] == '\n') isComment = false;
                ++src;
            }

            if (src[0] == '/' && src[1] == '/') {
                ++src;
                isComment = true;
            }
            if (isComment) {
                ++src;
                continue;
            }

            if (src[0] == 0) break;
            if (isdigit(src[0])) {
                const char *end = src;
                while (isdigit(*++end));
                m_tokens.push_back(Token(TT_number, src, end, line));
                src = end;
            }
            else if(src[0] == '\'') {
                const char *end = src;
                while (*++end != '\'');
                m_tokens.push_back(Token(TT_string, src + 1, end, line));
                src = end + 1;
            }
            else if (src[0] == '_' || isalpha(src[0])) {
                const char *end = src + 1;
                while (isdigit(end[0]) || isalpha(end[0]) || end[0] == '_') ++end;
                m_tokens.push_back(Token(TT_id, src, end, line));
                src = end;
            }
            else {
                int opLen = 1;
                switch (src[1]) {
                    case '=':
                        switch (src[0]) {
                            case '+': case '-': 
                            case '*': case '/': case '%':
                            case '<': case '>': case '=':
                                opLen = 2;
                                break;
                        }
                        break;
                    case '+':
                        if (src[0] == '+') opLen = 2;
                        break;
                    case '-':
                        if (src[0] == '-') opLen = 2;
                        break;
                    case '|':
                        if (src[0] == '|') opLen = 2;
                        break;
                    case '&':
                        if (src[0] == '&') opLen = 2;
                        break;
                    default:
                        break;
                }
                m_tokens.push_back(Token(TT_notation, src, src + opLen, line));
                src += opLen;
            }
        }
    }
private:
    void program()
    {
        func();
        if (hasMoreToken()) program();
    }
    void func()
    {
        consumeToken(TT_id, "function");
        consumeToken(TT_id);
        FuncSyntax *f = new FuncSyntax(getLastToken().value);
        consumeToken(TT_notation, "(");
        f->args = args();
        consumeToken(TT_notation, ")");
        consumeToken(TT_notation, "{");
        f->stmts = stmts();
        consumeToken(TT_notation, "}");
        m_funcs.push_back(FuncSyntaxPtr(f));
    }
    StmtNodePtr args()
    {
        StmtNode_Stmts *argsNode = new StmtNode_Stmts;
        StmtNodePtr r(argsNode);
        validargs(true, argsNode);
        return r;
    }
    void validargs(bool test, StmtNode_Stmts *argsNode)
    {
        if (tryConsumeToken(TT_id)) {
            argsNode->stmts.push_back(
                    StmtNodePtr(new StmtNode_Local(getLastToken().value)));
            if (tryConsumeToken(TT_notation, ",")) {
                validargs(false, argsNode);
            }
        }
        else {
            if (!test) ASSERT(0);
        }
    }
    StmtNodePtr stmts()
    {
        StmtNode_Stmts *stmtsNode = new StmtNode_Stmts();
        StmtNodePtr r(stmtsNode);
        validstmts(true, stmtsNode);
        return r;
    }
    void validstmts(bool test, StmtNode_Stmts *stmtsNode)
    {
        StmtNodePtr node = stmt();
        if (node == NULL) {
            if (!test) ASSERT(0);
        }
        else {
            stmtsNode->stmts.push_back(node);
            rvalidstmts(stmtsNode);
        }
    }
    void rvalidstmts(StmtNode_Stmts *stmtsNode)
    {
        StmtNodePtr node = stmt();
        if (node != NULL) {
            stmtsNode->stmts.push_back(node);
            rvalidstmts(stmtsNode);
        }
    }
    StmtNodePtr stmt()
    {
        StmtNodePtr r;
        if (tryConsumeToken(TT_notation, "{")) {
            r = stmts();
            consumeToken(TT_notation, "}");
        }
        else if (tryConsumeToken(TT_id, "if")) {
            StmtNode_IfElse* p = new StmtNode_IfElse();
            r.reset(p);
            consumeToken(TT_notation, "(");
            p->expr = validexpr(false);
            consumeToken(TT_notation, ")");
            p->ifStmt = stmt();
            if (tryConsumeToken(TT_id, "else")) {
                p->elseStmt = stmt();
            }
            else ;
        }
        else if (tryConsumeToken(TT_id, "while")) {
            StmtNode_While *p = new StmtNode_While();
            r.reset(p);
            consumeToken(TT_notation, "(");
            p->expr = validexpr(false);
            consumeToken(TT_notation, ")");
            p->stmt = stmt();
        }
        else if (tryConsumeToken(TT_id, "do")) {
            StmtNode_Stmts *p = new StmtNode_Stmts();
            r.reset(p);
            StmtNode_While *stmtWhile = new StmtNode_While();

            StmtNodePtr st = stmt();
            p->stmts.push_back(st);
            p->stmts.push_back(StmtNodePtr(stmtWhile));
            stmtWhile->stmt = st;

            consumeToken(TT_id, "while");
            consumeToken(TT_notation, "(");
            stmtWhile->expr = validexpr(false);
            consumeToken(TT_notation, ")");
            consumeToken(TT_notation, ";");
        }
        else if (tryConsumeToken(TT_id, "for")) {
            // FIXME : bug with "continue"
            StmtNode_Stmts *p = new StmtNode_Stmts();
            r.reset(p);
            StmtNode_ExprOnly *expr1 = new StmtNode_ExprOnly();
            StmtNode_While *stmtWhile = new StmtNode_While();
            StmtNode_ExprOnly *expr3 = new StmtNode_ExprOnly();
            StmtNode_Stmts *whileBody = new StmtNode_Stmts();
            p->stmts.push_back(StmtNodePtr(expr1));
            p->stmts.push_back(StmtNodePtr(stmtWhile));
            stmtWhile->stmt.reset(whileBody);
            whileBody->stmts.push_back(StmtNodePtr(expr3));

            consumeToken(TT_notation, "(");
            expr1->expr = expr();
            consumeToken(TT_notation, ";");
            stmtWhile->expr = expr();
            consumeToken(TT_notation, ";");
            expr3->expr = expr();
            consumeToken(TT_notation, ")");
            whileBody->stmts.push_back(stmt());

            std::swap(whileBody->stmts[0], whileBody->stmts[1]);
        }
        else if (tryConsumeToken(TT_id, "local")) {
            StmtNode_Local *p = new StmtNode_Local();
            r.reset(p);
            consumeToken(TT_id);
            p->left = getLastToken().value;
            if (tryConsumeToken(TT_notation, ";")) {
                ;
            }
            else {
                consumeToken(TT_notation, "=");
                p->right = bin_assign();
                consumeToken(TT_notation, ";");
            }
        }
        else if (tryConsumeToken(TT_id, "return")) {
            StmtNode_Return *p = new StmtNode_Return();
            r.reset(p);
            p->right = expr();
            consumeToken(TT_notation, ";");
        }
        else if (tryConsumeToken(TT_id, "break")) {
            StmtNode_Break *p = new StmtNode_Break();
            r.reset(p);
            consumeToken(TT_notation, ";");
        }
        else if (tryConsumeToken(TT_id, "continue")) {
            StmtNode_Continue *p = new StmtNode_Continue();
            r.reset(p);
            consumeToken(TT_notation, ";");
        }
        else {
            StmtNode_ExprOnly *p = new StmtNode_ExprOnly();
            r.reset(p);
            p->expr = expr();
            if (!tryConsumeToken(TT_notation, ";")) {
                r.reset();
            }
        }
        return r;
    }
    ExprNodePtr expr()
    {
        return validexpr(true);
    }
    ExprNodePtr validexpr(bool test)
    {
        ExprNodePtr r = bin_assign();
        if (r == NULL && !test) ASSERT(0);
        return r;
    }
    ExprNodePtr bin_assign()
    {
        ExprNodePtr r;
        backupTokenPos();
        std::string name = getCurToken().value;
        if (tryConsumeToken(TT_id) && (
                    tryConsumeToken(TT_notation, "=") ||
                    tryConsumeToken(TT_notation, "*=") ||
                    tryConsumeToken(TT_notation, "/=") ||
                    tryConsumeToken(TT_notation, "%=") ||
                    tryConsumeToken(TT_notation, "+=") ||
                    tryConsumeToken(TT_notation, "-="))) {
            ExprNode_Assign *p = new ExprNode_Assign(name);
            r.reset(p);
            if (getLastToken().value == "=") {
                p->right = bin_assign();
            }
            else {
                ExprNode_BinaryOp *op = new ExprNode_BinaryOp(getLastToken().value.substr(0, 1));
                p->right = ExprNodePtr(op);
                op->left = ExprNodePtr(new ExprNode_ID(name));
                op->right = bin_assign();
            }
            discardBackupTokenPos();
            return r;
        }
        else {
            restoreTokenPos();
            return bin_and();
        }
    }
    ExprNodePtr bin_and()
    {
        ExprNodePtr r = bin_eql();
        return rbin_and(r);
    }
    ExprNodePtr rbin_and(ExprNodePtr left)
    {
        if (tryConsumeToken(TT_notation, "&&") || tryConsumeToken(TT_notation, "||")) {
            ExprNode_BinaryOp *p = new ExprNode_BinaryOp(getLastToken().value);
            ExprNodePtr r(p);
            p->left = left;
            p->right = bin_eql();
            return rbin_and(r);
        }
        else return left;
    }
    ExprNodePtr bin_eql()
    {
        ExprNodePtr r = bin_cmp();
        return rbin_eql(r);
    }
    ExprNodePtr rbin_eql(ExprNodePtr left)
    {
        if (tryConsumeToken(TT_notation, "==") || tryConsumeToken(TT_notation, "!=")) {
            ExprNode_BinaryOp *p = new ExprNode_BinaryOp(getLastToken().value);
            ExprNodePtr r(p);
            p->left = left;
            p->right = bin_cmp();
            return rbin_eql(r);
        }
        else return left;
    }
    ExprNodePtr bin_cmp()
    {
        ExprNodePtr r = bin_add();
        return rbin_cmp(r);
    }
    ExprNodePtr rbin_cmp(ExprNodePtr left)
    {
        if (tryConsumeToken(TT_notation, "<") || tryConsumeToken(TT_notation, "<=") ||
            tryConsumeToken(TT_notation, ">") || tryConsumeToken(TT_notation, ">=")) {
            ExprNode_BinaryOp *p = new ExprNode_BinaryOp(getLastToken().value);
            ExprNodePtr r(p);
            p->left = left;
            p->right = bin_add();
            return rbin_cmp(r);
        }
        else return left;
    }
    ExprNodePtr bin_add()
    {
        ExprNodePtr r = bin_mul();
        return rbin_add(r);
    }
    ExprNodePtr rbin_add(ExprNodePtr left)
    {
        if (tryConsumeToken(TT_notation, "+") || tryConsumeToken(TT_notation, "-")) {
            ExprNode_BinaryOp *p = new ExprNode_BinaryOp(getLastToken().value);
            ExprNodePtr r(p);
            p->left = left;
            p->right = bin_mul();
            return rbin_add(r);
        }
        else return left;
    }
    ExprNodePtr bin_mul()
    {
        ExprNodePtr r = unary();
        return rbin_mul(r);
    }
    ExprNodePtr rbin_mul(ExprNodePtr left)
    {
        if (tryConsumeToken(TT_notation, "*") || 
            tryConsumeToken(TT_notation, "/") || 
            tryConsumeToken(TT_notation, "%")) {
            ExprNode_BinaryOp *p = new ExprNode_BinaryOp(getLastToken().value);
            ExprNodePtr r(p);
            p->left = left;
            p->right = unary();
            return rbin_mul(r);
        }
        else return left;
    }
    ExprNodePtr unary()
    {
        if (tryConsumeToken(TT_notation, "!")) {
            ExprNode_UnaryOp *p = new ExprNode_UnaryOp(getLastToken().value);
            ExprNodePtr r(p);
            p->child = factor();
            return r;
        }
        else if (tryConsumeToken(TT_notation, "--") || tryConsumeToken(TT_notation, "++")) {
            std::string op = getLastToken().value;
            consumeToken(TT_id);
            ExprNode_Assign *p = new ExprNode_Assign(getLastToken().value);
            ExprNodePtr r(p);
            ExprNode_BinaryOp *bop = new ExprNode_BinaryOp(op.substr(0, 1));
            p->right.reset(bop);
            bop->left = ExprNodePtr(new ExprNode_ID(p->left));
            bop->right = ExprNodePtr(new ExprNode_Const(ValuePtr(new NumValue(1))));
            return r;
        }
        else {
            return factor();
        }
    }
    ExprNodePtr factor()
    {
        if (tryConsumeToken(TT_notation, "(")) {
            ExprNodePtr p = validexpr(false);
            consumeToken(TT_notation, ")");
            return p;
        }
        ExprNodePtr r = call();
        if (r != NULL) return r;
        r = id();
        if (r != NULL) return r;
        return _const();
    }
    ExprNodePtr call()
    {
        backupTokenPos();
        ExprNodePtr r;
        std::string name = getCurToken().value;
        if (tryConsumeToken(TT_id) && tryConsumeToken(TT_notation, "(")) {
            ExprNode_Call *p = new ExprNode_Call(name);
            ExprNodePtr r(p);
            params(p->params);
            consumeToken(TT_notation, ")");
            discardBackupTokenPos();
            return r;
        }
        else {
            restoreTokenPos();
            return ExprNodePtr();
        }
    }
    void params(std::vector<ExprNodePtr>& params)
    {
        validparams(true, params);
    }
    void validparams(bool test, std::vector<ExprNodePtr>& params)
    {
        ExprNodePtr p = validexpr(true);
        if (p == NULL) {
            if (!test) ASSERT(0);
        }
        else {
            params.push_back(p);
            if (tryConsumeToken(TT_notation, ",")) {
                validparams(false, params);
            }
        }
    }
    ExprNodePtr id()
    {
        ExprNodePtr r;
        if (tryConsumeToken(TT_id)) {
            ExprNode_ID *p = new ExprNode_ID(getLastToken().value);
            r.reset(p);
        }
        return r;
    }
    ExprNodePtr _const()
    {
        ExprNodePtr p = string();
        if (p != NULL) return p;
        return number();
    }
    ExprNodePtr string()
    {
        ExprNodePtr r;
        if (tryConsumeToken(TT_string)) {
            ExprNode_Const *p = new ExprNode_Const(
                    ValuePtr(new StringValue(getLastToken().value)));
            r.reset(p);
        }
        return r;
    }
    ExprNodePtr number()
    {
        ExprNodePtr r;
        if (tryConsumeToken(TT_number)) {
            ExprNode_Const *p = new ExprNode_Const(
                    ValuePtr(new NumValue(getLastToken().value)));
            r.reset(p);
        }
        return r;
    }
private:
    void consumeToken(TokenType t, const std::string& str)
    {
        ASSERT(tryConsumeToken(t, str));
    }
    void consumeToken(TokenType t)
    {
        ASSERT(tryConsumeToken(t));
    }
    bool tryConsumeToken(TokenType t)
    {
        if (hasMoreToken() && m_tokens[m_curToken].type == t) {
            ++m_curToken;
            return true;
        }
        return false;
    }
    bool tryConsumeToken(TokenType t, const std::string& str)
    {
        if (hasMoreToken() && 
                m_tokens[m_curToken].type == t && m_tokens[m_curToken].value == str) {
            ++m_curToken;
            return true;
        }
        else return false;
    }
    bool hasMoreToken() const { return m_curToken < m_tokens.size(); }
    void backupTokenPos() { m_tokenPosBackup.push_back(m_curToken) ;}
    void restoreTokenPos() { m_curToken = m_tokenPosBackup.back(); m_tokenPosBackup.pop_back(); }
    void discardBackupTokenPos() { m_tokenPosBackup.pop_back(); }
    const Token& getLastToken() const 
    {
        ASSERT(m_curToken > 0 && m_curToken <= m_tokens.size());
        return m_tokens[m_curToken - 1];
    }
    const Token& getCurToken() const
    {
        ASSERT(m_curToken >= 0 && m_curToken < m_tokens.size());
        return m_tokens[m_curToken];
    }

private:
    std::vector<int> m_tokenPosBackup;
    std::vector<Token> m_tokens;
    int m_curToken;
    std::vector<FuncSyntaxPtr> m_funcs;
    std::string m_fname;
};

Parser::Parser():
    m_impl(new ParserImpl())
{
}
Parser::~Parser()
{
    delete m_impl;
} 
bool Parser::parse(const std::string& fname, const std::string& src) 
{
    return m_impl->parse(fname, src);
}
const std::vector<FuncSyntaxPtr> &Parser::getFuncs() const
{
    return m_impl->getFuncs();
}
