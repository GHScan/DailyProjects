#include "pch.h" 

#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <math.h>

#include <memory>
#include <map>
#include <string>
#include <vector>
using namespace std;

struct VirtualMachine {
    map<string, double> variables;
    bool shouldBreak;
    VirtualMachine(): shouldBreak(false){}
};

//============================== AST
struct ExprNode {
    virtual ~ExprNode(){}
    virtual double interpret(VirtualMachine *vm) = 0;
};
typedef shared_ptr<ExprNode> ExprNodePtr;
struct ExprNode_Number : public ExprNode {
public:
    ExprNode_Number(double value): m_value(value){}
    virtual double interpret(VirtualMachine *vm);
private:
    double m_value;
};
struct ExprNode_BinaryOp: public ExprNode {
public:
    ExprNode_BinaryOp(const string& op, const ExprNodePtr& lv, const ExprNodePtr& rv): m_op(op), m_lv(lv), m_rv(rv){}
    virtual double interpret(VirtualMachine *vm);
private:
    string m_op;
    ExprNodePtr m_lv, m_rv;
};
struct ExprNode_Variable: public ExprNode {
public:
    ExprNode_Variable(const string& name): m_name(name){}
    virtual double interpret(VirtualMachine *vm);
private:
    string m_name;
};

struct StmtNode {
    virtual ~StmtNode() {}
    virtual void interpret(VirtualMachine *vm) = 0;
};
typedef shared_ptr<StmtNode> StmtNodePtr;
struct StmtNode_Assignment: public StmtNode{
public:
    StmtNode_Assignment(const string& name, const ExprNodePtr &rv): m_name(name), m_rv(rv){}
    virtual void interpret(VirtualMachine *vm);
private:
    string m_name;
    ExprNodePtr m_rv;
};
struct StmtNode_StmtList: public StmtNode{
public:
    vector<StmtNodePtr>& getStmtList() { return m_stmtList; }
    virtual void interpret(VirtualMachine *vm);
private:
    vector<StmtNodePtr> m_stmtList;
};
struct StmtNode_While: public StmtNode {
public:
    StmtNode_While(const ExprNodePtr& condExpr, const StmtNodePtr& stmt): m_condExpr(condExpr), m_stmt(stmt){}
    virtual void interpret(VirtualMachine *vm);
private:
    ExprNodePtr m_condExpr;
    StmtNodePtr m_stmt;
};
struct StmtNode_Break: public StmtNode{
public:
    virtual void interpret(VirtualMachine *vm);
private:
};
struct StmtNode_IfElse: public StmtNode{
public:
    StmtNode_IfElse(const ExprNodePtr& condExpr, const StmtNodePtr& thenStmt, const StmtNodePtr& elseStmt): m_condExpr(condExpr), m_thenStmt(thenStmt), m_elseStmt(elseStmt){}
    virtual void interpret(VirtualMachine *vm);
private:
    ExprNodePtr m_condExpr;
    StmtNodePtr m_thenStmt, m_elseStmt;
};
struct StmtNode_Call: public StmtNode {
public:
    StmtNode_Call(const string& funcName, const vector<ExprNodePtr>& params): m_funcName(funcName), m_params(params){}
    virtual void interpret(VirtualMachine *vm);
private:
    string m_funcName;
    vector<ExprNodePtr> m_params;
};

//============================== Runtime
double ExprNode_Number::interpret(VirtualMachine *vm) {
    return m_value;
}
double ExprNode_BinaryOp::interpret(VirtualMachine *vm) {
    if (m_op == "+") return m_lv->interpret(vm) + m_rv->interpret(vm);
    else if (m_op == "-") return m_lv->interpret(vm) - m_rv->interpret(vm);
    else if (m_op == "*") return m_lv->interpret(vm) * m_rv->interpret(vm);
    else if (m_op == "/") return m_lv->interpret(vm) / m_rv->interpret(vm);
    else if (m_op == "%") return fmod(m_lv->interpret(vm), m_rv->interpret(vm));
    else if (m_op == "!=") return fabs(m_lv->interpret(vm) - m_rv->interpret(vm)) > 0.00001;
    else if (m_op == "==") return fabs(m_lv->interpret(vm) - m_rv->interpret(vm)) <= 0.00001;
    else if (m_op == "<=") return m_lv->interpret(vm) <= m_rv->interpret(vm);
    else if (m_op == ">=") return m_lv->interpret(vm) >= m_rv->interpret(vm);
    else if (m_op == "<") return m_lv->interpret(vm) < m_rv->interpret(vm);
    else if (m_op == ">") return m_lv->interpret(vm) > m_rv->interpret(vm);
    else assert(0);
    return 0;
}
double ExprNode_Variable::interpret(VirtualMachine *vm) {
    return vm->variables[m_name];
}

void StmtNode_Assignment::interpret(VirtualMachine *vm) {
    vm->variables[m_name] = m_rv->interpret(vm);
}
void StmtNode_While::interpret(VirtualMachine *vm) {
    assert(vm->shouldBreak == false);
    while (m_condExpr->interpret(vm) && !vm->shouldBreak) {
        if (m_stmt != nullptr) m_stmt->interpret(vm);
    }
    vm->shouldBreak = false;
}
void StmtNode_Break::interpret(VirtualMachine *vm) {
    vm->shouldBreak = true;
}
void StmtNode_StmtList::interpret(VirtualMachine *vm) {
    for (int i = 0; i < (int)m_stmtList.size() && !vm->shouldBreak; ++i) {
        m_stmtList[i]->interpret(vm);
    }
}
void StmtNode_IfElse::interpret(VirtualMachine *vm) {
    if (m_condExpr->interpret(vm)) {
        if (m_thenStmt != nullptr) m_thenStmt->interpret(vm);
    } else {
        if (m_elseStmt != nullptr) m_elseStmt->interpret(vm);
    }
}
void StmtNode_Call::interpret(VirtualMachine *vm) {
    if (m_funcName == "print") {
        for (int i = 0; i < (int)m_params.size(); ++i) {
            printf("%g\t", m_params[i]->interpret(vm));
        }
        puts("");
    } else assert(0);
}

//============================== Parser
struct Token {
    enum Type {
        NUMBER = 128, ID, CALL, IF, ELSE, WHILE, BREAK, EQ, NE, GE, LE,
    };
    Token(int t): type((Type)t){}
    Token(int t, const char *begin, const char *end): type((Type)t), value(begin, end){}
    Type type;
    string value;
};

static vector<Token> tokensize(const char* str) {
    vector<Token> tokens;
    while (*str) {
        while (isspace(*str)) ++str;
        if (!*str) break;
        if (str[0] == '/' && str[1] == '/') {
            while (*str && *str != '\n') ++str;
            if (*str) ++str;
            continue;
        }
        if (str[0] == '/' && str[1] == '*') {
            while (*str && !(str[0] == '*' && str[1] == '/')) ++str;
            if (*str) str += 2;
            continue;
        }

        if (isdigit(*str)) {
            char* endstr = nullptr;
            strtod(str, &endstr);
            tokens.push_back(Token(Token::NUMBER, str, endstr));
            str = endstr;
        } else if (isalpha(*str)) {
            if (strncmp(str, "if", 2) == 0) str += 2, tokens.push_back(Token(Token::IF));
            else if (strncmp(str, "else", 4) == 0) str += 4, tokens.push_back(Token(Token::ELSE));
            else if (strncmp(str, "while", 5) == 0) str += 5, tokens.push_back(Token(Token::WHILE));
            else if (strncmp(str, "break", 5) == 0) str += 5, tokens.push_back(Token(Token::BREAK));
            else if (strncmp(str, "call", 4) == 0) str += 4, tokens.push_back(Token(Token::CALL));
            else {
                const char *endstr = str;
                while (isalpha(*endstr)) ++endstr;
                tokens.push_back(Token(Token::ID, str, endstr));
                str = endstr;
            }
        } else {
            if (strncmp(str, "!=", 2) == 0) str += 2, tokens.push_back(Token(Token::NE));
            else if (strncmp(str, "==", 2) == 0) str += 2, tokens.push_back(Token(Token::EQ));
            else if (strncmp(str, ">=", 2) == 0) str += 2, tokens.push_back(Token(Token::GE));
            else if (strncmp(str, "<=", 2) == 0) str += 2, tokens.push_back(Token(Token::LE));
            else {
                tokens.push_back(Token(*str++));
            }
        }
    }
    return tokens;
}

class Parser {
public:
    Parser(const vector<Token>& tokens): m_tokens(tokens), m_tokenPos(0) {
        m_tokens.insert(m_tokens.begin(), Token('{'));
        m_tokens.insert(m_tokens.end(), Token('}'));
        m_ast = _stmt();
    }
    StmtNodePtr getAST() { return m_ast; }
private:
    bool test(int type) { return m_tokens[m_tokenPos].type == type; }
    bool tryConsume(int type) {
        if (m_tokens[m_tokenPos].type == type) return ++m_tokenPos, true;
        else return false;
    }
    Token& consume(int type) {
        assert(m_tokens[m_tokenPos].type == type);
        return ++m_tokenPos, m_tokens[m_tokenPos - 1];
    }
private:
    StmtNodePtr _stmt() {
        if (tryConsume(';')) return nullptr;
        else if (StmtNodePtr p = _assgin_stmt()) return p;
        else if (StmtNodePtr p = _call_stmt()) return p;
        else if (StmtNodePtr p = _if_stmt()) return p;
        else if (StmtNodePtr p = _while_stmt()) return p;
        else if (StmtNodePtr p = _break_stmt()) return p;
        else if (StmtNodePtr p = _block()) return p;
        else assert(0);
        return nullptr;
    }
    StmtNodePtr _assgin_stmt() {
        if (!test(Token::ID)) return nullptr;
        Token tname = consume(Token::ID);
        consume('=');
        StmtNodePtr stmt = make_shared<StmtNode_Assignment>(tname.value, _expr());
        consume(';');
        return stmt;
    }
    StmtNodePtr _call_stmt() {
        if (!tryConsume(Token::CALL)) return nullptr;
        Token tfuncName = consume(Token::ID);
        consume('(');
        vector<ExprNodePtr> params;
        while (!tryConsume(')')) {
            if (!params.empty()) consume(',');
            params.push_back(_expr());
        }
        consume(';');
        return make_shared<StmtNode_Call>(tfuncName.value, params);
    }
    StmtNodePtr _if_stmt() {
        if (!tryConsume(Token::IF)) return nullptr;
        consume('(');
        ExprNodePtr condExpr = _expr();
        consume(')');
        StmtNodePtr thenStmt = _stmt();
        StmtNodePtr elseStmt;
        if (tryConsume(Token::ELSE))  {
            elseStmt = _stmt();
        }
        return make_shared<StmtNode_IfElse>(condExpr, thenStmt, elseStmt);
    }
    StmtNodePtr _while_stmt() {
        if (!tryConsume(Token::WHILE)) return nullptr;
        consume('(');
        ExprNodePtr condExpr = _expr();
        consume(')');
        return make_shared<StmtNode_While>(condExpr, _stmt());
    }
    StmtNodePtr _break_stmt() {
        if (!tryConsume(Token::BREAK)) return nullptr;
        consume(';');
        return make_shared<StmtNode_Break>();
    }
    StmtNodePtr _block() {
        if (!tryConsume('{')) return nullptr;
        StmtNodePtr stmt = make_shared<StmtNode_StmtList>();
        StmtNode_StmtList *p = static_cast<StmtNode_StmtList*>(stmt.get());
        while (!tryConsume('}')) {
            if (StmtNodePtr substmt = _stmt()) {
                p->getStmtList().push_back(substmt);
            }
        }
        return stmt;
    }
    ExprNodePtr _expr() {
        return _relat_expr();
    }
    ExprNodePtr _relat_expr() {
        ExprNodePtr lv = _add_expr();
        for (;;) {
            if (tryConsume(Token::NE)) {
                lv = make_shared<ExprNode_BinaryOp>("!=", lv, _add_expr());
            } else if (tryConsume(Token::EQ)) {
                lv = make_shared<ExprNode_BinaryOp>("==", lv, _add_expr());
            } else if (tryConsume(Token::GE)) {
                lv = make_shared<ExprNode_BinaryOp>(">=", lv, _add_expr());
            } else if (tryConsume(Token::LE)) {
                lv = make_shared<ExprNode_BinaryOp>("<=", lv, _add_expr());
            } else if (tryConsume('<')) {
                lv = make_shared<ExprNode_BinaryOp>("<", lv, _add_expr());
            } else if (tryConsume('>')) {
                lv = make_shared<ExprNode_BinaryOp>(">", lv, _add_expr());
            } else break;
        }
        return lv;
    }
    ExprNodePtr _add_expr() {
        ExprNodePtr lv = _mul_expr();
        for (;;) {
            if (tryConsume('+')) {
                lv = make_shared<ExprNode_BinaryOp>("+", lv, _mul_expr());
            } else if (tryConsume('-')) {
                lv = make_shared<ExprNode_BinaryOp>("-", lv, _mul_expr());
            } else break;
        }
        return lv;
    }
    ExprNodePtr _mul_expr() {
        ExprNodePtr lv = _primary_expr();
        for (;;) {
            if (tryConsume('*')) {
                lv = make_shared<ExprNode_BinaryOp>("*", lv, _primary_expr());
            } else if (tryConsume('/')) {
                lv = make_shared<ExprNode_BinaryOp>("/", lv, _primary_expr());
            } else if (tryConsume('%')) {
                lv = make_shared<ExprNode_BinaryOp>("%", lv, _primary_expr());
            } else break;
        }
        return lv;
    }
    ExprNodePtr _primary_expr() { 
        if (tryConsume('(')) {
            ExprNodePtr e = _expr();
            consume(')');
            return e;
        } else if (test(Token::ID)) {
            return make_shared<ExprNode_Variable>(consume(Token::ID).value);
        } else if (test(Token::NUMBER)) {
            return make_shared<ExprNode_Number>(atof(consume(Token::NUMBER).value.c_str()));
        } else {
            assert(0);
            return nullptr;
        }
    }
private:
    vector<Token> m_tokens;
    int m_tokenPos;
    StmtNodePtr m_ast;
};

int main(int argc, char *argv[]) {
    if (argc == 1) {
        printf("Usage %s scriptname\n", argv[0]);
        return 0;
    }

    char filecontent[1 << 16];
    {
        FILE *f = fopen(argv[1], "r");
        if (f == nullptr) puts("invalid scriptname"), exit(1);
        fread(filecontent, sizeof filecontent, 1, f);
        fclose(f);
    }

    VirtualMachine vm;
    Parser(tokensize(filecontent)).getAST()->interpret(&vm);
}
