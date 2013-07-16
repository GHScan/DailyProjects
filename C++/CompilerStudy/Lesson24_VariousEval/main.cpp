#include "pch.h" 

#include <assert.h>
#include <stdlib.h>

#include <vector>
#include <string>
#include <iostream>
using namespace std;
//============================== lexical analysiser
enum TokenType{
    TT_End,
    TT_Int,
    TT_Operator,
};
struct Token {
    TokenType type;
    const char *begin, *end;
    Token(): type(TT_End), begin(NULL), end(NULL){}
    Token(TokenType _type, const char *_begin, const char *_end): type(_type), begin(_begin), end(_end){}
};
class Scanner {
public:
    Scanner(const char *str): m_str(str) {}
    Token next() {
        if (m_fetch.type != TT_End) {
            Token r = m_fetch;
            m_fetch.type = TT_End;
            return r;
        }
        for (; isspace(*m_str); ++m_str);
        if (!*m_str) return Token(TT_End, NULL, NULL);
        if (isdigit(*m_str)) {
            const char *begin = m_str;
            while (isdigit(*++m_str));
            return Token(TT_Int, begin, m_str);
        } else {
            const char *begin = m_str++;
            return Token(TT_Operator, begin, begin + 1);
        }
    }
    Token fetch() {
        if (m_fetch.type == TT_End) m_fetch = next();
        return m_fetch;
    }
private:
    const char *m_str;
    Token m_fetch;
};
//============================== syntax analysiser
struct ASTNode{
    virtual ~ASTNode(){}
    virtual void destroy(){}
};
struct ASTNode_Int: public ASTNode {
    int num;
    explicit ASTNode_Int(int _num): num(_num){}
    virtual void destroy(){ delete this; }
};
struct ASTNode_BinaryOp: public ASTNode {
    char op;
    ASTNode *left, *right;
    ASTNode_BinaryOp(char _op, ASTNode *_left, ASTNode *_right): op(_op), left(_left), right(_right){}
    virtual void destroy(){ 
        if (left != NULL) left->destroy();
        if (right != NULL) right->destroy();
        delete this;
    }
};
class Parser {
public:
    Parser(Scanner *scaner): m_scaner(scaner){}
    ASTNode* parse() {
        return _expr(0);
    }
private:
    ASTNode* _expr(int rbp) {
        ASTNode *n = _atom();
        while (m_scaner->fetch().type != TT_End && rbp < getOperatorLBP(m_scaner->fetch())) {
            n = _infix(n);
        }
        return n;
    }
    ASTNode* _infix(ASTNode *left) {
        Token t = m_scaner->next();
        assert(t.type == TT_Operator);
        switch (t.begin[0]) {
            case '+': return new ASTNode_BinaryOp('+', left, _expr(getOperatorRBP(t)));
            case '-': return new ASTNode_BinaryOp('-', left, _expr(getOperatorRBP(t)));
            case '*': return new ASTNode_BinaryOp('*', left, _expr(getOperatorRBP(t)));
            case '/': return new ASTNode_BinaryOp('/', left, _expr(getOperatorRBP(t)));
            case '%': return new ASTNode_BinaryOp('%', left, _expr(getOperatorRBP(t)));
            default: assert(0); return NULL;
        }
    } 
    ASTNode* _atom() {
        Token t = m_scaner->next();
        ASTNode *r = NULL;
        if (t.type == TT_Int) {
            r = new ASTNode_Int(atoi(string(t.begin, t.end).c_str()));
        } else {
            assert(t.type == TT_Operator && t.begin[0] == '(');
            r = _expr(0);
            consumeOperatorToken(')');
        }
        return r;
    }
private:
    int getOperatorLBP(const Token &t) {
        assert(t.type == TT_Operator);
        switch (t.begin[0]) {
            case '+': return 1;
            case '-': return 1;
            case '*': return 2;
            case '/': return 2;
            case '%': return 2;
            case ')': return 0;
            default: assert(0); return 0;
        }
    }
    int getOperatorRBP(const Token &t) {
        return getOperatorLBP(t);
    }
private:
    void consumeOperatorToken(char c) {
        Token t = m_scaner->next();
        assert(t.type == TT_Operator && t.begin[0] == c);
    }
private:
    Scanner *m_scaner;
};
//============================== tree-walking interpreter
int evalByWalkAST(ASTNode *node) {
    if (ASTNode_Int* p = dynamic_cast<ASTNode_Int*>(node)) {
        return p->num;
    } else if (ASTNode_BinaryOp *p = dynamic_cast<ASTNode_BinaryOp*>(node)) {
        switch (p->op) {
            case '+': return evalByWalkAST(p->left) + evalByWalkAST(p->right);
            case '-': return evalByWalkAST(p->left) - evalByWalkAST(p->right);
            case '*': return evalByWalkAST(p->left) * evalByWalkAST(p->right);
            case '/': return evalByWalkAST(p->left) / evalByWalkAST(p->right);
            case '%': return evalByWalkAST(p->left) % evalByWalkAST(p->right);
            default: assert(0);
        }
    } else {
        assert(0);
        return 0;
    }
}
//============================== bytecode virtual machine
struct VM {
    vector<int> evalStack;
    vector<char> codes;
    int pc;
    VM(): pc(0){}
};
void vm_add(VM *vm) {
    vm->evalStack[(int)vm->evalStack.size() - 2] += vm->evalStack[(int)vm->evalStack.size() - 1];
    vm->evalStack.pop_back();
}
void vm_sub(VM *vm) {
    vm->evalStack[(int)vm->evalStack.size() - 2] -= vm->evalStack[(int)vm->evalStack.size() - 1];
    vm->evalStack.pop_back();
}
void vm_mul(VM *vm) {
    vm->evalStack[(int)vm->evalStack.size() - 2] *= vm->evalStack[(int)vm->evalStack.size() - 1];
    vm->evalStack.pop_back();
}
void vm_div(VM *vm) {
    vm->evalStack[(int)vm->evalStack.size() - 2] /= vm->evalStack[(int)vm->evalStack.size() - 1];
    vm->evalStack.pop_back();
}
void vm_mod(VM *vm) {
    vm->evalStack[(int)vm->evalStack.size() - 2] %= vm->evalStack[(int)vm->evalStack.size() - 1];
    vm->evalStack.pop_back();
}
void vm_push(VM *vm) {
    vm->evalStack.push_back((int&)vm->codes[vm->pc]);
    vm->pc += sizeof(int);
}
template<typename T>
void vm_emit(VM *vm, T param) {
    int off = (int)vm->codes.size();
    vm->codes.resize(vm->codes.size() + sizeof(T));
    (T&)vm->codes[off] = param;
}
void emitByteCodeByWalkAST(VM *vm, ASTNode *node) {
    if (ASTNode_Int *p = dynamic_cast<ASTNode_Int*>(node)) {
        vm_emit(vm, &vm_push);
        vm_emit(vm, p->num);
    } else if (ASTNode_BinaryOp *p = dynamic_cast<ASTNode_BinaryOp*>(node)) {
        emitByteCodeByWalkAST(vm, p->left);
        emitByteCodeByWalkAST(vm, p->right);
        switch (p->op) {
            case '+': vm_emit(vm, &vm_add); break;
            case '-': vm_emit(vm, &vm_sub); break;
            case '*': vm_emit(vm, &vm_mul); break;
            case '/': vm_emit(vm, &vm_div); break;
            case '%': vm_emit(vm, &vm_mod); break;
            default: assert(0);
        }
    } else {
        assert(0);
    }
}
int evalByRuningVM(ASTNode *node) {
    VM vm;
    emitByteCodeByWalkAST(&vm, node);
    while (vm.pc < (int)vm.codes.size()) {
        void(*op)(VM*) = (void(*&)(VM*))vm.codes[vm.pc];
        vm.pc += sizeof(&vm_push);
        op(&vm);
    }
    return vm.evalStack[0];
}

//============================== main
int main() {
    Scanner scanner("3+5-2*(5-3)");
    ASTNode *n = Parser(&scanner).parse();
    cout << evalByRuningVM(n) << endl;
    n->destroy();
}
