#include "pch.h" 

#include <stdlib.h>
#include <stdarg.h>
#include <time.h>

#include <vector>
#include <string>
#include <iostream>
using namespace std;

#define ASSERT(b) do{ if(!(b)) throw "assert failed! " #b; } while(0)
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
};
struct ASTNode_Int: public ASTNode {
    int num;
    explicit ASTNode_Int(int _num): num(_num){}
};
struct ASTNode_BinaryOp: public ASTNode {
    char op;
    ASTNode *left, *right;
    ASTNode_BinaryOp(char _op, ASTNode *_left, ASTNode *_right): op(_op), left(_left), right(_right){}
    ~ASTNode_BinaryOp() { delete left; delete right;}
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
        ASSERT(t.type == TT_Operator);
        switch (t.begin[0]) {
            case '+': return new ASTNode_BinaryOp('+', left, _expr(getOperatorRBP(t)));
            case '-': return new ASTNode_BinaryOp('-', left, _expr(getOperatorRBP(t)));
            case '*': return new ASTNode_BinaryOp('*', left, _expr(getOperatorRBP(t)));
            case '/': return new ASTNode_BinaryOp('/', left, _expr(getOperatorRBP(t)));
            case '%': return new ASTNode_BinaryOp('%', left, _expr(getOperatorRBP(t)));
            default: ASSERT(0); return NULL;
        }
    } 
    ASTNode* _atom() {
        Token t = m_scaner->next();
        ASTNode *r = NULL;
        if (t.type == TT_Int) {
            r = new ASTNode_Int(atoi(string(t.begin, t.end).c_str()));
        } else {
            ASSERT(t.type == TT_Operator && t.begin[0] == '(');
            r = _expr(0);
            consumeOperatorToken(')');
        }
        return r;
    }
private:
    static int getOperatorLBP(const Token &t) {
        ASSERT(t.type == TT_Operator);
        switch (t.begin[0]) {
            case '+': return 1;
            case '-': return 1;
            case '*': return 2;
            case '/': return 2;
            case '%': return 2;
            case ')': return 0;
            default: ASSERT(0); return 0;
        }
    }
    int getOperatorRBP(const Token &t) {
        return getOperatorLBP(t);
    }
private:
    void consumeOperatorToken(char c) {
        Token t = m_scaner->next();
        ASSERT(t.type == TT_Operator && t.begin[0] == c);
    }
private:
    Scanner *m_scaner;
};
//============================== tree-walking interpreter
int _evalByWalkAST(ASTNode *node) {
    if (ASTNode_Int* p = dynamic_cast<ASTNode_Int*>(node)) {
        return p->num;
    } else if (ASTNode_BinaryOp *p = dynamic_cast<ASTNode_BinaryOp*>(node)) {
        switch (p->op) {
            case '+': return _evalByWalkAST(p->left) + _evalByWalkAST(p->right);
            case '-': return _evalByWalkAST(p->left) - _evalByWalkAST(p->right);
            case '*': return _evalByWalkAST(p->left) * _evalByWalkAST(p->right);
            case '/': return _evalByWalkAST(p->left) / _evalByWalkAST(p->right);
            case '%': return _evalByWalkAST(p->left) % _evalByWalkAST(p->right);
            default: ASSERT(0); return 0;
        }
    } else {
        ASSERT(0);
        return 0;
    }
}
int evalByWalkAST(ASTNode *n, int loop) {
    int sum = 0;
    for (int i = 0; i < loop; ++i) sum += _evalByWalkAST(n);
    return sum;
}
//============================== bytecode virtual machine
struct VM {
    vector<char> codes;
    vector<int> evalStack;
    VM() {}
    template<typename T>
    void emit(T param) {
        int off = (int)codes.size();
        codes.resize(codes.size() + sizeof(T));
        (T&)codes[off] = param;
    }
};
enum VMCode {
    VMC_Push, VMC_Add, VMC_Sub, VMC_Mul, VMC_Div, VMC_Mod, 
};
int vmRun(VM *vm) {
    vector<int> &evalStack = vm->evalStack;
    int pc = 0;
    int endpc = (int)vm->codes.size();
    while (pc < endpc) {
        VMCode c = (VMCode&)vm->codes[pc];
        pc += sizeof(VMCode);
        switch (c) {
            case VMC_Push: 
                evalStack.push_back((int&)vm->codes[pc]);
                pc += sizeof(int);
                break;
            case VMC_Add: 
                evalStack[evalStack.size() - 2] += evalStack.back();
                evalStack.pop_back();
                break;
            case VMC_Sub: 
                evalStack[evalStack.size() - 2] -= evalStack.back();
                evalStack.pop_back();
                break;
            case VMC_Mul: 
                evalStack[evalStack.size() - 2] *= evalStack.back();
                evalStack.pop_back();
                break;
            case VMC_Div: 
                evalStack[evalStack.size() - 2] /= evalStack.back();
                evalStack.pop_back();
                break;
            case VMC_Mod: 
                evalStack[evalStack.size() - 2] %= evalStack.back();
                evalStack.pop_back();
                break;
            default: ASSERT(0); 
        }
    }
    int r = evalStack.back();
    evalStack.clear();
    return r;
}
void emitVMCodeByWalkAST(VM *vm, ASTNode *node) {
    if (ASTNode_Int *p = dynamic_cast<ASTNode_Int*>(node)) {
        vm->emit(VMC_Push);
        vm->emit(p->num);
    } else if (ASTNode_BinaryOp *p = dynamic_cast<ASTNode_BinaryOp*>(node)) {
        emitVMCodeByWalkAST(vm, p->left);
        emitVMCodeByWalkAST(vm, p->right);
        switch (p->op) {
            case '+': vm->emit(VMC_Add); break;
            case '-': vm->emit(VMC_Sub); break;
            case '*': vm->emit(VMC_Mul); break;
            case '/': vm->emit(VMC_Div); break;
            case '%': vm->emit(VMC_Mod); break;
            default: ASSERT(0);
        }
    } else {
        ASSERT(0);
    }
}
int evalByRuningVM(ASTNode *node, int loop) {
    VM vm;
    emitVMCodeByWalkAST(&vm, node);

    int sum = 0;
    for (int i = 0; i < loop; ++i) sum += vmRun(&vm);
    return sum;
}
//============================== JIT
#ifdef _MSC_VER
#pragma warning(disable : 4312)
#pragma warning(disable : 4311)
#include <Windows.h>
#pragma warning(default : 4311)
#pragma warning(default : 4312)
void* mallocExec(int size) {
    void *p = ::VirtualAlloc(NULL, size, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    ASSERT(p != NULL);
    return p;
}
void freeExec(void *p) {
    ASSERT(p != NULL);
	::VirtualFree(p, 0, MEM_RELEASE);
}
#endif
#if defined(__linux__) || defined(__APPLE__)
#include <unistd.h>
#include <sys/mman.h>
static void* mallocExec(int size) {
    void *p = NULL;
    int erro = ::posix_memalign(&p, ::getpagesize(), size);
    ASSERT(p != NULL && erro == 0);
    erro = ::mprotect(p, size, PROT_READ | PROT_WRITE | PROT_EXEC);
    ASSERT(erro == 0);
    return p;
}
static void freeExec(void *p) {
    ASSERT(p != NULL);
    ::free(p);
}
#endif
class JITEngine {
public:
    JITEngine(): m_size(0) { m_codes = (char*)mallocExec(4096); }
    ~JITEngine() { freeExec(m_codes); }
    void emit(int n, ...) {
        va_list arg;
        va_start(arg, n);
        for (int i = 0; i < n; ++i) m_codes[m_size++] = (char)va_arg(arg, int);
        va_end(arg);
    }
    int run() { return ((int(*)())m_codes)();}
private:
    char *m_codes;
    int m_size;
};
void x86Emit_begin(JITEngine *engine) {
    engine->emit(1, 0x53); // push ebx
    engine->emit(1, 0x52); // push edx
}
void x86Emit_push(JITEngine *engine, int i) {
    char *pi = (char*)&i;
    engine->emit(5, 0x68, pi[0], pi[1], pi[2], pi[3]); // push i
}
void x86Emit_op(JITEngine *engine, char c) {
    engine->emit(4, 0x8b, 0x44, 0x24, 0x04); // mov eax, dword [esp+4]
    engine->emit(3, 0x8b, 0x1c, 0x24); // mov ebx, dword [esp]
    switch (c) { // op eax, ebx
        case '+': engine->emit(2, 0x03, 0xc3); break; 
        case '-': engine->emit(2, 0x2b, 0xc3); break;
        case '*': engine->emit(3, 0x0f, 0xaf, 0xc3);break;
        case '/': case '%': 
            engine->emit(2, 0x33, 0xd2); // xor edx, edx
            engine->emit(2, 0xf7, 0xfb);
            if (c == '%') engine->emit(2, 0x8b, 0xc2); // mov eax, edx
            break;
        default: ASSERT(0); break;
    }
    engine->emit(4, 0x89, 0x44, 0x24, 0x04); // mov dword [esp+4], eax
    engine->emit(3, 0x83, 0xc4, 0x04); // add esp, 4
}
void x86Emit_end(JITEngine *engine) {
    engine->emit(3, 0x8b, 0x04, 0x24); // mov eax, dword [esp]
    engine->emit(3, 0x83, 0xc4, 0x04); // add esp, 4
    engine->emit(1, 0x5a); // pop edx
    engine->emit(1, 0x5b); // pop ebx
    engine->emit(1, 0xc3); // ret
}
void emitx86InsByWalkAST(JITEngine *engine, ASTNode *node) {
    if (ASTNode_Int* p = dynamic_cast<ASTNode_Int*>(node)) {
        x86Emit_push(engine, p->num);
    } else if (ASTNode_BinaryOp* p = dynamic_cast<ASTNode_BinaryOp*>(node)) {
        emitx86InsByWalkAST(engine, p->left);
        emitx86InsByWalkAST(engine, p->right);
        x86Emit_op(engine, p->op);
    } else {
        ASSERT(0);
    }
}
int evalByRuningJIT(ASTNode *n, int loop) {
    JITEngine engine;
    x86Emit_begin(&engine);
    emitx86InsByWalkAST(&engine, n);
    x86Emit_end(&engine);

    int sum = 0;
    for (int i = 0; i < loop; ++i) sum += engine.run();
    return sum;
}
//============================== main
#define TIMINE(codes) { clock_t start = clock(); codes; printf("%f sec\n", float(clock() - start) / CLOCKS_PER_SEC);}

int main() {
    try {
        Scanner scanner("1+2-3*4+(5-6)-(7+8)%9");
        ASTNode *n = Parser(&scanner).parse();

        const int LOOP = 2000000;
        int result = 0, result2 = 0;
        {
            printf("tree-walking interpreter (loop=%d)\n", LOOP);
            TIMINE(result = evalByWalkAST(n, LOOP));
        }
        {
            printf("stack-based virtual machine (loop=%d)\n", LOOP);
            TIMINE(result2 = evalByRuningVM(n, LOOP); ASSERT(result == result2); );
        }
        {
            printf("jit compiler (loop=%d)\n", LOOP);
            TIMINE(result2 = evalByRuningJIT(n, LOOP); ASSERT(result == result2));
        }

        delete n;
    } catch(const char *e) {
        printf("unhandled exception: %s\n", e);
    }
}
