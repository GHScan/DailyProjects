
#include "pch.h"

#include <stdlib.h>

#include <AsmJit/AsmJit.h>

#define ASSERT(b) do { if (!(b)) throw "assert failed !" #b; } while(0)

using namespace AsmJit;

enum TokenType {
    TT_EOF, TT_Int, TT_Operator,
};
struct Token {
    TokenType type;
    const char *begin, *end;
    Token(): type(TT_EOF), begin(NULL), end(NULL) {}
    Token(TokenType _type, const char *_begin, const char *_end): type(_type), begin(_begin), end(_end){}
};
class Scanner {
public:
    Scanner(const char *str): m_str(str) { move(); }
    Token fetch() const {
        return m_token;
    }
    Token next() {
        Token token = fetch();
        move();
        return token;
    }
private:
    void move() {
        for (; isspace(*m_str); ++m_str);
        if (!*m_str) {
            m_token = Token();
            return;
        }
        if (isdigit(*m_str)) {
            const char *begin = m_str;
            while (isdigit(*++m_str));
            m_token = Token(TT_Int, begin, m_str);
        } else {
            const char *begin = m_str++;
            m_token = Token(TT_Operator, begin, begin + 1);
        }
    }
private:
    const char *m_str;
    Token m_token;
};
class JitCompiler {
public:
    JitCompiler(Scanner *scanner) {
        m_asm.push(edx);
        _expr(scanner, 0);
        m_asm.mov(eax, dword_ptr(esp, 0));
        m_asm.add(esp, 4);
        m_asm.pop(edx);
        m_asm.ret();
        m_f = function_cast<int(*)()>(m_asm.make());
    }
    ~JitCompiler() {
        MemoryManager::getGlobal()->free((void*)m_f);
    }
    int run() {
        return m_f();
    }
private:
    void _expr(Scanner *scanner, int rbp) {
        _atom(scanner);
        while (scanner->fetch().type != TT_EOF && rbp < getOperatorLBP(scanner->fetch())) {
            _infix(scanner);
        }
    }
    void _infix(Scanner *scanner) {
        Token token = scanner->next();
        _expr(scanner, getOperatorRBP(token));
        m_asm.mov(eax, dword_ptr(esp, 4));
        switch (*token.begin) {
            case '+': m_asm.add(eax, dword_ptr(esp, 0)); break;
            case '-': m_asm.sub(eax, dword_ptr(esp, 0)); break;
            case '*': m_asm.imul(eax, dword_ptr(esp, 0)); break;
            case '/': 
            case '%': 
                m_asm.xor_(edx, edx);
                m_asm.idiv(dword_ptr(esp, 0));
                if (*token.begin == '%') m_asm.mov(eax, edx);
                break;
            default: ASSERT(0);
        }
        m_asm.mov(dword_ptr(esp, 4), eax);
        m_asm.add(esp, 4);
    }
    void _atom(Scanner *scanner) {
        Token token = scanner->next();
        if (token.type == TT_Int) {
            m_asm.push(atoi(string(token.begin, token.end).c_str()));
        } else {
            ASSERT(*token.begin == '(');
            _expr(scanner, 0);
            token = scanner->next();
            ASSERT(*token.begin == ')');
        }
    }
private:
    static int getOperatorLBP(Token token) {
        ASSERT(token.type == TT_Operator);
        switch (*token.begin) {
            case '+': case '-': return 1;
            case '*': case '/': case '%': return 2;
            case ')': return 0;
            default : ASSERT(0); return 0;
        }
    }
    static int getOperatorRBP(Token token) {
        return getOperatorLBP(token);
    }
private:
    Assembler m_asm;
    int (*m_f)();
};

#define TIMINE(codes) { clock_t start = clock(); codes; printf("%f sec\n", float(clock() - start) / CLOCKS_PER_SEC);}

int main() {
    const int LOOP = 2000000;

    {
        Scanner scanner("1+2-3*4+(5-6)-(7+8)%9");
        JitCompiler compiler(&scanner);

        printf("jit compiler (loop=%d)\n", LOOP);
        int sum = 0;
        TIMINE(for (int i = 0; i < LOOP; ++i) sum += compiler.run(););
        printf("sum=%d\n", sum);
    }
}
