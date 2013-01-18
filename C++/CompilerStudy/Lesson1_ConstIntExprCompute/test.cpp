// vim:fileencoding=gbk
//
#include "pch.h"

#include <cassert>
#include <cmath>

#include <string>
#include <vector>
#include <map>

enum TokenType
{
    TT_operator = 1,
    TT_number = 2,
};

struct Token 
{
    TokenType t;
    union {
        const char *src;
        mutable int val;
    };
};

std::map<char, int> g_opPriority;
std::map<char, int(*)(int, int)> g_opFunc;

void lexicalAnalysis(const char *src, std::vector<Token>& tokens)
{
    for (;;) {
        while (isspace(*src)) ++src;
        if (*src == 0) break;
        if (isdigit(*src)) {
            Token t = { TT_number, src,};
            tokens.push_back(t);
            while (isdigit(*src)) ++src;
        }
        else if (src[0] == '-' && isdigit(src[1])) {
            Token t = { TT_number, src,};
            tokens.push_back(t);
            while (isdigit(*++src));
        }
        else {
            if (*src == '(' || *src == ')' || g_opPriority.count(*src) > 0) {
                Token t = { TT_operator, src++,};
                tokens.push_back(t);
            }
            else assert(0);
        }
    }
}

int compute(const char *exp)
{
    std::vector<Token> tokens;
    lexicalAnalysis(exp, tokens);

    // infix 2 suffix
    std::vector<Token> stackR, stackOp;
    for (int i = 0; i < (int)tokens.size(); ++i) {
        const Token& t = tokens[i];
        if (t.t == TT_number) {
            t.val = atoi(t.src);
            stackR.push_back(t);
        }
        else {
            if (t.src[0] == '(') stackOp.push_back(t);
            else if (t.src[0] == ')') {
                while (!stackOp.empty() && stackOp.back().src[0] != '(') {
                    stackR.push_back(stackOp.back());
                    stackOp.pop_back();
                }
                stackOp.pop_back();
            }
            else {
                while (!stackOp.empty() && 
                        g_opPriority[stackOp.back().src[0]] >= g_opPriority[t.src[0]]) {
                    stackR.push_back(stackOp.back());
                    stackOp.pop_back();
                }
                stackOp.push_back(t);
            }
        }
    }
    while (!stackOp.empty()) {
        stackR.push_back(stackOp.back());
        stackOp.pop_back();
    }

    // suffix compute
    while (stackR.size() > 1) {
        for (int i = (int)stackR.size() - 1; i >= 2; --i) {
            const Token &op = stackR[i], &val1 = stackR[i - 2], &val2 = stackR[i - 1];
            if (op.t == TT_operator && val1.t == TT_number && val2.t == TT_number) {
                val1.val = g_opFunc[op.src[0]](val1.val, val2.val);
                stackR.erase(stackR.begin() + i - 1, stackR.begin() + i + 1);
            }
        }
    }
    assert(stackR.size() == 1);
        
    return stackR[0].val;
}

int op_add(int a, int b) { return a + b;}
int op_sub(int a, int b) { return a - b;}
int op_mul(int a, int b) { return a * b;}
int op_div(int a, int b) { return a / b;}
int op_power(int a, int b) { return (int)powf(float(a), float(b)); }

int main() {
    g_opPriority['+'] = g_opPriority['-'] = 1;
    g_opPriority['*'] = g_opPriority['/'] = 2;
    g_opPriority['!'] = 3;
    g_opFunc['+'] = &op_add; g_opFunc['-'] = &op_sub;
    g_opFunc['*'] = &op_mul; g_opFunc['/'] = &op_div;
    g_opFunc['!'] = &op_power;

    assert(compute("23 *5") == 115);
    assert(compute("-23 / 5") == -4);
    assert(compute("4 *(1 + 2)") == 12);
    assert(compute("((10 - 3) / 2) *(8 - (3 + 6))") == -3);
    assert(compute("2!5") == 32);
    assert(compute("2!3+2!(2+2)") == 24);

    for (;;) {
        cout << "input expression : ( support +-*/()! ) (! means power)" << endl;
        std::string s;
        getline(cin, s);
        if (s == "exit") break;
        cout << "result:" << compute(s.c_str()) << endl;
    }
}
