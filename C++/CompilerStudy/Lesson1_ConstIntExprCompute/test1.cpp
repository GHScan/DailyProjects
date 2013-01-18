#include "pch.h" 

#include <stdlib.h>

#include <string>
#include <vector>
#include <memory>

struct Token
{
    virtual std::string toString() const {}
    virtual ~Token() {}
};
typedef std::shared_ptr<Token> TokenPtr;
struct Token_Operator: public Token
{
    char op;
    int hash;
    Token_Operator(char op, int hash) { this->op = op, this->hash = hash; }
    TokenPtr call(const TokenPtr &a, const TokenPtr &b);
};
struct Token_Number: public Token
{
    double num;
    Token_Number(double num) { this->num = num; }
    virtual std::string toString() const 
    {
        char buf[32];
        sprintf(buf, "%.0f", num);
        return buf;
    }
};
struct Token_String: public Token
{
    std::string s;
    Token_String(const std::string& s) { this->s = s; }
    virtual std::string toString() const { return s; }
};
TokenPtr Token_Operator::call(const TokenPtr &a, const TokenPtr &b)
{
    Token_Number *na = dynamic_cast<Token_Number*>(a.get()), *nb = dynamic_cast<Token_Number*>(b.get());
    Token_String *sa = dynamic_cast<Token_String*>(a.get());
    switch (op) {
        case '+': 
            if (sa != nullptr) return TokenPtr(new Token_String(sa->s + b->toString()));
            return TokenPtr(new Token_Number(na->num + nb->num));
        case '-': return TokenPtr(new Token_Number(na->num - nb->num));
        case '*':
            if (sa != nullptr) {
                std::string r = sa->s;
                for (int i = 1; i < nb->num; ++i) r += sa->s;
                return TokenPtr(new Token_String(r));
            }
            return TokenPtr(new Token_Number(na->num * nb->num));
        case '/': return TokenPtr(new Token_Number(na->num / nb->num));
    }
    return TokenPtr();
}

TokenPtr evalTokens(const TokenPtr* begin, int count)
{
    if (count == 1) return begin[0];
    int minOpIdx = 1;
    Token_Operator *minOp = static_cast<Token_Operator*>(begin[1].get());
    for (int i = 3; i < count; i += 2) {
        Token_Operator *op = static_cast<Token_Operator*>(begin[i].get());
        if (op->hash < minOp->hash) minOpIdx = i, minOp = op;
    }
    return minOp->call(
            evalTokens(begin, minOpIdx), 
            evalTokens(begin + minOpIdx + 1, count - minOpIdx - 1));
}
std::string eval(const std::string& expr) 
{
    std::vector<TokenPtr> tokens;
    int depth = 0, opid = 0;
    for (char *p = (char*)expr.c_str(); ;) {
        while (isspace(p[0])) ++p;
        if (p[0] == 0) break;
        if (isdigit(p[0])) {
            char *newp = NULL;
            tokens.push_back(TokenPtr(new Token_Number(strtod(p, &newp))));
            p = newp;
        }
        else if (p[0] == '\'') {
            char *end = p;
            while (*++end != '\'');
            tokens.push_back(TokenPtr(new Token_String(std::string(p + 1, end))));
            p = end + 1;
        }
        else {
            ++opid;
            switch (p[0]) {
                case '+':
                case '-':
                    tokens.push_back(TokenPtr(new Token_Operator(p[0], (depth << 24) | (1 << 16) | (0xffff - opid))));
                    break;
                case '*':
                case '/':
                    tokens.push_back(TokenPtr(new Token_Operator(p[0], (depth << 24) | (2 << 16) | (0xffff - opid))));
                    break;
                case ')': --depth; break;
                case '(': ++depth; break;
            }
            ++p;
        }
    }
    return evalTokens(&tokens[0], tokens.size())->toString();
}

int main() 
{
    cout << eval("2+ (13-(2+3))* 5") << endl;
    cout << eval("'abc'*3") << endl;
    cout << eval("'def'+(3 + 5)") << endl;
}
