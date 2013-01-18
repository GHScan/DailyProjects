#include "pch.h" 

#include <cassert>
#include <memory.h>

#include <iostream>
#include <string>
#include <vector>
#include <map>

enum TokenType
{
    TT_operator = 1,
    TT_number, 
    TT_string,
    TT_identify,
};

struct Token
{
    TokenType t;
    const char *begin, *end;
};

enum ValueType
{
    VT_undefined = 1,
    VT_number,
    VT_string,
};

struct Value
{
    ValueType t;
    union {
        int num;
        std::string *ps;
    };
    Value();
    Value(const Token& token);
    Value(int num);
    Value(const char *val);
    Value(const Value&);
    Value& operator = (const Value&);
    ~Value();
    Value operator + (const Value& o) const;
    Value operator - (const Value& o) const;
    Value operator * (const Value& o) const;
    Value operator / (const Value& o) const;
    std::string tostring() const;
};
Value::Value():
    t(VT_undefined), num(0)
{
}
Value::Value(const Token& token)
{
    switch (token.t) {
        case TT_number:
            t = VT_number;
            num = atoi(token.begin);
            break;
        case TT_string:
            t = VT_string;
            ps = new std::string(token.begin + 1, token.end - 1);
            break;
        default:
            assert(0);
            break;
    }
}
Value::Value(int _num):
    t(VT_number), num(_num)
{
}
Value::Value(const char *val):
    t(VT_string), ps(new std::string(val))
{
}
Value::~Value()
{
    if (t == VT_string) delete ps;
}
Value::Value(const Value& o):
    t(VT_undefined), num(0)
{
    *this = o;
}
Value& Value::operator = (const Value& o)
{
    if (this  == &o) return *this;
    if (t == VT_string) delete ps;
    t = o.t;
    if (o.t == VT_string) ps = new std::string(*o.ps);
    else num = o.num;
    return *this;
}
Value Value::operator + (const Value& o) const
{
    switch (t) {
        case VT_number:
            switch (o.t) {
                case VT_number:
                    return Value(num + o.num);
                default:
                    assert(0);
                    break;
            }
            break;
        case VT_string:
            switch (o.t) {
                case VT_number:
                    return Value((*ps + o.tostring()).c_str());
                case VT_string:
                    return Value((*ps + *o.ps).c_str());
                default:
                    assert(0);
                    break;
            }
            break;
        default:
            assert(0);
            break;
    }
    assert(0);
    return Value();
}
Value Value::operator - (const Value& o) const
{
    assert(t == o.t && t == VT_number);
    return Value(num - o.num);
}
Value Value::operator * (const Value& o) const
{
    if (t == VT_number) {
        assert(o.t == VT_number);
        return Value(num * o.num);
    }
    else if (t == VT_string) {
        assert(o.t == VT_number);
        std::string r;
        for (int i = 0; i < o.num; ++i) r += *ps;
        return Value(r.c_str());
    }
    else { 
        assert(0);
        return Value();
    }
}
Value Value::operator / (const Value& o) const
{
    assert(t == o.t && t == VT_number);
    return Value(num / o.num);
}
std::string Value::tostring() const
{
    switch (t) {
        case VT_undefined: return "undefined";
        case VT_number: 
           {
               char buf[32];
               sprintf(buf, "%d", num);
               return buf;
           }
        case VT_string:
           return *ps;
        default:
            assert(0);
            return "";
    }
}

class SymbolTable
{
public:
    SymbolTable(){}
    int getNameIndex(const char *name);
    const Value& getValue(int idx) const;
    void setValue(int idx, const Value& v);
private:
    SymbolTable(const SymbolTable&);
    SymbolTable& operator = (const SymbolTable&);
private:
    std::map<std::string, int> m_name2Idx;
    std::vector<Value> m_values;
};
int SymbolTable::getNameIndex(const char *name)
{
    if (m_name2Idx.count(name) > 0) {
        return m_name2Idx[name];
    }
    int r = m_name2Idx[name] = (int)m_values.size();
    m_values.push_back(Value());
    return r;
}
const Value& SymbolTable::getValue(int idx) const
{
    return m_values[idx];
}
void SymbolTable::setValue(int idx, const Value& v)
{
    m_values[idx] = v;
}

enum ExprNodeType
{
    ENT_operator = 1,
    ENT_value,
    ENT_identify,
};

struct ExprNode
{
    ExprNodeType t;
    union {
        char op;
        Value *pval;
        int symbolIdx;
    };
    ExprNode *left, *right;

    ExprNode(const Token &token, SymbolTable& symTable);
    ExprNode(const ExprNode&);
    ExprNode& operator = (const ExprNode&);
    ~ExprNode();
    bool isPureOperator() const;
    void optimizeConstExpr();
    Value calc(const SymbolTable* symTable);
    void rdelete();
    int getNodeCount() const;
    static int getOpPriority(char op);
};
int ExprNode::getOpPriority(char op)
{
    switch (op) {
        case '+': case '-':
            return 1;
        case '*': case '/':
            return 2;
        default:
            assert(0);
            return 0;
    }
}
ExprNode::ExprNode(const Token &token, SymbolTable& symTable):
    left(NULL), right(NULL)
{
    switch (token.t) {
        case TT_operator:
            t = ENT_operator;
            op = token.begin[0];
            break;
        case TT_number:
        case TT_string:
            t = ENT_value;
            pval = new Value(token);
            break;
        case TT_identify:
            t = ENT_identify;
            symbolIdx = symTable.getNameIndex(std::string(token.begin, token.end).c_str());
            break;
        default:
            assert(0);
            break;
    }
}
ExprNode::~ExprNode()
{
    assert(left == NULL && right == NULL);
    if (t == ENT_value) delete pval;
}
bool ExprNode::isPureOperator() const
{
    return left == NULL && right == NULL && t == ENT_operator;
}
void ExprNode::optimizeConstExpr()
{
    if (left != NULL) left->optimizeConstExpr();
    if (right != NULL) right->optimizeConstExpr();
    if (t == ENT_operator) {
        if (left != NULL && left->t == ENT_value &&
                right != NULL && right->t == ENT_value) {
            pval = new Value(calc(NULL));
            t = ENT_value;
            left->rdelete(); left = NULL;
            right->rdelete(); right = NULL;
        }
    }
}
Value ExprNode::calc(const SymbolTable* symTable)
{
    switch (t) {
        case ENT_identify:
            return symTable->getValue(symbolIdx);
        case ENT_value:
            return *pval;
        case ENT_operator:
            break;
        default:
            assert(0);
            break;
    }
    assert(left != NULL && right != NULL);
    Value lval = left->calc(symTable), rval = right->calc(symTable);
    switch (op) {
        case '+':
            return lval + rval;
        case '-':
            return lval - rval;
        case '*':
            return lval * rval;
        case '/':
            return lval / rval;
        default:
            assert(0);
            return Value();
    }
}
void ExprNode::rdelete()
{
    if (left != NULL) {
        left->rdelete(); left = NULL;
    }
    if (right != NULL) { 
        right->rdelete(); right = NULL;
    }
    delete this;
}
int ExprNode::getNodeCount() const
{
    int r = 1;
    if (left != NULL) r += left->getNodeCount();
    if (right != NULL) r += right->getNodeCount();
    return r;
}

class Interpreter
{
public:
    Interpreter();
    ~Interpreter();
    bool compile(const char *src);
    std::string result(const char *env);
    int getNodeCount() const;
private:
    void lexicalAnalysis(const char *src, std::vector<Token>& tokens);
    void syntaxAnalysis(const std::vector<Token>& tokens);
    void optimize();
    void updateSymbolTable(const char *env);
private:
    SymbolTable m_symTable;
    ExprNode *m_node;
};
Interpreter::Interpreter():
    m_node(NULL)
{
}
Interpreter::~Interpreter()
{
    if (m_node != NULL) m_node->rdelete();
}
bool Interpreter::compile(const char *src)
{
    std::vector<Token> tokens;
    lexicalAnalysis(src, tokens);
    syntaxAnalysis(tokens);
    optimize();
    return true;
}
std::string Interpreter::result(const char *env)
{
    updateSymbolTable(env);
    return m_node->calc(&m_symTable).tostring();
}
int Interpreter::getNodeCount() const
{
    return m_node->getNodeCount();
}
void Interpreter::lexicalAnalysis(const char *src, std::vector<Token>& tokens)
{
    for (;;) {
        while (isspace(*src)) ++src;
        if (*src == 0) break;
        else if (*src == '"') {
            Token token = {TT_string, src};
            while (*++src != '"');
            token.end = ++src;
            tokens.push_back(token);
        }
        else if (isdigit(*src)) {
            Token token = {TT_number, src};
            while (isdigit(*++src));
            token.end = src;
            tokens.push_back(token);
        }
        else if (isalpha(*src) || *src == '_') {
            Token token = {TT_identify, src};
            ++src;
            while (isalpha(*src) || isdigit(*src) || *src == '_') ++src;
            token.end = src;
            tokens.push_back(token);
        }
        else {
            switch (*src) {
                case '+': case '-':
                case '*': case '/':
                case '(': case ')':
                case '=': case ';':
                    {
                        Token token = {TT_operator, src, src + 1};
                        tokens.push_back(token);
                        ++src;
                    }
                    break;
                default:
                    assert(0);
                    break;
            }
        }
    }
}
void Interpreter::syntaxAnalysis(const std::vector<Token>& tokens)
{
    std::vector<ExprNode*> nodeStack;
    std::vector<Token> opStack;
    // infix to suffix
    for (int i = 0; i < (int)tokens.size(); ++i) {
        const Token& token = tokens[i];
        if (token.t != TT_operator) {
            nodeStack.push_back(new ExprNode(token, m_symTable));
        }
        else {
            if (token.begin[0] == '(') opStack.push_back(token);
            else if (token.begin[0] == ')') {
                while (opStack.back().begin[0] != '(') {
                    nodeStack.push_back(new ExprNode(opStack.back(), m_symTable));
                    opStack.pop_back();
                }
                opStack.pop_back();
            }
            else {
                while (!opStack.empty()) {
                    const Token& lastOp = opStack.back();
                    if (lastOp.begin[0] == '(') break;
                    if (ExprNode::getOpPriority(lastOp.begin[0]) < ExprNode::getOpPriority(token.begin[0])) break;
                    nodeStack.push_back(new ExprNode(lastOp, m_symTable));
                    opStack.pop_back();
                }
                opStack.push_back(token);
            }
        }
    }
    while (!opStack.empty()) {
        nodeStack.push_back(new ExprNode(opStack.back(), m_symTable));
        opStack.pop_back();
    }

    // suffix to expression tree
    while (nodeStack.size() >= 3) {
        for (int i = (int)nodeStack.size() - 1; i >= 2; --i) {
            ExprNode *nodeOp = nodeStack[i], *nodeVal1 = nodeStack[i - 2], *nodeVal2 = nodeStack[i - 1];
            if (!nodeOp->isPureOperator() || nodeVal1->isPureOperator() || nodeVal2->isPureOperator()) continue;
            nodeOp->left = nodeVal1, nodeOp->right = nodeVal2;
            nodeStack[i - 2] = nodeOp;
            nodeStack.erase(nodeStack.begin() + i - 1, nodeStack.begin() + i + 1);
        }
    }
    assert(nodeStack.size() == 1);

    m_node = nodeStack[0];
}
void Interpreter::optimize()
{
    assert(m_node != NULL);
    m_node->optimizeConstExpr();
}
void Interpreter::updateSymbolTable(const char *env)
{
    std::vector<Token> tokens;
    lexicalAnalysis(env, tokens);
    while (!tokens.empty()) {
        assert(tokens[0].t == TT_identify);
        assert(tokens[1].t == TT_operator && tokens[1].begin[0] == '=');
        assert(tokens[2].t == TT_number || tokens[2].t == TT_string);
        if (tokens.size() >= 4) {
            assert(tokens[3].t == TT_operator && tokens[3].begin[0] == ';');
        }

        int idx = m_symTable.getNameIndex(std::string(tokens[0].begin, tokens[0].end).c_str());
        m_symTable.setValue(idx, Value(tokens[2]));
        tokens.erase(tokens.begin(), tokens.begin() + (tokens.size() >= 4 ? 4 : 3));
    }
}

int main()
{
    // test 1
    {
        Interpreter i;
        assert(i.compile("3*(5+2)/2"));
        assert(i.getNodeCount() == 1);
        assert(i.result("") == "10");
    }
    // test 2
    {
        Interpreter i;
        assert(i.compile("3*(a1+2)/a2"));
        assert(i.getNodeCount() == 7);
        assert(i.result("a1=3;a2=5;") == "3");
        assert(i.result("a1=6;a2=2;") == "12");
    }
    // test 3
    {
        Interpreter i;
        assert(i.compile("\"abc\"*3"));
        assert(i.getNodeCount() == 1);
        assert(i.result("") == "abcabcabc");
    }
    // test 4
    {
        Interpreter i;
        assert(i.compile("(\"abc\" + (2 + 5)) * a + 1"));
        assert(i.getNodeCount() == 5);
        assert(i.result("a=2;") == "abc7abc71");
    }

    Interpreter i;

    cout << "Input the expression: " << endl;
    std::string input;
    getline(cin, input);
    assert(i.compile(input.c_str()));

    for (;;) {
        cout << "Input the environment: " << endl;
        getline(cin, input);
        if (input == "exit") break;
        cout << "result : " << i.result(input.c_str()) << endl;
    }
}
