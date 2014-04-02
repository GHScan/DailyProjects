#include "pch.h" 

#include <assert.h>

#include <string>
#include <vector>

struct Token
{
    enum Type
    {
        TT_number = 1,
        TT_notation,
    };
    Type type;
    std::string value;
};

struct ExprNode_BinaryOp;
struct ExprNode_NumberNode;
struct IExprNodeVisitor;
struct IExprNode
{
    virtual ~IExprNode() {}
    virtual void accept(IExprNodeVisitor *p) = 0;
};
struct IExprNodeVisitor
{
    virtual ~IExprNodeVisitor() {}
    virtual void visit(ExprNode_BinaryOp* node) = 0;
    virtual void visit(ExprNode_NumberNode *node) = 0;
};

struct ExprNode_BinaryOp:
    public IExprNode
{
    std::string op;
    IExprNode *left, *right;
    ExprNode_BinaryOp(const std::string& op, IExprNode* left, IExprNode* right)
    {
        this->op = op, this->left = left, this->right = right;
    }
    virtual void accept(IExprNodeVisitor *p) { p->visit(this); }
};

struct ExprNode_NumberNode:
    public IExprNode
{
    int number;
    ExprNode_NumberNode(const std::string& s)
    {
        number = atoi(s.c_str());
    }
    virtual void accept(IExprNodeVisitor *p) { p->visit(this); }
};

class ExprNodeVisitor_Printer:
    public IExprNodeVisitor
{
private:
    virtual void visit(ExprNode_BinaryOp* node)
    {
        cout << '(';
        node->left->accept(this);
        cout << node->op;
        node->right->accept(this);
        cout << ')';
    }
    virtual void visit(ExprNode_NumberNode *node)
    {
        cout << node->number;
    }
};
class ExprNodeVisitor_Eval:
    public IExprNodeVisitor
{
private:
    virtual void visit(ExprNode_BinaryOp* node)
    {
        node->left->accept(this);
        node->right->accept(this);
        if (node->op == "+") {
            int v = m_values.back();
            m_values.pop_back();
            m_values.back() += v;
        }
        else if (node->op == "-") {
            int v = m_values.back();
            m_values.pop_back();
            m_values.back() -= v;
        }
        else if (node->op == "*") {
            int v = m_values.back();
            m_values.pop_back();
            m_values.back() *= v;
        }
        else if (node->op == "/") {
            int v = m_values.back();
            m_values.pop_back();
            m_values.back() /= v;
        }
        else assert(0);
        if (m_values.size() == 1) {
            cout << m_values.back();
        }
    }
    virtual void visit(ExprNode_NumberNode *node)
    {
        m_values.push_back(node->number);
    }
private:
    std::vector<int> m_values;
};

/*
   expr = add           | return add()
   add = add + mul      | return new ExprNode_BinaryOp('+', add(), mul())
         add - mul      | return new ExprNode_BinaryOp('-', add(), mul())
         mul            | return mul()
   mul = mul * factor   | return new ExprNode_BinaryOp('*', mul(), factor())
         mul / factor   | return new ExprNode_BinaryOp('*', mul(), factor())
         factor         | return factor()
   factor = num         | return new ExprNode_NumberNode(num)
            (expr)      | return expr()
*/

/*
 expr = add             | return add()
 add = mul radd         | return radd(mul())
 radd = + mul radd      | return radd(new ExprNode_BinaryOp('+', arg0, mul()))
        - mul radd      | return radd(new ExprNode_BinaryOp('-', arg0, mul()))
        e               | return arg0
 mul = factor rmul      | return rmul(factor())
 rmul = * factor rmul   | return rmul(new ExprNode_BinaryOp('*', arg0, factor()))
        / factor rmul   | return rmul(new ExprNode_BinaryOp('/', arg0, factor()))
        e               | return arg0
 factor = num           | return new ExprNode_NumberNode(num)
          (expr)        | return expr()
*/
class Parser
{
public:
    Parser(const char *src): m_node(NULL)
    {
        std::vector<Token> tokens;
        int curToken = 0;
        lexicalAnalysis(src, tokens);
        m_node = expr(tokens, curToken);
    }
    ~Parser() { delete m_node; }
    void accept(IExprNodeVisitor *v)
    {
        m_node->accept(v);
    }

private:
    IExprNode* expr(const std::vector<Token>& tokens, int &curToken)
    {
        return add(tokens, curToken);
    }
    IExprNode* add(const std::vector<Token>& tokens, int &curToken)
    {
        IExprNode *left = mul(tokens, curToken);
        return radd(tokens, curToken, left);
    }
    IExprNode* radd(const std::vector<Token>& tokens, int &curToken, IExprNode* left)
    {
        if (curToken == tokens.size()) return left;
        const Token& t = tokens[curToken];
        if (t.type == Token::TT_notation && 
                (t.value == "+" || t.value == "-")) {
            ++curToken;
            IExprNode *opNode = new ExprNode_BinaryOp(t.value, left, mul(tokens, curToken));
            return radd(tokens, curToken, opNode);
        }
        else return left;
    }
    IExprNode* mul(const std::vector<Token>& tokens, int &curToken)
    {
        IExprNode *left = factor(tokens, curToken); 
        return rmul(tokens, curToken, left);
    }
    IExprNode* rmul(const std::vector<Token>& tokens, int &curToken, IExprNode *left)
    {
        if (curToken == tokens.size()) return left;
        const Token& t = tokens[curToken];
        if (t.type == Token::TT_notation && 
                (t.value == "*" || t.value == "/")) {
            ++curToken;
            IExprNode *opNode = new ExprNode_BinaryOp(t.value, left, factor(tokens, curToken));
            return rmul(tokens, curToken, opNode);
        }
        else return left;
    }
    IExprNode* factor(const std::vector<Token>& tokens, int &curToken)
    {
        if (curToken == tokens.size()) return NULL;
        const Token& t = tokens[curToken];
        if (t.type == Token::TT_number) {
            ++curToken;
            return new ExprNode_NumberNode(t.value);
        }
        else if (t.type == Token::TT_notation && t.value == "(") {
            ++curToken;
            IExprNode *node = expr(tokens, curToken);
            assert(tokens[curToken].type == Token::TT_notation && tokens[curToken].value == ")");
            ++curToken;
            return node;
        }
        return NULL;
    }
private:
    static void lexicalAnalysis(const char *src, std::vector<Token>& tokens)
    {
        for (;;) {
            while (isspace(src[0])) ++src;
            if (src[0] == 0) break;
            if (isdigit(src[0])) {
                const char *end = src;
                while (isdigit(*++end));
                Token t = {Token::TT_number, std::string(src, end)};
                tokens.push_back(t);
                src = end;
            }
            else {
                Token t = {Token::TT_notation, std::string(src, src + 1)};
                tokens.push_back(t);
                ++src;
            }
        }
    }
private:
    IExprNode *m_node;
};

int main()
{
    Parser parser("3 + (11 + 2*((3+5)-1))/3");

    IExprNodeVisitor *v = new ExprNodeVisitor_Printer();
    parser.accept(v);
    cout << endl;
    delete v;

    v = new ExprNodeVisitor_Eval();
    parser.accept(v);
    cout << endl;
    delete v;
}
