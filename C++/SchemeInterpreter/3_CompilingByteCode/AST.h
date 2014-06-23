#ifndef AST_H
#define AST_H

#include "SymbolTable.h"

struct ASTNode_Literal;
struct ASTNode_If;
struct ASTNode_Lambda;
struct ASTNode_Begin;
struct ASTNode_Define;
struct ASTNode_GetVar;
struct ASTNode_SetVar;
struct ASTNode_Application;

struct IASTVisitor {
    virtual ~IASTVisitor() {}
    virtual void visit(ASTNode_Literal *node) = 0;
    virtual void visit(ASTNode_If *node) = 0;
    virtual void visit(ASTNode_Lambda *node) = 0;
    virtual void visit(ASTNode_Begin *node) = 0;
    virtual void visit(ASTNode_Define *node) = 0;
    virtual void visit(ASTNode_GetVar *node) = 0;
    virtual void visit(ASTNode_SetVar *node) = 0;
    virtual void visit(ASTNode_Application *node) = 0;
};

struct IASTNode {
    virtual ~IASTNode() {}
    virtual void acceptVisitor(IASTVisitor *v) = 0;
};
typedef shared_ptr<IASTNode> ASTNodePtr;

struct ASTNode_Literal: public IASTNode {
    int index;

    explicit ASTNode_Literal(int _index): index(_index) {
    }

    virtual void acceptVisitor(IASTVisitor *v) { v->visit(this); }
};

struct ASTNode_If: public IASTNode {
    ASTNodePtr predNode; 
    ASTNodePtr thenNode;
    ASTNodePtr elseNode;

    ASTNode_If(ASTNodePtr _pred, ASTNodePtr _then, ASTNodePtr _else):
        predNode(_pred), thenNode(_then), elseNode(_else) {
    }

    virtual void acceptVisitor(IASTVisitor *v) { v->visit(this); }
};

struct ASTNode_Lambda: public IASTNode {
    vector<string> formals;
    ASTNodePtr body;

    ASTNode_Lambda(const vector<string> &_formals, ASTNodePtr _body):
        formals(_formals), body(_body) {
    }

    ASTNode_Lambda(vector<string> &&_formals, ASTNodePtr _body):
        formals(_formals), body(_body) {
    }

    virtual void acceptVisitor(IASTVisitor *v) { v->visit(this); }
};

struct ASTNode_Begin: public IASTNode {
    vector<ASTNodePtr> nodes;

    virtual void acceptVisitor(IASTVisitor *v) { v->visit(this); }
};

struct ASTNode_Define: public IASTNode {
    string name;
    ASTNodePtr rightNode;

    ASTNode_Define(const string &_name, ASTNodePtr _right):
        name(_name), rightNode(_right) {
    }

    virtual void acceptVisitor(IASTVisitor *v) { v->visit(this); }
};

struct ASTNode_GetVar: public IASTNode {
    VarAddress address;

    explicit ASTNode_GetVar(VarAddress _address):
        address(_address) {
    }

    virtual void acceptVisitor(IASTVisitor *v) { v->visit(this); }
};

struct ASTNode_SetVar: public IASTNode {
    VarAddress address;
    ASTNodePtr rightNode;

    ASTNode_SetVar(const VarAddress &_address, ASTNodePtr _right):
        address(_address), rightNode(_right) {
    }

    virtual void acceptVisitor(IASTVisitor *v) { v->visit(this); }
};

struct ASTNode_Application: public IASTNode {
    ASTNodePtr func;
    vector<ASTNodePtr> actuals;

    ASTNode_Application(ASTNodePtr _func, const vector<ASTNodePtr> & _actuals):
        func(_func), actuals(_actuals) {
    }

    ASTNode_Application(ASTNodePtr _func, vector<ASTNodePtr> && _actuals):
        func(_func), actuals(_actuals) {
    }

    virtual void acceptVisitor(IASTVisitor *v) { v->visit(this); }
};

#endif
