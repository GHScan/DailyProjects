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

struct IASTVisitor {
    virtual ~IASTVisitor() {}
    virtual void visit(ASTNode_Literal *node) = 0;
    virtual void visit(ASTNode_If *node) = 0;
    virtual void visit(ASTNode_Lambda *node) = 0;
    virtual void visit(ASTNode_Begin *node) = 0;
    virtual void visit(ASTNode_Define *node) = 0;
    virtual void visit(ASTNode_GetVar *node) = 0;
    virtual void visit(ASTNode_SetVar *node) = 0;
};

struct IASTNode {
    virtual ~IASTNode() {}
    virtual void acceptVisitor(IASTVisitor *v) = 0;
};
typedef shared_ptr<IASTNode> ASTNodePtr;

struct ASTNode_Literal: public IASTNode {
    int index;

    virtual void acceptVisitor(IASTVisitor *v) { v->visit(this); }
};

struct ASTNode_If: public IASTNode {
    ASTNodePtr predNode; 
    ASTNodePtr thenNode;
    ASTNodePtr elseNode;

    virtual void acceptVisitor(IASTVisitor *v) { v->visit(this); }
};

struct ASTNode_Lambda: public IASTNode {
    vector<string> formals;
    ASTNodePtr body;

    virtual void acceptVisitor(IASTVisitor *v) { v->visit(this); }
};

struct ASTNode_Begin: public IASTNode {
    vector<ASTNodePtr> nodes;

    virtual void acceptVisitor(IASTVisitor *v) { v->visit(this); }
};

struct ASTNode_Define: public IASTNode {
    string name;
    ASTNodePtr rightNode;

    virtual void acceptVisitor(IASTVisitor *v) { v->visit(this); }
};

struct ASTNode_GetVar: public IASTNode {
    VarAddress address;

    virtual void acceptVisitor(IASTVisitor *v) { v->visit(this); }
};

struct ASTNode_SetVar: public IASTNode {
    VarAddress name;
    ASTNodePtr rightNode;

    virtual void acceptVisitor(IASTVisitor *v) { v->visit(this); }
};

#endif
