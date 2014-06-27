#ifndef AST_H
#define AST_H

#include "Token.h"

struct IASTNodeVisitor {
    virtual ~IASTNodeVisitor() {}
};

struct IASTNode {
    virtual ~IASTNode() {}
    virtual void accept(IASTNodeVisitor *v) = 0;

    Token token;
    explicit IASTNode(Token _token): token(_token) {
    }
};

#endif
