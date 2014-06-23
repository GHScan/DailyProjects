#ifndef ASTCOMPILER_H
#define ASTCOMPILER_H

#include "AST.h"
#include "SValue.h"

class SymbolTable;

ASTNodePtr compileToAST(SymbolTable *symTable, vector<SValue> &literals, SValue exp);

#endif
