#ifndef BYTECODECOMPILER_H
#define BYTECODECOMPILER_H

#include "AST.h"
#include "SScriptFunctionProto.h"

void compileToByteCode(
        SScriptFunctionProto* parent,
        ASTNode_Lambda *lambda, 
        vector<SScriptFunctionProtoPtr> *protos);

#endif
