
#include "pch.h"

#include "AST.h"
#include "LuaFunction.h"

ExpNode_Lambda::ExpNode_Lambda(const LuaFunctionMetaPtr& _meta): 
    IExpNode(_meta->line), meta(_meta){
}
