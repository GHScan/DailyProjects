
#include "pch.h"

#include "LLVMCompiler.h"
#include "AST.h"
#include "SourceFileProto.h"

LLVMCompiler::LLVMCompiler(const SourceFileProtoPtr &proto) {
    for (auto stmt : proto->globalVars) {
        auto p = static_cast<StmtNode_DefineVarable*>(stmt.get());
        cout << p->type << "," << p->name << endl;
    }
    for (auto name2func : proto->funcs) {
        auto func = name2func.second.get();
        printf("%s %s(%s):%d\n", func->retType.c_str(), func->name.c_str(),func->isVarArgs ? "..." : "", static_cast<StmtNode_Block*>(func->body.get())->stmts.size());
    }
}
void LLVMCompiler::compile() {
}
void LLVMCompiler::run() {
}
