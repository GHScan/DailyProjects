
#ifndef SOURCE_FILE_PROTO_H
#define SOURCE_FILE_PROTO_H

#include "AST.h"

struct FunctionProto {
    string name;
    string retType;
    vector<pair<string, string> > argsTypeID;
    bool isVarArgs;
    StmtNodePtr body;
    FunctionProto(): isVarArgs(false){}
};
typedef shared_ptr<FunctionProto> FunctionProtoPtr;

struct SourceFileProto {
    vector<StmtNodePtr> globalVars;
    map<string, FunctionProtoPtr> funcs;
    map<string, FunctionProtoPtr> externFuncs;
};
typedef shared_ptr<SourceFileProto> SourceFileProtoPtr;

#endif
