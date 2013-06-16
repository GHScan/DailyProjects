
#ifndef SOURCE_FILE_PROTO_H
#define SOURCE_FILE_PROTO_H

struct FunctionProto {
    string retType;
    vector<string> argTypes;
    vector<string> argNames;
    StmtNodePtr body;
};
typedef shared_ptr<FunctionProto> FunctionProtoPtr;

struct SourceProto {
    vector<StmtNodePtr> globalVars;
    unordered_map<string, FunctionProtoPtr> funcs;
};

#endif
