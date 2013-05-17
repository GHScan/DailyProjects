
#ifndef JS_FUNCTION_H
#define JS_FUNCTION_H

struct StmtNode;
typedef shared_ptr<StmtNode> StmtNodePtr;

struct Function {
    enum FuncType {
        FT_JS,
        FT_C,
    };
    FuncType type;
};

struct FuncMeta {
    int argCount;
    int localCount, tempCount;
    vector<int> codes;
    StmtNodePtr stmt;
    int getLocalSpace() const { return localCount + tempCount; }
};
typedef shared_ptr<FuncMeta> FuncMetaPtr;

struct JSFunction:
    public Function {
    FuncMetaPtr meta;
};

struct CFunction:
    public Function {
};

#endif
