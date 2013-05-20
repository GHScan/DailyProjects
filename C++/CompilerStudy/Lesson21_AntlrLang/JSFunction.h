
#ifndef JS_FUNCTION_H
#define JS_FUNCTION_H

#include "JSValue.h"

struct IStmtNode;
typedef shared_ptr<IStmtNode> StmtNodePtr;

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
    vector<JSValue> constTable;
    FuncMeta(): argCount(0), localCount(0), tempCount(0){}
    int getLocalSpace() const { return localCount + tempCount; }
    int getConstIdx(const JSValue& cv) {
        for (int i = 0; i < (int)constTable.size(); ++i) {
            if (cv == constTable[i]) return i;
        }
        constTable.push_back(cv);
        return (int)constTable.size() - 1;
    }
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
