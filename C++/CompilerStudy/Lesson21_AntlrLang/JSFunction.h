
#ifndef JS_FUNCTION_H
#define JS_FUNCTION_H

#include "JSValue.h"
#include "GCObject.h"

struct IStmtNode;
typedef shared_ptr<IStmtNode> StmtNodePtr;

struct Function: 
    public GCObject {
    enum FuncType {
        FT_JS,
        FT_C,
    };
    FuncType funcType;
    void destroy();
    void accessGCObjects(vector<GCObject*> &objs);
    JSValue callFromC(JSValue* argsBegin, JSValue* argsEnd);
    void callFromVM(JSValue *argsBegin, JSValue* argsEnd);
protected:
    Function(FuncType _funcType): GCObject(GCT_Function), funcType(_funcType){ 
        GCObjectManager::instance()->link(this);
    }
};

struct FuncMeta {
    string fileName;
    int argCount;
    int localCount, tempCount;
    vector<int> codes;
    vector<int> ip2line;
    StmtNodePtr stmt;
    vector<JSValue> constTable;
    FuncMeta(const string &_fileName): fileName(_fileName), argCount(0), localCount(0), tempCount(0){}
    int getLocalSpace() const { return localCount + tempCount; }
    int getConstIdx(const JSValue& cv) {
        for (int i = 0; i < (int)constTable.size(); ++i) {
            if (cv == constTable[i]) return i;
        }
        constTable.push_back(cv);
        return (int)constTable.size() - 1;
    }
    void accessGCObjects(vector<GCObject*> &objs) {
        for (auto &v : constTable) {
            if (auto obj = v.gcAccess()) objs.push_back(obj);
        }
    }
};
typedef shared_ptr<FuncMeta> FuncMetaPtr;

struct JSFunction:
    public Function {
    FuncMetaPtr meta;
    static JSFunction* create(const FuncMetaPtr& meta) {
        return new JSFunction(meta);
    }
private:
    JSFunction(const FuncMetaPtr& _meta): Function(FT_JS), meta(_meta){}
};

typedef int (*CFuncT)(JSValue* argsBegin, JSValue* argsEnd);
struct CFuncEntry {
    const char *name;
    CFuncT func;
};
struct CFunction:
    public Function {
    CFuncT func;
    static CFunction* create(CFuncT func) {
        return new CFunction(func);
    }
private:
    CFunction(CFuncT _func): Function(FT_C), func(_func){}
};

inline void Function::destroy() {
    switch (funcType) {
        case Function::FT_C: delete static_cast<CFunction*>(this); break;
        case Function::FT_JS: delete static_cast<JSFunction*>(this); break;
        default: ASSERT(0); break;
    }
}

#endif
