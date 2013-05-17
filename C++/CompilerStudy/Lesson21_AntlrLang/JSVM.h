

#ifndef JS_VM_H
#define JS_VM_H

#include "JSValue.h"

struct JSString;
struct GCObject;
struct JSFunction;

struct StackFrame {
    JSValue* stack;
    JSValue *ret;
    JSFunction *func;
};

class JSVM {
public:
    static JSVM* instance() {
        static JSVM s_ins;
        return &s_ins;
    }

    const JSValue& getGlobal(const JSValue &key) { return m_globals[key]; }
    void setGlobal(const JSValue& key, const JSValue& value) {
        if (value.isNil()) m_globals.erase(key);
        else m_globals[key] = value;
    }
    void pushFrame(JSFunction *func, int retStackIdx);
    void popFrame();
    StackFrame* topFrame(int topIdx = 0) { 
        assert(topIdx <= 0);
        return &m_frames[m_frames.size() - 1 + topIdx];
    }

    void accessGCObjects(vector<GCObject*> &objs);
private:
    JSVM();
    ~JSVM(){}
    JSVM& operator = (const JSVM& o);
    JSVM(const JSVM& o);

private:
    unordered_map<JSValue, JSValue> m_globals;
    vector<JSValue> m_values;
    vector<StackFrame> m_frames;
};

#endif
