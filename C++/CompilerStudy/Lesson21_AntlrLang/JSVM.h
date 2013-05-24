

#ifndef JS_VM_H
#define JS_VM_H

#include "JSValue.h"

struct JSString;
struct GCObject;
struct JSFunction;
struct FuncMeta;
typedef shared_ptr<FuncMeta> FuncMetaPtr;

struct StackFrame {
    int oldStackSize;
    JSFunction *func;
    JSValue *localConstPtr[2];
    int ip;
    StackFrame(JSFunction *_func, JSValue *_local);
};

class JSVM {
public:
    static void createInstance() { s_ins = new JSVM(); }
    static void destroyInstance() { delete s_ins; }
    static JSVM* instance() { return s_ins; }

    const JSValue& getGlobal(const JSValue &key) { return m_globals[key]; }
    void setGlobal(const JSValue& key, const JSValue& value) {
        if (value.isNil()) m_globals.erase(key);
        else m_globals[key] = value;
    }
    void pushFrame(JSFunction *func, JSValue *argsBegin);
    void popFrame();
    StackFrame* topFrame(int topIdx = 0) { 
        assert(topIdx <= 0);
        return &m_frames[m_frames.size() - 1 + topIdx];
    }
    int getMetaIdx(const FuncMetaPtr& meta) {
        for (int i = 0; i < (int)m_metas.size(); ++i) {
            if (m_metas[i] == meta) return i;
        }
        m_metas.push_back(meta);
        return (int)m_metas.size() - 1;
    }
    const FuncMetaPtr& getMetaFromIdx(int idx) {
        return m_metas[idx];
    }

    JSValue* pushStack(const JSValue *begin, const JSValue* end) { 
        JSValue *r = &m_values.back();
        m_values.insert(m_values.end(), begin, end);
        return r;
    }
    void popStack(int n) { m_values.resize(m_values.size() - n);}

    void accessGCObjects(vector<GCObject*> &objs);
private:
    JSVM();
    ~JSVM();
    JSVM& operator = (const JSVM& o);
    JSVM(const JSVM& o);

    static JSVM *s_ins;
private:
    unordered_map<JSValue, JSValue> m_globals;
    vector<JSValue> m_values;
    vector<StackFrame> m_frames;
    vector<FuncMetaPtr> m_metas;
};

#endif
