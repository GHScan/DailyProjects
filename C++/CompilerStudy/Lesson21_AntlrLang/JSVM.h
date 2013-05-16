

#ifndef JS_VM_H
#define JS_VM_H

#include "JSValue.h"

struct JSString;
struct GCObject;

class JSVM {
public:
    JSValue* stack;

    static JSVM* instance() {
        static JSVM s_ins;
        return &s_ins;
    }

    void resizeStack(int n) {m_values.resize(n); stack = &m_values[0]; }
    const JSValue& getGlobal(const JSValue &key) { return m_globals[key]; }
    void setGlobal(const JSValue& key, const JSValue& value) {
        if (value.isNil()) m_globals.erase(key);
        else m_globals[key] = value;
    }

    void accessGCObjects(vector<GCObject*> &objs);
private:
    JSVM(){}
    ~JSVM(){}
    JSVM& operator = (const JSVM& o);
    JSVM(const JSVM& o);

private:
    vector<JSValue> m_values;
    unordered_map<JSValue, JSValue> m_globals;
};

#endif
