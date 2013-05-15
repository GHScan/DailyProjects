
#ifndef JS_STRING_H
#define JS_STRING_H

#include "GCObject.h"

struct JSString:
    public GCObject {
    string str;

private:
    JSString(const char *_str): GCObject(GCT_String), str(_str){}

private:
    friend class JSStringManager;
};

class JSStringManager {
public:
    JSString* get(const char *str);

    static JSStringManager* intance() {
        static JSStringManager s_ins;
        return &s_ins;
    }

private:
    JSStringManager(){}
    JSStringManager& operator = (const JSStringManager&);
    JSStringManager(const JSStringManager&);
};

#endif
