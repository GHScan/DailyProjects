
#ifndef JS_STRING_H
#define JS_STRING_H

#include "GCObject.h"

struct JSString:
    public GCObject {
    char *buf;
    int size;
    int hashCode;

    int getHashCode() {
        if (hashCode == 0) hashCode = hashOf(buf, buf + size);
        return hashCode;
    }
    void attach(const char *str) { buf = (char*)str; size = (int)strlen(str); hashCode = 0; }
    void detach(){ buf = NULL; size = 0; hashCode = 0;}

    ~JSString() { free(buf); }
private:
    JSString(const char *str): GCObject(GCT_String), hashCode(0) {
        size = (int)strlen(str);
        buf = (char*)malloc(size + 1);
        strcpy(buf, str);
        GCObjectManager::instance()->link(this);
    }
    JSString(): GCObject(GCT_String), buf(NULL), size(0), hashCode(0){}

private:
    friend class JSStringManager;
};

struct JSStringContentHash {
    int operator () (JSString* str) const {
        return str->getHashCode();
    }
};
struct JSStringContentEqual {
    bool operator () (JSString* left, JSString *right) const {
        if (left->size != right->size) return false;
        return memcmp(left->buf, right->buf, left->size) == 0;
    }
};

class JSStringManager {
public:
    JSString* get(const char *str);

    void notifyValidGCObjects(GCObject *obj);

    static void createInstance() { s_ins = new JSStringManager(); }
    static void destroyInstance() { delete s_ins; }
    static JSStringManager* instance() { return s_ins; }

private:
    unordered_set<JSString*, JSStringContentHash, JSStringContentEqual> m_strs;

private:
    JSStringManager(){}
    JSStringManager& operator = (const JSStringManager&);
    JSStringManager(const JSStringManager&);

    static JSStringManager *s_ins;
};

#endif
