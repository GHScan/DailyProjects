
#ifndef LUA_STRING_H
#define LUA_STRING_H

#include "GCObject.h"

class LuaString:
    public GCObject {
public:
    void destroy() {
        delete this;
    }

    const char* buf() const { return m_buf; }
    int size() const { return m_size; }

    void attach(char *buf, int size) {
        this->~LuaString();
        m_buf = buf, m_size = size;
    }
    void detach() { m_buf = NULL, m_size = 0; m_contentHash = 0; }

    int getContentHash() const;
    bool isContentEqual(const LuaString& o) const {
        if (m_size == o.m_size) {
            return memcmp(m_buf, o.m_buf, m_size) == 0;
        }
        return false;
    }
    bool isContentLess(const LuaString& o) const {
        int msize = min(m_size, o.m_size);
        int r = memcmp(m_buf, o.m_buf, msize);
        if (r == 0) return m_size < o.m_size;
        else return r;
    }
private:
    friend class StringPool;

    LuaString(): GCObject(OT_String), m_buf(NULL), m_size(0), m_contentHash(0){}
    LuaString(const char *buf);
    LuaString(const char *buf, int size);
    ~LuaString();

private:
    LuaString(const LuaString&);
    LuaString& operator = (const LuaString&);
    bool operator == (const LuaString& o) const;
    bool operator < (const LuaString& o) const;
private:
    char *m_buf;
    int m_size;
    mutable int m_contentHash;
};

struct LuaStringContentHash {
    int operator () (const LuaString* o) const {
        return o->getContentHash();
    }
};
struct LuaStringContentEqual {
    bool operator () (const LuaString* l, const LuaString* r) const {
        return l->isContentEqual(*r);
    }
};

class StringPool {
public:
    StringPool();
    ~StringPool();

    LuaString* createString(const char *buf);
    LuaString* createString(const char *buf, int size);

    void onFullGCEnd(GCObject *head);
private:
    StringPool(const StringPool&);
    StringPool& operator = (const StringPool&);

private:
    unordered_set<LuaString*, LuaStringContentHash, LuaStringContentEqual> m_strSet;
};

#endif
