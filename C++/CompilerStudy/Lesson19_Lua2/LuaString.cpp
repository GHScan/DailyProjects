
#include "pch.h"
#include "LuaString.h"
#include "LuaVM.h"

//========== LuaString ==========
LuaString::LuaString(const char *buf):
    GCObject(OT_String), m_contentHash(0) {
    m_size = (int)strlen(buf);
    m_buf = new char[m_size + 1];
    memcpy(m_buf, buf, m_size);
    m_buf[m_size] = 0;
}
LuaString::LuaString(const char *buf, int size):
    GCObject(OT_String), m_size(size), m_contentHash(0) {
    m_buf = new char[m_size + 1];
    memcpy(m_buf, buf, m_size);
    m_buf[m_size] = 0;
}
LuaString::~LuaString() {
    sdelete(m_buf);
    m_size = m_contentHash = 0;
}
int LuaString::getContentHash() const {
    if (m_contentHash == 0) {
        hash_combine(m_contentHash, m_contentHash);
        for (int i = 0; i < m_size; ++i) {
            hash_combine(m_contentHash, m_buf[i]);
        }
    }
    return m_contentHash;
}

//========== StringPool ==========
StringPool::StringPool() {
}
StringPool::~StringPool() {
    ASSERT(m_strSet.empty());
}
LuaString* StringPool::createString(const char *buf) {
    return createString(buf, (int)strlen(buf));
}
LuaString* StringPool::createString(const char *buf, int size) {
    LuaString *r = NULL;
    LuaString finder; finder.attach((char*)buf, size);
    auto iter = m_strSet.find(&finder);
    finder.detach();
    if (iter != m_strSet.end()) {
        r = *iter;
    } else {
        r = new LuaString(buf, size);
        m_strSet.insert(r);
        LuaVM::instance()->getGCObjManager()->linkObject(r);
    }
    return r;
}
void StringPool::onFullGCEnd(GCObject *head) {
    m_strSet.clear();
    while (head != NULL) {
        if (head->objType == GCObject::OT_String) {
            m_strSet.insert(static_cast<LuaString*>(head));
        }
        head = head->next;
    }
}
