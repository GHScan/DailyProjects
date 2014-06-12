#ifndef SCHEME_MEMORY_MANAGER_H
#define SCHEME_MEMORY_MANAGER_H

#include "SchemeRef.h"
#include "TaggedPointer.h"

class SchemeDynamicObject;
class SchemeStaticObject;
struct GCContext;

class SchemeMemoryManager {
public:
    SchemeMemoryManager(int initHeapSize = 32);
    SchemeMemoryManager(const SchemeMemoryManager& o) = delete;
    SchemeMemoryManager& operator = (const SchemeMemoryManager& o) = delete;
    ~SchemeMemoryManager();

    void setOOMHandler(function<void(SchemeMemoryManager*)> handler);

    void addDynamicObject(SchemeDynamicObject* dy);
    SchemeRef* mallocSchemeRefs(int refCount);

    GCContext* beginGC();
    void addGCRoot(GCContext *ctx, SchemeStaticObject **st);
    void addGCRoot(GCContext *ctx, SchemeDynamicObject *dy);
    void endGC(GCContext *ctx);

private:
    TaggedPointer mDynamicObjList;
    int mDynamicObjCount;
    int mDynamicObjThresold;
    vector<SchemeRef> mWorkingStaticHeap;
    vector<SchemeRef> mFreeStaticHeap;
    int mStaticHeapFreeOff;
    function<void(SchemeMemoryManager*)> mOOMHandler;
};

#endif
