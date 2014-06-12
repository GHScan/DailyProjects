#include "pch.h"
#include "SchemeMemoryManager.h"
#include "SchemeDynamicObject.h"
#include "SchemeStaticObject.h"

SchemeMemoryManager::SchemeMemoryManager(int initHeapSize):
    mDynamicObjCount(0), mDynamicObjThresold(initHeapSize),
    mWorkingStaticHeap(initHeapSize), mFreeStaticHeap(initHeapSize), mStaticHeapFreeOff(0) {
    mDynamicObjList.setPointer(nullptr);
}

SchemeMemoryManager::~SchemeMemoryManager() {
    mOOMHandler(this);

    assert(mDynamicObjList.getPointer() == nullptr && mDynamicObjCount == 0);
    assert(mStaticHeapFreeOff == 0);
}

void SchemeMemoryManager::setOOMHandler(function<void(SchemeMemoryManager*)> handler) {
    mOOMHandler = handler;
}

void SchemeMemoryManager::addDynamicObject(SchemeDynamicObject* dy) {
    dy->next.setPointer(mDynamicObjList.getPointer());
    mDynamicObjList.setPointer(dy);

    if (++mDynamicObjCount > mDynamicObjThresold) {
        mOOMHandler(this);
    }
}

SchemeRef* SchemeMemoryManager::mallocSchemeRefs(int refCount) {
    for (; mStaticHeapFreeOff + refCount > (int)mWorkingStaticHeap.size(); ) {
        mOOMHandler(this);
    }

    SchemeRef *p = &mWorkingStaticHeap[mStaticHeapFreeOff];
    mStaticHeapFreeOff += refCount;
    return p;
}

struct GCContext {
    SchemeRef *oldBegin;
    SchemeRef *oldEnd;
    SchemeRef *scanned;
    SchemeRef *free;

    bool isBelongOldHeap(const SchemeStaticObject *obj) const {
        return &obj->first >= oldBegin && &obj->first < oldEnd;
    }

    bool isBelongNewHeap(const SchemeStaticObject *obj) const {
        return &obj->first >= scanned && &obj->first < free;
    }

    SchemeStaticObject* moveToNewHeap(SchemeStaticObject* obj) {
        assert(isBelongOldHeap(obj));

        if (obj->second.getType() != SchemeRef::TYPE_StaticObject || !isBelongNewHeap(obj->second.getStaticObject())) {
            int refCount = obj->getRefCount();
            SchemeStaticObject *newObj = (SchemeStaticObject*)free;
            free += refCount;
            memcpy(newObj, obj, refCount * sizeof(SchemeRef));
            obj->second.setStaticObject(newObj);
        }

        return obj->second.getStaticObject();
    }

    void updateObjectFieldsForNewHeap() {
        for (; scanned < free; ) {
            int refCount = ((SchemeStaticObject*)scanned)->getRefCount();
            for (int i = 0; i < refCount; ++i) {
                SchemeRef *ref = &scanned[i];
                if (ref->getType() == SchemeRef::TYPE_StaticObject) {
                    ref->setStaticObject(moveToNewHeap(ref->getStaticObject()));
                }
            }
            scanned += refCount;
        }
    }
};

GCContext* SchemeMemoryManager::beginGC() {
    mFreeStaticHeap.resize(max(mFreeStaticHeap.size(), mWorkingStaticHeap.size()));

    GCContext *ctx = new GCContext;
    ctx->oldBegin = &mWorkingStaticHeap[0];
    ctx->oldEnd = ctx->oldBegin + mStaticHeapFreeOff;
    ctx->scanned = &mFreeStaticHeap[0];
    ctx->free = ctx->scanned;
    return ctx;
}

void SchemeMemoryManager::addGCRoot(GCContext *ctx, SchemeStaticObject **st) {
    assert(st != nullptr);
    *st = ctx->moveToNewHeap(*st);
}

void SchemeMemoryManager::addGCRoot(GCContext *ctx, SchemeDynamicObject *dy) {
    dy->mark();
}

void SchemeMemoryManager::endGC(GCContext *ctx) {
    // copy and compacting
    ctx->updateObjectFieldsForNewHeap();

    // mark and sweep
    mDynamicObjCount = 0;
    for (TaggedPointer *p = &mDynamicObjList; p->getPointer() != nullptr;) {
        SchemeDynamicObject* obj = (SchemeDynamicObject*)p->getPointer();
        if (obj->isMarked()) {
            obj->unmark();
            p = &obj->next;
            ++mDynamicObjCount;
        } else {
            p->setPointer(obj->next.getPointer());
            delete obj;
        }
    }

    mStaticHeapFreeOff = ctx->free - &mFreeStaticHeap[0];
    mFreeStaticHeap.swap(mWorkingStaticHeap);
    mFreeStaticHeap.resize(max((int)mFreeStaticHeap.size(), mStaticHeapFreeOff * 3 / 2));

    mDynamicObjThresold = max(mDynamicObjThresold, mDynamicObjCount * 3 / 2);

    delete ctx;
}
