#include "pch.h"
#include "SObjectManager.h"

const int SObjectManager::AGE_2_GEN[SObject::MAX_AGE + 1] = {
    0, 1, 2, 2, 2, 3, 3, 3, 3, 
};

const int SObjectManager::GEN_2_AGE[GEN_N] = {
    0, 1, 4, 8,
};

SObjectManager::SObjectManager(): 
    mGCAge(0) {

    for (int i = 0; i <= SObject::MAX_AGE; ++i) {
        ASSERT(GEN_2_AGE[AGE_2_GEN[i]] >= i);
    }

    for (int i = 0; i < GEN_N; ++i) {
        mGens[i].size = 0;
        mGens[i].firstObj = nullptr;
    }

    mGens[0].initThreshold = mGens[0].threshold = 256 * 1024;
    mGens[1].initThreshold = mGens[1].threshold = 2 * 1024 * 1024;
    mGens[2].initThreshold = mGens[2].threshold = 16 * 1024 * 1024;
    mGens[3].initThreshold = mGens[3].threshold = 64 * 1024 * 1024;

    mMarkedObjs.reserve(32 * 1024);
}

SObjectManager::~SObjectManager() {
    performGC(GEN_N - 1);
}

void SObjectManager::mark(SValue v) {
    if (v.isObject()) mark(v.getObject());
}

void SObjectManager::mark(SObject *obj) {
    if (obj && obj->getAge() <= mGCAge && obj->mark()) {
        mMarkedObjs.push_back(obj);
    }
}

bool SObjectManager::isOldGenForGC(SObject *obj) {
    return obj && obj->getAge() > mGCAge;
}

static void markChidren(SObjectManager *mgr, SObject *obj) {
    switch (obj->getType()) {
        case SVT_Pair: {
            auto p = static_cast<SPair*>(obj);
            mgr->mark(p->getCar());
            mgr->mark(p->getCdr());
           }
            break;
        case SVT_Env: {
            auto p = static_cast<SEnv*>(obj);
            mgr->mark(p->prevEnv);
            for (int i = 0; i < p->vCount; ++i) {
                mgr->mark(p->values[i]);
            }
          }
            break;
        case SVT_Func: {
            auto p = static_cast<SFunc*>(obj);
            mgr->mark(p->env);
           }
            break;
        case SVT_NativeFunc: {
         }
            break;
        case SVT_Class: {
            auto p = static_cast<SClass*>(obj);
            mgr->mark(p->env);
        }
            break;
        default:
            ASSERT(0);
            break;
    }
}

void SObjectManager::performGC(int gen) {
    mGCAge = GEN_2_AGE[gen];

    if (mRootCollector) {
        mRootCollector(this);
    }

    auto &stable = SObject::sYoungContainer;
    for (auto p : stable) {
        if (p->getAge() > mGCAge) {
            markChidren(this, p);
        }
    }

    while (!mMarkedObjs.empty()) {
        auto obj = mMarkedObjs.back();
        mMarkedObjs.pop_back();
        markChidren(this, obj);
    }


    for (auto iter = stable.begin(); iter != stable.end();) {
        if ((*iter)->getAge() <= mGCAge && !(*iter)->isMarked()) {
            iter = stable.erase(iter);
        } else {
            ++iter;
        }
    }

    for (int i = gen; i >= 0; --i) {
        uint64_t upSize = 0;
        uint64_t remainSize = 0;

        SObject** pp = &mGens[i].firstObj;
        SObject** nextpp = (i + 1) < GEN_N ? &mGens[i + 1].firstObj : nullptr;
        while (*pp != nullptr) {
            SObject *obj = *pp;

            if (!obj->isMarked()) {
                *pp = obj->next;

                switch (obj->getType()) {
                    case SVT_Pair: destroyObject(static_cast<SPair*>(obj)); break;
                    case SVT_Env: destroyObject(static_cast<SEnv*>(obj)); break;
                    case SVT_Func: destroyObject(static_cast<SFunc*>(obj)); break;
                    case SVT_NativeFunc: destroyObject(static_cast<SNativeFunc*>(obj)); break;
                    case SVT_Class: destroyObject(static_cast<SClass*>(obj)); break;
                    default: ASSERT(0); break;
                }

            } else {
                obj->unmark();
                obj->growUp();

                int size = 0;
                switch (obj->getType()) {
                    case SVT_Pair: size = static_cast<SPair*>(obj)->estimateSize(); break;
                    case SVT_Env: size = static_cast<SEnv*>(obj)->estimateSize(); break;
                    case SVT_Func: size = static_cast<SFunc*>(obj)->estimateSize(); break;
                    case SVT_NativeFunc: size = static_cast<SNativeFunc*>(obj)->estimateSize(); break;
                    case SVT_Class: size = static_cast<SClass*>(obj)->estimateSize(); break;
                    default: ASSERT(0); break;
                }

                if (AGE_2_GEN[obj->getAge()] > i) {
                    upSize += size;

                    *pp = obj->next;
                    obj->next = *nextpp;
                    *nextpp = obj;

                } else {
                    remainSize += size;

                    pp = &obj->next;
                }
            }
        }

        if (i + 1 < GEN_N) mGens[i + 1].size += upSize;

        mGens[i].size = remainSize;

        uint64_t totalSize = upSize + remainSize;
        switch (i) {
            case 0: mGens[i].threshold = max(totalSize * 16, mGens[i].threshold); break;
            case 1: mGens[i].threshold = max(totalSize * 16, mGens[i].threshold); break;
            case 2: mGens[i].threshold = max(totalSize * 16, mGens[i].threshold); break;
            case 3: mGens[i].threshold = max(totalSize * 16, mGens[i].threshold); break;
            default: ASSERT(0); break;
        }
    }

    for (auto iter = stable.begin(); iter != stable.end();) {
        if ((*iter)->getAge() == 1) {
            iter = stable.erase(iter);
        } else {
            ++iter;
        }
    }
}
