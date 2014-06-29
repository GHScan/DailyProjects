#include "pch.h"
#include "SObjectManager.h"


SObjectManager::SObjectManager(int initGCThreshold): 
    mFirstObj(nullptr), mGCThreshold(initGCThreshold) {
}

SObjectManager::~SObjectManager() {
    performFullGC();
}

void SObjectManager::mark(SValue v) {
    switch (v.getType()) {
        case SVT_Reserved:
        case SVT_Bool:
        case SVT_Number:
        case SVT_String:
        case SVT_Symbol:
            break;

        case SVT_Pair: {
            auto p = v.getObject<SPair>();
            if (p->mark()) {
                mMarkedObjs.push(p);
            }
           }
            break;
        case SVT_Env: {
            auto p = v.getObject<SEnv>();
            if (p->mark()) {
                mMarkedObjs.push(p);
            }
          }
            break;
        case SVT_Func: {
            auto p = v.getObject<SFunc>();
            if (p->mark()) {
                mMarkedObjs.push(p);
            }
           }
            break;
        case SVT_NativeFunc: {
            auto p = v.getObject<SNativeFunc>();
            if (p->mark()) {
                mMarkedObjs.push(p);
            }
         }
            break;
        case SVT_Class: {
            auto p = v.getObject<SClass>();
            if (p->mark()) {
                mMarkedObjs.push(p);
            }
            }
            break;

        default:
            ASSERT(0);
            break;
    }
}

void SObjectManager::mark(SObject *obj) {
    if (obj && obj->mark()) {
        mMarkedObjs.push(obj);
    }
}

void SObjectManager::performFullGC() {
    if (mRootCollector) {
        mRootCollector(this);
    }

    while (!mMarkedObjs.empty()) {
        auto obj = mMarkedObjs.front();
        mMarkedObjs.pop();

        switch (obj->getType()) {
            case SVT_Pair: {
                auto p = static_cast<SPair*>(obj);
                mark(p->car);
                mark(p->cdr);
               }
                break;
            case SVT_Env: {
                auto p = static_cast<SEnv*>(obj);
                mark(p->prevEnv);
                for (int i = 0; i < p->vCount; ++i) {
                    mark(p->values[i]);
                }
              }
                break;
            case SVT_Func: {
                auto p = static_cast<SFunc*>(obj);
                mark(p->env);
               }
                break;
            case SVT_NativeFunc: {
                auto p = static_cast<SNativeFunc*>(obj);
                (void)p;
                 }
                break;
            case SVT_Class: {
                auto p = static_cast<SClass*>(obj);
                mark(p->prevEnv);
            }
                break;
            default:
                ASSERT(0);
                break;
        }
    }

    SObject **pp = &mFirstObj;
    while (*pp != nullptr) {
        auto p = *pp;
        if (p->isMarked()) {
            p->unmark();
            pp = &p->next;
        } else {
            *pp = p->next;

            switch (p->getType()) {
                case SVT_Pair: 
                    destroyObject(static_cast<SPair*>(p));
                    break;
                case SVT_Env: 
                    destroyObject(static_cast<SEnv*>(p));
                    break;
                case SVT_Func:
                    destroyObject(static_cast<SFunc*>(p));
                    break;
                case SVT_NativeFunc:
                    destroyObject(static_cast<SNativeFunc*>(p));
                    break;
                case SVT_Class: 
                    destroyObject(static_cast<SClass*>(p));
                    break;
                default:
                    ASSERT(0);
                    break;
            }
        }
    }

    mGCThreshold = max(mMemPool.getMemorySize() * 2, mGCThreshold);
}
