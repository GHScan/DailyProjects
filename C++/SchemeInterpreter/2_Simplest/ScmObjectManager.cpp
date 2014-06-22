#include "pch.h"
#include "ScmObjectManager.h"
#include "ScmTypes.h"

ScmObjectManager::ScmObjectManager(): 
    mFirstObj(nullptr), mObjCount(0), mObjThreshold(16) {

    auto empty = create<ScmPair>(&ScmObject::EMPTY, nullptr, nullptr);
    empty->car = empty->cdr = empty;
}

ScmObjectManager::~ScmObjectManager() {
    mSymbols.clear();
    ScmObject::EMPTY = nullptr;

    performFullGC();

    ASSERT(mFirstObj == nullptr);
}

ScmSymbol* ScmObjectManager::getSymbol(const string &s) {
    auto iter = mSymbols.find(s);
    if (iter == mSymbols.end()) {
        iter = mSymbols.insert(make_pair(s, nullptr)).first;
    }

    if (iter->second == nullptr) {
        create<ScmSymbol>(&iter->second, &iter->first);
    }

    return iter->second;
}

void ScmObjectManager::setRootCollector(function<void(ScmObjectManager*)> rootCollector) {
    mRootCollector = rootCollector;
}

void ScmObjectManager::mark(ScmObject *obj) {
    if (obj != nullptr && obj->mark()) {
        mMarkedObjs.push(obj);
    }
}

void ScmObjectManager::performFullGC() {
    //------------------------------
    mRootCollector(this);

    mark(ScmObject::EMPTY);

    for (auto &kv : mSymbols) {
        mark(kv.second);
    }

    //------------------------------
    auto &markedObjs = mMarkedObjs;

    while (!markedObjs.empty()) {
        ScmObject *obj = markedObjs.front();
        markedObjs.pop();

        switch (obj->type) {
            case SOT_Pair:
                static_cast<ScmPair*>(obj)->markChildren(&markedObjs);
                break;
            case SOT_Vector:
                static_cast<ScmVector*>(obj)->markChildren(&markedObjs);
                break;
            case SOT_Dictionary:
                static_cast<ScmDictionary*>(obj)->markChildren(&markedObjs);
                break;
            case SOT_Env:
                static_cast<ScmEnv*>(obj)->markChildren(&markedObjs);
                break;
            case SOT_ScriptFunction:
                static_cast<ScmScriptFunction*>(obj)->markChildren(&markedObjs);
                break;
            default:
                break;
        }
    }

    //------------------------------
    mObjCount = 0;
    ScmObject **p = &mFirstObj;

    while (*p != nullptr) {
        ScmObject *obj = *p;

        if (obj->isMarked()) {
            obj->unmark();
            p = &obj->next;
            ++mObjCount;
        } else {
            *p = obj->next;

            switch (obj->type) {
                case ScmSymbol::TYPE:
                    delete static_cast<const ScmSymbol*>(obj);
                    break;
                case ScmInt::TYPE:
                    delete static_cast<const ScmInt*>(obj);
                    break;
                case ScmBigInt::TYPE:
                    delete static_cast<const ScmBigInt*>(obj);
                    break;
                case ScmDouble::TYPE:
                    delete static_cast<const ScmDouble*>(obj);
                    break;
                case ScmString::TYPE:
                    delete static_cast<const ScmString*>(obj);
                    break;
                case ScmPair::TYPE:
                    delete static_cast<const ScmPair*>(obj);
                    break;
                case ScmVector::TYPE:
                    delete static_cast<const ScmVector*>(obj);
                    break;
                case ScmDictionary::TYPE:
                    delete static_cast<const ScmDictionary*>(obj);
                    break;
                case ScmEnv::TYPE:
                    delete static_cast<const ScmEnv*>(obj);
                    break;
                case ScmScriptFunction::TYPE:
                    delete static_cast<const ScmScriptFunction*>(obj);
                    break;
                case ScmCFunction::TYPE:
                    delete static_cast<const ScmCFunction*>(obj);
                    break;
                default:
                    ASSERT(0);
                    break;
            }
        }
    }

    mObjThreshold = max(mObjThreshold, mObjCount * 2);
}
