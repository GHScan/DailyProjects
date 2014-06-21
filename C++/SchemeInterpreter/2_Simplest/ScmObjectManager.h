#ifndef SCMOBJECTMANAGER_H
#define SCMOBJECTMANAGER_H

#include "ScmObject.h"

struct GCContext;
struct ScmSymbol;

class ScmObjectManager {
public:
    ScmObjectManager();
    ~ScmObjectManager();

    ScmSymbol* getSymbol(const string &s);

    template<typename ObjT, typename SlotT, typename ...ArgT>
    ObjT* create(SlotT**slot, ArgT&& ...args) {

        if (mObjCount >= mObjThreshold) {
            performFullGC();
        }

        auto obj = new ObjT(forward<ArgT>(args)...);
        obj->next = mFirstObj;
        mFirstObj = obj;

        slot[0] = obj;
        return obj;
    }

    ScmObjectManager(const ScmObjectManager&) = delete;
    ScmObjectManager& operator = (const ScmObjectManager&) = delete;
public:
    void setRootCollector(function<void(ScmObjectManager*)> rootCollector);

    void mark(ScmObject *obj);
    void performFullGC();

private:
    ScmObject *mFirstObj;
    function<void(ScmObjectManager *mgr)> mRootCollector;
    unordered_map<string, ScmSymbol*> mSymbols;
    int mObjCount;
    int mObjThreshold;
    queue<ScmObject*> mMarkedObjs;
};

#endif
