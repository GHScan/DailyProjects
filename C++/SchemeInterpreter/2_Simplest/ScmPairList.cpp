#include "pch.h"
#include "ScmPairList.h"
#include "ScmObject.h"
#include "ScmTypes.h"
#include "ScmObjectManager.h"

void ScmPairList::drop(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    ScmPair *l = ret[1]->staticCast<ScmPair>();
    ScmInt *n = ret[2]->staticCast<ScmInt>();
    for (int i = 0; i < n->number; ++i) {
        l = l->cdr->staticCast<ScmPair>();
    }
    ret[0] = l;
}

void ScmPairList::append(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    static vector<ScmPair*> s_pairs;

    ScmPair *a = ret[1]->staticCast<ScmPair>();

    for (; a != ScmObject::EMPTY; a = a->cdr->staticCast<ScmPair>()) {
        s_pairs.push_back(a);
    }

    ret[0] = ret[2];
    for (int i = (int)s_pairs.size() - 1; i >= 0; --i) {
        mgr->create<ScmPair>(ret, s_pairs[i]->car, ret[0]);
    }

    s_pairs.clear();
}

void ScmPairList::length(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    ScmInt *n = mgr->create<ScmInt>(ret, 0);
    ScmPair *l = ret[1]->staticCast<ScmPair>();

    for (; l != ScmObject::EMPTY; l = l->cdr->staticCast<ScmPair>()) {
        ++n->number;
    }
}

void ScmPairList::last(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    ScmPair *l = ret[1]->staticCast<ScmPair>();

    while (l->cdr != ScmObject::EMPTY) {
        l = l->cdr->staticCast<ScmPair>();
    }

    ret[0] = l;
}
