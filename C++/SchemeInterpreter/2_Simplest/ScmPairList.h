#ifndef SCMPAIRLIST_H
#define SCMPAIRLIST_H

struct ScmObject;
class ScmObjectManager;

struct ScmPairList {
    static void drop(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
    static void append(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
    static void length(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
    static void last(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
    static void toVectorList(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
};

#endif
