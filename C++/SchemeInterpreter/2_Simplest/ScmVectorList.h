#ifndef SCMVECTORLIST_H
#define SCMVECTORLIST_H

struct ScmObject;
class ScmObjectManager;

struct ScmVectorList {
    static void parse(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
    static void expandLibraryForms(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
    static void toPairList(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
};

#endif
