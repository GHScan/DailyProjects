#ifndef SCMNUMERIC_H
#define SCMNUMERIC_H

struct ScmObject;
class ScmObjectManager;

struct ScmNumeric {
    static void add(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
    static void sub(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
    static void mul(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
    static void div(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
    static void quotient(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
    static void remainder(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
    static void equal(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
    static void less(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
    static void lessEqual(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
    static void greater(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
    static void greaterEqual(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
    static void sqr(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
    static void sqrt(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
    static void _not(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd);
};

#endif
