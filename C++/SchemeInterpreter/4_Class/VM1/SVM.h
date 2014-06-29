#ifndef SVM_H
#define SVM_H

#include "SValue.h"

struct SEnv;
struct SFunc;
struct SFuncProto;
struct SClassProto;
class SObjectManager;
class StackAllocator;
class SAssembler;

struct StackFrame {
    SEnv *localEnv;
    SFunc *func;
    int pc;
};

class SVM {
public:
    SVM(const char *fname);
    ~SVM();

    SVM(const SVM&);
    SVM& operator = (const SVM&);

    void run();

private:
    SAssembler *mAssembler;
    SObjectManager *mObjMgr;
    vector<SValue> mEvalStack;
    vector<StackFrame> mFrameStack;
    vector<SValue> mConstants;
    vector<SValue> mGlobals;
    vector<SFuncProto*> mFuncProtos;
    vector<SClassProto*> mClassProtos;
    StackAllocator *mProtoAlloc;
    SFuncProto *mMainFuncProto;
};

#endif
