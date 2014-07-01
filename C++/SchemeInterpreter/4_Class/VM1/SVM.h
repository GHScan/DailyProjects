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
class SEvalStack;

struct StackFrame {
    SEnv *localEnv;
    SFunc *func;
    int pc;
    int retOff;
};

class SVM {
public:
    SVM(FILE *file, bool disasm);
    ~SVM();

    SVM(const SVM&);
    SVM& operator = (const SVM&);

    void run();

private:
    SAssembler *mAssembler;
    SObjectManager *mObjMgr;
    SEvalStack *mEvalStack;
    vector<StackFrame> mFrameStack;
    vector<SValue> mConstants;
    vector<SValue> mGlobals;
    vector<SFuncProto*> mFuncProtos;
    vector<SClassProto*> mClassProtos;
    StackAllocator *mProtoAlloc;
    SFuncProto *mMainFuncProto;
};

#endif
