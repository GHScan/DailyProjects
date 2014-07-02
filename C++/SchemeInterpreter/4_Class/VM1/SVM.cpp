#include "pch.h"
#include "SVM.h"
#include "SProto.h"
#include "STypes.h"
#include "SObjectManager.h"
#include "StackAllocator.h"
#include "SExpression.h"
#include "Atom.h"
#include "SAssembler.h"
#include "SDisassembler.h"
#include "SInterpreter.h"
#include "SStack.h"

static void setupConstants(SExpression e, vector<SValue> &constants, SObjectManager *mgr) {
    for (auto n = e.getNode()->ref(1).getNode(); n != nullptr; n = n->next) {
        constants.push_back(SValue::EMPTY);

        auto v = n->value.getNode()->ref(1);
        if (auto p = v.getInt()) {
            mgr->createNumber(&constants.back(), strtod(p->c_str(), nullptr));
        } else if (auto p = v.getFloat()) {
            mgr->createNumber(&constants.back(), strtod(p->c_str(), nullptr));
        } else if (auto p = v.getSymbol()) {
            mgr->createSymbol(&constants.back(), p);
        } else if (auto p = v.getString()) {
            mgr->createString(&constants.back(), p);
        } else {
            ASSERT(0 && "invalid constants!");
        }
    }
}

static void setupGlobals(SExpression e, vector<SValue> &globals, SObjectManager *mgr) {
    vector<Atom*> names;

    for (auto n = e.getNode()->ref(1).getNode(); n != nullptr; n = n->next) {
        names.push_back(n->value.getNode()->ref(1).getSymbol());
    }

    globals.resize(names.size(), SValue::EMPTY);

    pair<const char*, SValue> builtinVars[] = {
        {"true", SValue::TRUE},
        {"false", SValue::FALSE},
        {"else", SValue::TRUE},
        {"empty", SValue::EMPTY},
    };
    for (auto kv : builtinVars) {
        auto iter = find(names.begin(), names.end(), AtomPool::instance()->intern(kv.first));
        if (iter != names.end()) {
            globals[iter - names.begin()] = kv.second;
        }
    }

    pair<const char*, NativeFuncT> builtinFuncs[] = {
        {"+", [](SObjectManager *mgr, SValue *ret){ mgr->createNumber(ret, ret[1].getNumber() + ret[2].getNumber()); }},
        {"-", [](SObjectManager *mgr, SValue *ret){ mgr->createNumber(ret, ret[1].getNumber() - ret[2].getNumber()); }},
        {"*", [](SObjectManager *mgr, SValue *ret){ mgr->createNumber(ret, ret[1].getNumber() * ret[2].getNumber()); }},
        {"/", [](SObjectManager *mgr, SValue *ret){ mgr->createNumber(ret, ret[1].getNumber() / ret[2].getNumber()); }},
        {"quotient", [](SObjectManager *mgr, SValue *ret){ mgr->createNumber(ret, floor(ret[1].getNumber() / ret[2].getNumber())); }},
        {"remainder", [](SObjectManager *mgr, SValue *ret){ mgr->createNumber(ret, fmod(ret[1].getNumber(), ret[2].getNumber())); }},
        {"=", [](SObjectManager *mgr, SValue *ret){ mgr->createBool(ret, ret[1].getNumber() == ret[2].getNumber()); }},
        {"<", [](SObjectManager *mgr, SValue *ret){ mgr->createBool(ret, ret[1].getNumber() < ret[2].getNumber()); }},
        {"<=", [](SObjectManager *mgr, SValue *ret){ mgr->createBool(ret, ret[1].getNumber() <= ret[2].getNumber()); }},
        {">", [](SObjectManager *mgr, SValue *ret){ mgr->createBool(ret, ret[1].getNumber() > ret[2].getNumber()); }},
        {">=", [](SObjectManager *mgr, SValue *ret){ mgr->createBool(ret, ret[1].getNumber() >= ret[2].getNumber()); }},
        {"sqr", [](SObjectManager *mgr, SValue *ret){ mgr->createNumber(ret, ret[1].getNumber() * ret[1].getNumber()); }},
        {"sqrt", [](SObjectManager *mgr, SValue *ret){ mgr->createNumber(ret, ::sqrt(ret[1].getNumber())); }},
        {"identity", [](SObjectManager *mgr, SValue *ret){ ret[0] = ret[1]; }},

        {"not", [](SObjectManager *mgr, SValue *ret){ mgr->createBool(ret, ret[1] == SValue::FALSE); }},
        {"eq?", [](SObjectManager *mgr, SValue *ret){ mgr->createBool(ret, ret[1] == ret[2]); }},
        {"equal?", [](SObjectManager *mgr, SValue *ret){ mgr->createBool(ret, ret[1].equal(ret[2])); }},
        {"empty?", [](SObjectManager *mgr, SValue *ret){ mgr->createBool(ret, ret[1] == SValue::EMPTY); }},

        {"cons", [](SObjectManager *mgr, SValue *ret){ mgr->createObject<SPair>(ret, ret[1], ret[2]); }},
        {"car", [](SObjectManager *mgr, SValue *ret){ ret[0] = ret[1].getObject<SPair>()->getCar(); }},
        {"cdr", [](SObjectManager *mgr, SValue *ret){ ret[0] = ret[1].getObject<SPair>()->getCdr(); }},
        {"drop", [](SObjectManager *mgr, SValue *ret){
            SPair *p = ret[1].getObject<SPair>();
            double n = ret[2].getNumber();
            for (; n > 0; --n) p = p->getCdr().getObject<SPair>();
            *ret = SValue(p);
         }},
        {"length", [](SObjectManager *mgr, SValue *ret){
            SValue l = ret[1];
            double n = 0;
            for (; l != SValue::EMPTY; l = l.getObject<SPair>()->getCdr(), ++n);
            *ret = SValue(n);
        }},
        {"last", [](SObjectManager *mgr, SValue *ret){
            SPair *p = ret[1].getObject<SPair>();
            for (; p->getCdr() != SValue::EMPTY; p = p->getCdr().getObject<SPair>());
            ret[0] = p->getCar();
         }},
        {"append", [](SObjectManager *mgr, SValue *ret){
            if (ret[1] == SValue::EMPTY) {
                ret[0] = ret[2];
            } else {
                auto srcPair = ret[1].getObject<SPair>();
                auto newPair = mgr->createObject<SPair>(ret, srcPair->getCar(), SValue::EMPTY);
                SValue v;
                while (srcPair->getCdr() != SValue::EMPTY) {
                    srcPair = srcPair->getCdr().getObject<SPair>();
                    auto p = mgr->createObject<SPair>(&v, srcPair->getCar(), SValue::EMPTY);
                    newPair->setCdr(v);
                    newPair = p;
                }
                newPair->setCdr(ret[2]);
            }
        }},

        {"pretty-print", [](SObjectManager *mgr, SValue *ret){
            ret[1].writeToStream(cout);
            cout << endl;
            ret[0] = SValue::VOID;
        }},
        {"current-inexact-milliseconds", [](SObjectManager *mgr, SValue *ret){
            *ret = SValue(clock() * 1000.0 / CLOCKS_PER_SEC);
        }},
        {"random", [](SObjectManager *mgr, SValue *ret){
            *ret = SValue(fmod(::rand() * RAND_MAX + ::rand(), ret[1].getNumber()));
        }},
    };
    for (auto kv : builtinFuncs) {
        auto iter = find(names.begin(), names.end(), AtomPool::instance()->intern(kv.first));
        if (iter != names.end()) {
            mgr->createObject<SNativeFunc>(&globals[iter - names.begin()], kv.second);
        }
    }
}

static void setupFuncs(SExpression e, vector<SFuncProto*> &protos, SAssembler *assembler, bool disasm, StackAllocator *alloc) {
    vector<uint8_t> codes;

    for (auto f = e.getNode()->ref(1).getNode(); f != nullptr; f = f->next) {
        codes.clear();

        assembler->assemble(codes, f->value.getNode()->ref(1));

        if (disasm) {
            cout << format("func %d:\n", (int)protos.size());
            SDisassembler::disassemble(cout, 1, &codes[0], (int)codes.size());
            cout << endl;
        }

        auto fproto = alloc->malloc<SFuncProto>();

        ASSERT(!codes.empty());
        fproto->evalStackSize = atoi(f->value.getNode()->ref(1).getNode()->ref(0).getNode()->ref(1).getInt()->c_str());
        fproto->codeSize = (int)codes.size();
        fproto->codes = alloc->mallocArray<uint8_t>(codes.size());
        memcpy(fproto->codes, &codes[0], codes.size());

        protos.push_back(fproto);
    }
}

static void setupClasses(SExpression e, vector<SClassProto*> &protos, vector<SFuncProto*> &fprotos, StackAllocator *alloc) {
    vector<SClassProto::FieldInfo> fields;
    vector<SClassProto::MethodInfo> methods;

    for (auto c = e.getNode()->ref(1).getNode(); c != nullptr; c = c->next) {
        fields.clear();
        methods.clear();

        auto content = c->value.getNode()->ref(1).getNode();
        for (auto field = content->ref(0).getNode()->ref(1).getNode(); field != nullptr; field = field->next) {
            fields.push_back(SClassProto::FieldInfo{field->value.getNode()->ref(1).getSymbol()});
        }
        for (auto methodName = content->ref(1).getNode()->ref(1).getNode(), methodIndex = content->ref(2).getNode()->ref(1).getNode();
                methodName != nullptr;
                methodName = methodName->next, methodIndex = methodIndex->next) {
            methods.push_back(SClassProto::MethodInfo{
                    methodName->value.getNode()->ref(1).getSymbol(), 
                    fprotos[atoi(methodIndex->value.getNode()->ref(1).getInt()->c_str())],
            });
        }

        auto cproto = alloc->malloc<SClassProto>();

        cproto->fieldCount = (int)fields.size();
        cproto->fieldInfos = nullptr;
        if (cproto->fieldCount > 0) {
            cproto->fieldInfos = alloc->mallocArray<SClassProto::FieldInfo>(cproto->fieldCount);
            memcpy(cproto->fieldInfos, &fields[0], cproto->fieldCount * sizeof(fields[0]));
        }

        cproto->methodCount = (int)methods.size();
        cproto->methodInfos = nullptr;
        if (cproto->methodCount > 0) {
            cproto->methodInfos = alloc->mallocArray<SClassProto::MethodInfo>(cproto->methodCount);
            memcpy(cproto->methodInfos, &methods[0], cproto->methodCount * sizeof(methods[0]));
        }

        protos.push_back(cproto);
    }
}

extern SExpression parseFile(FILE *file, StackAllocator *allocator);

SVM::SVM(FILE *file, bool disasm): 
    mAssembler(nullptr), mObjMgr(nullptr), mProtoAlloc(nullptr), mMainFuncProto(nullptr) {

    new AtomPool;

    mAssembler = new SAssembler(mConstants);

    mEvalStack = new SEvalStack(4 * 1024);

    mObjMgr = new SObjectManager();
    mObjMgr->installRootCollector([this](SObjectManager *mgr){
        int youngOff = 0;

        for (auto riter = mFrameStack.rbegin(); riter != mFrameStack.rend(); ++riter) {
            if (mgr->isOldGenForGC(riter->localEnv)) {
                youngOff = riter->retOff;
                break;
            }

            mgr->mark(riter->localEnv);
            mgr->mark(riter->func);
        }

        if (mEvalStack != nullptr) {
            for (auto iter = mEvalStack->begin() + youngOff; iter != mEvalStack->end(); ++iter) {
                mgr->mark(*iter);
            }
        }

        for (auto v : mConstants) mgr->mark(v);
        for (auto v : mGlobals) mgr->mark(v);
    });

    mProtoAlloc = new StackAllocator();

    StackAllocator parserAlloc;
    auto n = parseFile(file, &parserAlloc).getNode();

    setupConstants(n->ref(1), mConstants, mObjMgr);
    setupGlobals(n->ref(2), mGlobals, mObjMgr);
    setupFuncs(n->ref(4), mFuncProtos, mAssembler, disasm, mProtoAlloc);
    setupClasses(n->ref(3), mClassProtos, mFuncProtos, mProtoAlloc);

    mMainFuncProto = mFuncProtos[atoi(n->ref(0).getNode()->ref(1).getInt()->c_str())];
}

SVM::~SVM() {
    ASSERT(mEvalStack->empty() && mFrameStack.empty());
    mConstants.clear();
    mGlobals.clear();

    DELETE(mProtoAlloc);
    DELETE(mObjMgr);
    DELETE(mEvalStack);
    DELETE(mAssembler);

    delete AtomPool::instance();
}

void SVM::run() {
    mEvalStack->push(SValue::EMPTY);
    mObjMgr->createObject<SFunc>(&mEvalStack->top<-1>(), nullptr, mMainFuncProto);

    SInterpreter::call(0, mEvalStack, mFrameStack, mObjMgr, mConstants, mGlobals, mFuncProtos, mClassProtos);

    mEvalStack->top<-1>().writeToStream(cout);
    cout << endl;
    mEvalStack->pop();

    ASSERT(mEvalStack->empty() && mFrameStack.empty());
}
