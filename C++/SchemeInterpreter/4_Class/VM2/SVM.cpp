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

        auto v = n->value.getNode()->ref(1);
        if (auto p = v.getInt()) {
            constants.push_back(SValue((PtrValue)strtod(p->c_str(), nullptr)));
        } else if (auto p = v.getFloat()) {
            constants.push_back(SValue((PtrValue)strtod(p->c_str(), nullptr)));
        } else if (auto p = v.getSymbol()) {
            constants.push_back(SValue::EMPTY);
            constants.back().setSymbol(p);
        } else if (auto p = v.getString()) {
            constants.push_back(SValue::EMPTY);
            constants.back().setString(p);
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
        {"+", [](SObjectManager *mgr, SValue *ret){ ret[0] = SValue(ret[1].getNumber() + ret[2].getNumber()); }},
        {"-", [](SObjectManager *mgr, SValue *ret){ ret[0] = SValue(ret[1].getNumber() - ret[2].getNumber()); }},
        {"*", [](SObjectManager *mgr, SValue *ret){ ret[0] = SValue(ret[1].getNumber() * ret[2].getNumber()); }},
        {"/", [](SObjectManager *mgr, SValue *ret){ ret[0] = SValue(ret[1].getNumber() / ret[2].getNumber()); }},
        {"quotient", [](SObjectManager *mgr, SValue *ret){ ret[0] = SValue(ret[1].getNumber() / ret[2].getNumber()); }},
        {"remainder", [](SObjectManager *mgr, SValue *ret){ ret[0] = SValue(ret[1].getNumber() % ret[2].getNumber()); }},
        {"=", [](SObjectManager *mgr, SValue *ret){ ret[0] = ret[1].getNumber() == ret[2].getNumber() ? SValue::TRUE : SValue::FALSE; }},
        {"<", [](SObjectManager *mgr, SValue *ret){ ret[0] = ret[1].getNumber() < ret[2].getNumber() ? SValue::TRUE : SValue::FALSE;}},
        {"<=", [](SObjectManager *mgr, SValue *ret){ ret[0] = ret[1].getNumber() <= ret[2].getNumber() ? SValue::TRUE : SValue::FALSE;}},
        {">", [](SObjectManager *mgr, SValue *ret){ ret[0] = ret[1].getNumber() > ret[2].getNumber() ? SValue::TRUE : SValue::FALSE;}},
        {">=", [](SObjectManager *mgr, SValue *ret){ ret[0] = ret[1].getNumber() >= ret[2].getNumber() ? SValue::TRUE : SValue::FALSE;}},
        {"sqr", [](SObjectManager *mgr, SValue *ret){ ret[0] = SValue(ret[1].getNumber() * ret[1].getNumber());}},
        {"sqrt", [](SObjectManager *mgr, SValue *ret){ ret[0] = SValue((PtrValue)sqrt(ret[1].getNumber()));}},
        {"identity", [](SObjectManager *mgr, SValue *ret){ ret[0] = ret[1]; }},

        {"not", [](SObjectManager *mgr, SValue *ret){ ret[0] = ret[1] == SValue::FALSE ? SValue::TRUE : SValue::FALSE; }},
        {"eq?", [](SObjectManager *mgr, SValue *ret){ ret[0] = ret[1] == ret[2] ? SValue::TRUE : SValue::FALSE; }},
        {"equal?", [](SObjectManager *mgr, SValue *ret){ ret[0] = ret[1].equal(ret[2]) ? SValue::TRUE : SValue::FALSE; }},
        {"empty?", [](SObjectManager *mgr, SValue *ret){ ret[0] = ret[1] == SValue::EMPTY ? SValue::TRUE : SValue::FALSE; }},

        {"cons", [](SObjectManager *mgr, SValue *ret){ ret[0] = SValue(mgr->createObject<SPair>(ScopedValue<SValue>(ret[1]), ScopedValue<SValue>(ret[2]))); }},
        {"car", [](SObjectManager *mgr, SValue *ret){ ret[0] = ret[1].getObject<SPair>()->getCar(); }},
        {"cdr", [](SObjectManager *mgr, SValue *ret){ ret[0] = ret[1].getObject<SPair>()->getCdr(); }},
        {"drop", [](SObjectManager *mgr, SValue *ret){
            SPair *p = ret[1].getObject<SPair>();
            int n = ret[2].getNumber();
            for (; n > 0; --n) p = p->getCdr().getObject<SPair>();
            *ret = SValue(p);
         }},
        {"length", [](SObjectManager *mgr, SValue *ret){
            SValue l = ret[1];
            int n = 0;
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
                ScopedValue<SObject*> firstList(ret[1].getObject());
                ret[0] = SValue(mgr->createObject<SPair>(ScopedValue<SValue>(static_cast<SPair*>(firstList.value)->getCar())));
                ScopedValue<SObject*> lastPair(ret[0].getObject());

                while (static_cast<SPair*>(firstList.value)->getCdr() != SValue::EMPTY) {
                    auto nextPair = static_cast<SPair*>(firstList.value)->getCdr().getObject<SPair>();
                    firstList.value = nextPair;
                    auto newPair = mgr->createObject<SPair>(ScopedValue<SValue>(nextPair->getCar()));
                    static_cast<SPair*>(lastPair.value)->setCdr(SValue(newPair));
                    lastPair.value = newPair;
                }

                static_cast<SPair*>(lastPair.value)->setCdr(ret[2]);
            }
        }},

        {"pretty-print", [](SObjectManager *mgr, SValue *ret){
            ret[1].writeToStream(cout);
            cout << endl;
            ret[0] = SValue::VOID;
        }},
        {"current-inexact-milliseconds", [](SObjectManager *mgr, SValue *ret){
            *ret = SValue(PtrValue(clock() * 1000.0 / CLOCKS_PER_SEC));
        }},
        {"random", [](SObjectManager *mgr, SValue *ret){
            *ret = SValue((::rand() * RAND_MAX + ::rand()) % ret[1].getNumber());
        }},
    };
    for (auto kv : builtinFuncs) {
        auto iter = find(names.begin(), names.end(), AtomPool::instance()->intern(kv.first));
        if (iter != names.end()) {
            globals[iter - names.begin()] = SValue(mgr->createObject<SNativeFunc>(kv.second));
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
        fproto->codes = alloc->mallocArray<uint8_t>((int)codes.size());
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

        for (auto &frame : mFrameStack) {
            mgr->mark(&frame.localEnv);
            mgr->mark(&frame.func);
        }

        if (mEvalStack != nullptr) {
            for (auto &v : *mEvalStack) {
                mgr->mark(&v);
            }
        }

        for (auto &v : mConstants) mgr->mark(&v);
        for (auto &v : mGlobals) mgr->mark(&v);
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
    mEvalStack->push(SValue(mObjMgr->createObject<SFunc>(ScopedValue<SObject*>(nullptr), mMainFuncProto)));

    SInterpreter::call(0, mEvalStack, mFrameStack, mObjMgr, mConstants, mGlobals, mFuncProtos, mClassProtos);

    mEvalStack->top<-1>().writeToStream(cout);
    cout << endl;
    mEvalStack->pop();

    ASSERT(mEvalStack->empty() && mFrameStack.empty());
}
