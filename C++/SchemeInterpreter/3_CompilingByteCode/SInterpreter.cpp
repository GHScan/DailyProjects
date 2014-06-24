#include "pch.h"
#include "SInterpreter.h"
#include "SObjectManager.h"
#include "SStack.h"
#include "SValue.h"
#include "SScriptFunctionProto.h"
#include "SymbolTable.h"
#include "SParser.h"
#include "ASTCompiler.h"
#include "ByteCodeCompiler.h"
#include "ByteCodeExecuter.h"
#include "SNumeric.h"
#include "SPairList.h"
#include "ByteCodeDisassembler.h"

class SInterpreterImpl {
public:
    SInterpreterImpl();
    ~SInterpreterImpl();

    void interpret(istream &si);
private:
    void callScriptFunction(int actualCount);
    void eval();

    void setupBuiltin();

    void defineBuiltin(const char *name, CFunction f);
    void defineBuiltin(const char *name, SValue v);
    SValue* allocBuiltin(const char *name);

private:
    SObjectManager *mObjMgr;
    SEvalStack *mEvalStack;
    SFrameStack *mFrameStack;
    SymbolTable *mGSymTable;
    vector<SValue> mGlobals;
    vector<SValue> mLiterals;
    vector<SScriptFunctionProtoPtr> mProtos;
};

SInterpreterImpl::SInterpreterImpl(): 
    mObjMgr(nullptr), mEvalStack(nullptr), mFrameStack(nullptr), mGSymTable(nullptr) {

    mObjMgr = new SObjectManager(8 * 1024 * 1024, 1024);
    mFrameStack = new SFrameStack();
    mEvalStack = new SEvalStack(64 * 1024);
    mGSymTable = new SymbolTable(nullptr);

    mGlobals.reserve(1024);
    mLiterals.reserve(1024);
    mProtos.reserve(256);

    mObjMgr->installGCRootCollector([this](SObjectManager *mgr){
        if (mEvalStack != nullptr) {
            for (auto &v : *mEvalStack) {
                mgr->mark(&v);
            }
        }

        if (mFrameStack != nullptr) {
            for (auto &frame : *mFrameStack) {
                if (frame.localEnv) mgr->mark(&frame.localEnv);
                if (frame.func) mgr->mark(&frame.func);
            }
        }

        for (auto &v : mGlobals) {
            mgr->mark(&v);
        }
        for (auto &v : mLiterals) {
            mgr->mark(&v);
        }

    });

    setupBuiltin();

    mLiterals.push_back(SValue::VOID);
}

SInterpreterImpl::~SInterpreterImpl() {
    ASSERT(mEvalStack->size() == 0);
    ASSERT(mFrameStack->size() == 0);

    mLiterals.clear();
    mGlobals.clear();

    DELETE(mGSymTable);
    DELETE(mFrameStack);
    DELETE(mEvalStack);
    DELETE(mObjMgr);
}

SValue* SInterpreterImpl::allocBuiltin(const char *name) {
    auto address = mGSymTable->lookup(name);
    ASSERT(address.isGlobal());
    mGlobals.resize(max((int)mGlobals.size(), address.getVarIndex() + 1));
    return &mGlobals[address.getVarIndex()];
}

void SInterpreterImpl::defineBuiltin(const char *name, CFunction f) {
    allocBuiltin(name)->setExternalObject(mObjMgr->createCFunction(f));
}

void SInterpreterImpl::defineBuiltin(const char *name, SValue v) {
    *allocBuiltin(name) = v;
}

void SInterpreterImpl::setupBuiltin() {
    defineBuiltin("true", SValue::TRUE);
    defineBuiltin("false", SValue::FALSE);
    defineBuiltin("else", SValue::TRUE);
    defineBuiltin("empty", SValue::EMPTY);

    defineBuiltin("+", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-3) = SNumeric::add(mgr, stack->top(-2), stack->top(-1));
    });
    defineBuiltin("-", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-3) = SNumeric::sub(mgr, stack->top(-2), stack->top(-1));
    });
    defineBuiltin("*", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-3) = SNumeric::mul(mgr, stack->top(-2), stack->top(-1));
    });
    defineBuiltin("/", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-3) = SNumeric::div(mgr, stack->top(-2), stack->top(-1));
    });
    defineBuiltin("quotient", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-3) = SNumeric::quotient(mgr, stack->top(-2), stack->top(-1));
    });
    defineBuiltin("remainder", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-3) = SNumeric::remainder(mgr, stack->top(-2), stack->top(-1));
    });

    defineBuiltin("=", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-3) = SNumeric::equal(mgr, stack->top(-2), stack->top(-1));
    });
    defineBuiltin("<", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-3) = SNumeric::less(mgr, stack->top(-2), stack->top(-1));
    });
    defineBuiltin("<=", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-3) = SNumeric::lessEqual(mgr, stack->top(-2), stack->top(-1));
    });
    defineBuiltin(">", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-3) = SNumeric::greater(mgr, stack->top(-2), stack->top(-1));
    });
    defineBuiltin(">=", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-3) = SNumeric::greaterEqual(mgr, stack->top(-2), stack->top(-1));
    });

    defineBuiltin("not", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-2) = SNumeric::_not(mgr, stack->top(-1));
    });
    defineBuiltin("sqr", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-2) = SNumeric::sqr(mgr, stack->top(-1));
    });
    defineBuiltin("sqrt", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-2) = SNumeric::sqrt(mgr, stack->top(-1));
    });
    defineBuiltin("identity", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-2) = stack->top(-1);
    });

    defineBuiltin("eq?", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-3) = stack->top(-2) == stack->top(-1) ? SValue::TRUE : SValue::FALSE;
    });
    defineBuiltin("equal?", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-3) = stack->top(-2).equal(stack->top(-1)) ? SValue::TRUE : SValue::FALSE;
    });

    defineBuiltin("cons", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-3).setObject(mgr->createPair(ScopedValue<SValue>(stack->top(-2)), ScopedValue<SValue>(stack->top(-1))));
    });
    defineBuiltin("car", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-2) = stack->top(-1).getObject()->staticCast<SPair>()->car;
    });
    defineBuiltin("cdr", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-2) = stack->top(-1).getObject()->staticCast<SPair>()->cdr;
    });

    defineBuiltin("drop", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        SPairListAccessor list(stack->top(-2));
        auto iter = list.begin();
        advance(iter, stack->top(-1).getInt());
        stack->top(-3).setObject(iter.getPair());
    });
    defineBuiltin("append", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        SPairListAccessor firstList(stack->top(-2));
        SPairListBuilder retList(mgr);
        for (auto v : firstList) retList.push(v);
        retList.concat(stack->top(-1));
        stack->top(-3) = retList.getList();
    });
    defineBuiltin("length", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        SPairListAccessor list(stack->top(-1));
        stack->top(-2) = SValue((int)distance(list.begin(), list.end()));
    });
    defineBuiltin("last", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        SPairListAccessor list(stack->top(-1));
        auto iter = list.begin(), next = list.begin(), end = list.end();
        ++next;
        for (; next != end; iter = next, ++next);
        stack->top(-2) = iter.getPair()->car;
    });

    defineBuiltin("empty?", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-2) = stack->top(-1) == SValue::EMPTY ? SValue::TRUE : SValue::FALSE;
    });

    defineBuiltin("pretty-print", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-1).writeToStream(cout);
        cout << endl;
        stack->top(-2) = SValue::VOID;
    });
    defineBuiltin("current-inexact-milliseconds", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-1) = SValue(mgr->createDouble(clock() * 1000.0 / CLOCKS_PER_SEC));
    });
    defineBuiltin("random", [](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        stack->top(-2) = SValue(::rand() % stack->top(-1).getInt());
    });
    defineBuiltin("eval", [this](SObjectManager *mgr, SEvalStack *stack, int actualCount){
        eval();
        stack->top(-2) = stack->top(-1);
    });
}

void SInterpreterImpl::interpret(istream &si) {
    ASSERT(mEvalStack->size() == 0 && mFrameStack->size() == 0);

    string source;
    for (string line; getline(si, line); source += line + '\n');
    mEvalStack->push(parse(mObjMgr, source));

    while (mEvalStack->top(-1) != SValue::EMPTY) {
        SPair *l = mEvalStack->top(-1).getObject()->staticCast<SPair>();
        mEvalStack->push(l->car);
        mEvalStack->top(-2) = l->cdr;

        eval();

        if (mEvalStack->top(-1) != SValue::VOID) {
            mEvalStack->top(-1).writeToStream(cout);
            cout << endl;
        }

        mEvalStack->pop();
    }
    mEvalStack->pop();
}

void SInterpreterImpl::callScriptFunction(int actualCount) {
    SValue f = mEvalStack->top(-actualCount - 1);
    if (f.getType() == SCFunction::TYPE) {
        auto func = f.getExternalObject()->staticCast<SCFunction>();
        func->func(mObjMgr, mEvalStack, actualCount);
        mEvalStack->pop(actualCount);
    } else {
        executeByteCode(actualCount, mEvalStack, mFrameStack, mObjMgr, &mGlobals, &mLiterals, &mProtos);
    }
}

void SInterpreterImpl::eval() {
    ASTNodePtr body = compileToAST(mGSymTable, &mLiterals, mEvalStack->top(-1));
    mGlobals.resize(mGSymTable->getSymbolCount());

    ASTNode_Lambda lambda = {0, {}, body};
    compileToByteCode(nullptr, &lambda, &mProtos);

    // disassembleByteCode(cout, mProtos.back().get(), mGSymTable, mProtos, mLiterals);

    mEvalStack->top(-1).setObject(mObjMgr->createScriptFunction(mProtos.back().get(), ScopedValue<SObject*>(nullptr)));

    callScriptFunction(0);
}

SInterpreter::SInterpreter():
    mImpl(new SInterpreterImpl()) {
}

SInterpreter::~SInterpreter() {
    DELETE(mImpl);
}

void SInterpreter::interpret(istream &si) {
    mImpl->interpret(si);
}
