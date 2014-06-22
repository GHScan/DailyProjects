#include "pch.h"
#include "ScmInterpreter.h"
#include "ScmObject.h"
#include "ScmTypes.h"
#include "ScmObjectManager.h"
#include "ScmVectorList.h"
#include "ScmPairList.h"
#include "ScmNumeric.h"

struct StackFrame {
    ScmEnv *localEnv;
    ScmScriptFunction *func;
};

class ScmInterpreterImpl {
public:
    ScmInterpreterImpl();
    ~ScmInterpreterImpl();
    void interpret(istream &si);
    void* eval(ScmEnv *env, ScmObject *exp);

private:
    ScmObject** push(ScmObject *obj = nullptr) {
        mEvalStack.push_back(obj);
        return &mEvalStack.back();
    }

    ScmObject* pop() {
        auto ret = mEvalStack.back();
        mEvalStack.pop_back();
        return ret;
    }

    void popn(int n) {
        mEvalStack.erase(mEvalStack.begin() + mEvalStack.size() - n, mEvalStack.end());
    }

    ScmObject*& top() {
        return mEvalStack.back();
    }

    ScmObject*& top(int backwardIdx) {
        ASSERT(backwardIdx < 0);
        return mEvalStack[(int)mEvalStack.size() + backwardIdx];
    }

    template<typename ScmT, typename ...ArgT>
    void defineBuiltin(const string &name, ArgT &&...args) {
        ScmSymbol *sym = mObjMgr->getSymbol(name);
        mObjMgr->create<ScmT>(&mGlobalEnv->dict->getMutableDict()[sym], forward<ArgT>(args)...);
    }
    void defineBuiltin(const string &name, ScmObject *obj) {
        ScmSymbol *sym = mObjMgr->getSymbol(name);
        mGlobalEnv->dict->getMutableDict()[sym] = obj;
    }

    void setupGlobalEnv();

private:
    ScmObjectManager *mObjMgr;
    vector<StackFrame> mStackFrames;
    vector<ScmObject*> mEvalStack;
    ScmEnv *mGlobalEnv;
    ScmSymbol *mVoid;

private:
    ScmSymbol *IF;
    ScmSymbol *QUOTE;
    ScmSymbol *BEGIN;
    ScmSymbol *LAMBDA;
    ScmSymbol *DEFINE;
    ScmSymbol *SET;
};

ScmInterpreterImpl::ScmInterpreterImpl():
    mObjMgr(nullptr), mGlobalEnv(nullptr), mVoid(nullptr),
    IF(nullptr), QUOTE(nullptr), BEGIN(nullptr), LAMBDA(nullptr), DEFINE(nullptr), SET(nullptr) {

    mObjMgr = new ScmObjectManager();
    mObjMgr->setRootCollector([this](ScmObjectManager *mgr){
        if (mGlobalEnv != nullptr) mgr->mark(mGlobalEnv);
        if (IF != nullptr) mgr->mark(IF);
        if (QUOTE != nullptr) mgr->mark(QUOTE);
        if (BEGIN != nullptr) mgr->mark(BEGIN);
        if (LAMBDA != nullptr) mgr->mark(LAMBDA);
        if (DEFINE != nullptr) mgr->mark(DEFINE);
        if (SET != nullptr) mgr->mark(SET);
        for (auto v : mEvalStack) mgr->mark(v);
        for (auto &frame : mStackFrames) {
            mgr->mark(frame.localEnv);
            mgr->mark(frame.func);
        }
    });

    {
        auto dict = mObjMgr->create<ScmDictionary>(push());
        mObjMgr->create<ScmEnv>(&mGlobalEnv, nullptr, dict);
        pop();
    }

    mVoid = mObjMgr->getSymbol("____@@void#");

    IF = mObjMgr->getSymbol("if");
    QUOTE = mObjMgr->getSymbol("quote");
    BEGIN = mObjMgr->getSymbol("begin");
    LAMBDA = mObjMgr->getSymbol("lambda");
    DEFINE = mObjMgr->getSymbol("define");
    SET = mObjMgr->getSymbol("set!");

    setupGlobalEnv();
}

ScmInterpreterImpl::~ScmInterpreterImpl() {
    ASSERT(mStackFrames.empty() && mEvalStack.empty());

    mVoid = nullptr;
    mGlobalEnv = nullptr;

    IF = QUOTE = BEGIN = LAMBDA = DEFINE = SET = nullptr;

    delete mObjMgr;
}

void ScmInterpreterImpl::setupGlobalEnv() {
    defineBuiltin<ScmInt>("true", 1);
    defineBuiltin<ScmInt>("false", 0);
    defineBuiltin<ScmInt>("else", 1);
    defineBuiltin("empty", ScmObject::EMPTY);

    defineBuiltin<ScmCFunction>("+", &ScmNumeric::add);
    defineBuiltin<ScmCFunction>("-", &ScmNumeric::sub);
    defineBuiltin<ScmCFunction>("*", &ScmNumeric::mul);
    defineBuiltin<ScmCFunction>("/", &ScmNumeric::div);
    defineBuiltin<ScmCFunction>("quotient", &ScmNumeric::quotient);
    defineBuiltin<ScmCFunction>("remainder", &ScmNumeric::remainder);

    defineBuiltin<ScmCFunction>("=", &ScmNumeric::equal);
    defineBuiltin<ScmCFunction>("<", &ScmNumeric::less);
    defineBuiltin<ScmCFunction>("<=", &ScmNumeric::lessEqual);
    defineBuiltin<ScmCFunction>(">", &ScmNumeric::greater);
    defineBuiltin<ScmCFunction>(">=", &ScmNumeric::greaterEqual);
    defineBuiltin<ScmCFunction>("not", &ScmNumeric::_not);

    defineBuiltin<ScmCFunction>("sqr", &ScmNumeric::sqr);
    defineBuiltin<ScmCFunction>("sqrt", &ScmNumeric::sqrt);
    defineBuiltin<ScmCFunction>("identity",  [](ScmObjectManager* mgr, ScmObject **ret, ScmObject **argEnd){
        ret[0] = ret[1];
    });

    defineBuiltin<ScmCFunction>("eq?",  [](ScmObjectManager* mgr, ScmObject **ret, ScmObject **argEnd){
        mgr->create<ScmInt>(ret, ret[1] == ret[2]);
    });
    defineBuiltin<ScmCFunction>("equal?",  [](ScmObjectManager* mgr, ScmObject **ret, ScmObject **argEnd){
        mgr->create<ScmInt>(ret, ret[1]->equal(ret[2]));
    });

    defineBuiltin<ScmCFunction>("cons",  [](ScmObjectManager* mgr, ScmObject **ret, ScmObject **argEnd){
        mgr->create<ScmPair>(ret, ret[1], ret[2]);
    });
    defineBuiltin<ScmCFunction>("car",  [](ScmObjectManager* mgr, ScmObject **ret, ScmObject **argEnd){
        ret[0] = ret[1]->staticCast<ScmPair>()->car;
    });
    defineBuiltin<ScmCFunction>("cdr",  [](ScmObjectManager* mgr, ScmObject **ret, ScmObject **argEnd){
        ret[0] = ret[1]->staticCast<ScmPair>()->cdr;
    });
    defineBuiltin<ScmCFunction>("drop", &ScmPairList::drop);
    defineBuiltin<ScmCFunction>("append", &ScmPairList::append);
    defineBuiltin<ScmCFunction>("length", &ScmPairList::length);
    defineBuiltin<ScmCFunction>("last", &ScmPairList::last);
    defineBuiltin<ScmCFunction>("empty?",  [](ScmObjectManager* mgr, ScmObject **ret, ScmObject **argEnd){
        mgr->create<ScmInt>(ret, ret[1] == ScmObject::EMPTY);
    });

    defineBuiltin<ScmCFunction>("pretty-print",  [this](ScmObjectManager* mgr, ScmObject **ret, ScmObject **argEnd){
        ret[1]->writeToStream(cout);
        cout << endl;
        ret[0] = mVoid;
    });
    defineBuiltin<ScmCFunction>("current-inexact-milliseconds",  [](ScmObjectManager* mgr, ScmObject **ret, ScmObject **argEnd){
        mgr->create<ScmDouble>(ret, clock() * 1000.0 / CLOCKS_PER_SEC);
    });
    defineBuiltin<ScmCFunction>("random",  [](ScmObjectManager* mgr, ScmObject **ret, ScmObject **argEnd){
        mgr->create<ScmInt>(ret, ::rand() % ret[1]->staticCast<ScmInt>()->number);
    });
    defineBuiltin<ScmCFunction>("eval",  [this](ScmObjectManager* mgr, ScmObject **ret, ScmObject **argEnd){
        ScmPairList::toVectorList(mgr, ret, argEnd);
        ret[1] = ret[0];
        eval(mGlobalEnv, ret[1]);
        top(-3) = top();
        pop();
    });
}

void* ScmInterpreterImpl::eval(ScmEnv *env, ScmObject *exp) {
    if (auto sym = exp->dynamicCast<ScmSymbol>()) {
        return push(env->lookup(sym));
    } 

    auto vec = exp->dynamicCast<ScmVector>();
    if (vec == nullptr || vec->size() == 0) {
        return push(exp);
    }

    auto& form = vec->getImmutableVec();
    if (form[0]->equal(QUOTE)) {
        push();
        push(form[1]);
        auto ret = &top(-2);
        ScmVectorList::toPairList(mObjMgr, ret, ret + 2);
        pop();
        return ret;

    } else if (form[0]->equal(IF)) {
        eval(env, form[1]);
        if (pop()->staticCast<ScmInt>()->number) {
            return eval(env, form[2]);
        } else {
            return eval(env, form[3]);
        }

    } else if (form[0]->equal(BEGIN)) {
        for (int i = 1; i < (int)form.size() - 1; ++i) {
            eval(env, form[i]);
            pop();
        }
        return eval(env, form.back());

    } else if (form[0]->equal(LAMBDA)) {
        ASSERT(form.size() == 3);
        mObjMgr->create<ScmScriptFunction>(push(), env, form[1]->staticCast<ScmVector>(), form[2]);
        return 0;

    } else if (form[0]->equal(DEFINE)) {
        eval(env, form[2]);
        env->define(form[1]->staticCast<ScmSymbol>(), top());
        top() = mVoid;
        return 0;

    } else if (form[0]->equal(SET)) {
        eval(env, form[2]);
        env->set(form[1]->staticCast<ScmSymbol>(), top());
        top() = mVoid;
        return 0;

    } else {
        for (auto e : form) {
            eval(env, e);
        }
        auto func = &mEvalStack.back() - form.size() + 1;

        if (auto cfunc = func[0]->dynamicCast<ScmCFunction>()) {
            cfunc->func(mObjMgr, func, func + form.size());
            popn(form.size() - 1);
            return 0;
        } else {
            auto scriptFunc = func[0]->staticCast<ScmScriptFunction>();

            mStackFrames.push_back(StackFrame{nullptr, scriptFunc});
            auto &frame = mStackFrames.back();
            {
                auto dict = mObjMgr->create<ScmDictionary>(push());
                mObjMgr->create<ScmEnv>(&frame.localEnv, scriptFunc->env, dict);
                pop();
            }

            {
                auto &formals = scriptFunc->formals->getImmutableVec();
                ASSERT((formals.size() == form.size() - 1) && "Argument count mistmatch!");
                for (int i = (int)formals.size() - 1; i >= 0; --i) {
                    frame.localEnv->define(formals[i]->staticCast<ScmSymbol>(), pop());
                }
            }
            pop();

            eval(frame.localEnv, scriptFunc->body);

            mStackFrames.pop_back();
            return 0;
        }
    }
}

void ScmInterpreterImpl::interpret(istream &si) {
    ASSERT(mStackFrames.empty() && mEvalStack.empty());

    push();
    {
        string s;
        for (string line; getline(si, line); s += line + '\n');
        mObjMgr->create<ScmString>(push(), move(s));
    }
    ScmVectorList::parse(mObjMgr, &top(-2), &top(-2) + 2);
    pop();

    for (auto exp : top()->staticCast<ScmVector>()->getImmutableVec()) {
        eval(mGlobalEnv, exp);
        if (top() != mVoid) {
            top()->writeToStream(cout);
            cout << endl;
        }
        pop();
    }
    pop();

    ASSERT(mStackFrames.empty() && mEvalStack.empty());
}

//------------------------------
ScmInterpreter::ScmInterpreter():
    mImpl(new ScmInterpreterImpl()) {
}

ScmInterpreter::~ScmInterpreter() {
    delete mImpl;
}

void ScmInterpreter::interpret(istream &si) {
    mImpl->interpret(si);
}
