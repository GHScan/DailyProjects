
#include "pch.h"

#include <time.h>

#include "AST.h"
#include "Runtime.h"
#include "SymbolTable.h"
#include "TypeSystem.h"
#include "ByteCode.h"

template<typename T>
struct CType2ScriptType
{
    static IType* get(TypeSystem* ts)
    {
        return NULL;
    }
};
template<typename T>
struct CType2ScriptType<T*>
{
    static IType* get(TypeSystem* ts)
    {
        return ts->getPointer(CType2ScriptType<T>::get(ts));
    }
};
template<typename T>
struct CType2ScriptType<T const>
{
    static IType* get(TypeSystem* ts)
    {
        return CType2ScriptType<T>::get(ts);
    }
};
template<>
struct CType2ScriptType<int>
{
    static IType* get(TypeSystem* ts)
    {
        return ts->getType("int");
    }
};
template<>
struct CType2ScriptType<char>
{
    static IType* get(TypeSystem* ts)
    {
        return ts->getType("char");
    }
};

template<typename RetT>
void registerFunction(const string& name, RetT(*f)())
{
    TypeSystem* ts = TypeSystem::instance();
    IType *retT = CType2ScriptType<RetT>::get(ts);
    vector<IType*> argT = {};
    IType *ftype = ts->getFunc(retT, argT);
    SymbolTableManager::instance()->global()->addSymbol(name, ftype);
    CodeManager::instance()->registerFunction(name, FunctionPtr(new CFunction0<RetT>(f)));
}
template<typename RetT, typename ArgT0>
void registerFunction(const string& name, RetT(*f)(ArgT0))
{
    TypeSystem* ts = TypeSystem::instance();
    IType *retT = CType2ScriptType<RetT>::get(ts);
    vector<IType*> argT = {CType2ScriptType<ArgT0>::get(ts)};
    IType *ftype = ts->getFunc(retT, argT);
    SymbolTableManager::instance()->global()->addSymbol(name, ftype);
    CodeManager::instance()->registerFunction(name, FunctionPtr(new CFunction1<RetT, ArgT0>(f)));
}
template<typename RetT, typename ArgT0, typename ArgT1>
void registerFunction(const string& name, RetT(*f)(ArgT0, ArgT1))
{
    TypeSystem* ts = TypeSystem::instance();
    IType *retT = CType2ScriptType<RetT>::get(ts);
    vector<IType*> argT = {CType2ScriptType<ArgT0>::get(ts), CType2ScriptType<ArgT1>::get(ts)};
    IType *ftype = ts->getFunc(retT, argT);
    SymbolTableManager::instance()->global()->addSymbol(name, ftype);
    CodeManager::instance()->registerFunction(name, FunctionPtr(new CFunction2<RetT, ArgT0, ArgT1>(f)));
}
void registerVarLengFunction(const string& name, CVarlengFunction::FuncT f)
{
    TypeSystem* ts = TypeSystem::instance();
    IType *retT = CType2ScriptType<int>::get(ts);
    vector<IType*> argT = {CType2ScriptType<const char*>::get(ts)};
    IType *ftype = ts->getFunc(retT, argT);
    dynamic_cast<FunctionType*>(ftype)->isVarLengOfArg = true;
    SymbolTableManager::instance()->global()->addSymbol(name, ftype);
    CodeManager::instance()->registerFunction(name, FunctionPtr(new CVarlengFunction(f)));
}

template<typename RetT>
RetT callFromC(IFunction *func, RuntimeEnv* env)
{
    TypeSystem* ts = TypeSystem::instance();
    IType *retT = CType2ScriptType<RetT>::get(ts);
    vector<IType*> argT = {};
    IType *ftype = ts->getFunc(retT, argT);

    int retSize = retT->getSize();
    int retArgSize = retSize;
    env->pushValue(RetT());

    env->pushFrame(retArgSize);
    func->call(env);
    env->popFrame(retSize);

    RetT r;
    env->popValue(r);
    return r;
}

static int buildin_clock()
{
    return clock();
}
static int buildin_assert(int b)
{
    assert(b);
}
static void* buildin_malloc(int n)
{
    return malloc(n);
}
static int buildin_free(void *p)
{
    free(p);
}

static int buildin_printf(const char *fmt, char *args)
{
    while (*fmt) {
        string buf;
        if (*fmt == '%') {
            buf.push_back(*fmt);
            while (strchr("cdxs%", *++fmt) == NULL) buf.push_back(*fmt);
            buf.push_back(*fmt);
            switch (*fmt++) {
                case 'c':
                    printf(buf.c_str(), *((char*&)args)++);
                    break;
                case 'd': case 'x':
                    printf(buf.c_str(), *((int*&)args)++);
                    break;
                case 's':
                    printf(buf.c_str(), *((char**&)args)++);
                    break;
                case '%':
                    putchar('%');
                    break;
                default:
                    break;
            }
        }
        else {
            while (*fmt && *fmt != '%') buf.push_back(*fmt++);
            printf("%s", buf.c_str());
        }
    }
}

static void registerBuildin()
{
    registerFunction("clock", &buildin_clock);
    registerFunction("assert", &buildin_assert);
    registerFunction("malloc", &buildin_malloc);
    registerFunction("free", &buildin_free);
    registerVarLengFunction("printf", &buildin_printf);
}
static void genDisassemble(const string& fname)
{
    ofstream fo(fname.c_str());

    fo << "_global:\n";
    CodeManager::instance()->getFuncPreMain()->getCodeSeq()->disassemble(fo);

    for (auto name : CodeManager::instance()->getFuncNames()) {
        if (auto p = dynamic_cast<ASTFunction*>(CodeManager::instance()->getFunc(name).get())) {
            fo << name << ":\n";
            p->getCodeSeq()->disassemble(fo);
        }
    }
}
static void runMain()
{
    RuntimeEnv env;
    env.reserveGlobal(SymbolTableManager::instance()->global()->getOffset());
    CodeManager::instance()->emitAll();
    genDisassemble("disall.txt");
    callFromC<int>(CodeManager::instance()->getFuncPreMain(), &env);
    callFromC<int>(CodeManager::instance()->getFunc("main").get(), &env);
}

void parseFile(const char *fname);

int main(int argc, char *argv[])
{
    if (argc == 1) {
        puts("usage : interpreter file1 [file2 ...]");
        return 0;
    }

    try
    {
        registerBuildin();
        for (int i = 1; i < argc; ++i) parseFile(argv[i]);
        runMain();
    }
    catch (const exception& e) {
        cout << "Exception : " << e.what() << endl;
    }
}
