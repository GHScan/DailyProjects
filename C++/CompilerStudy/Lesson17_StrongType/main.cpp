
#include "pch.h"

#include <time.h>

#include "AST.h"
#include "Runtime.h"
#include "SymbolTable.h"
#include "TypeSystem.h"

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
template<>
struct CType2ScriptType<int>
{
    static IType* get(TypeSystem* ts)
    {
        return ts->getType("int");
    }
};

template<typename RetT>
void registerFunctionSymbol(const string& name, RetT(*f)())
{
    TypeSystem* ts = TypeSystem::instance();
    IType *retT = CType2ScriptType<RetT>::get(ts);
    vector<IType*> argT = {};
    SymbolTableManager::instance()->global()->addSymbol(name, ts->getFunc(retT, argT));
    GlobalEnvironment::instance()->registerFunction(name, FunctionPtr(
                new CFunction0<RetT>(f)));
}
template<typename RetT, typename ArgT0>
void registerFunctionSymbol(const string& name, RetT(*f)(ArgT0))
{
    TypeSystem* ts = TypeSystem::instance();
    IType *retT = CType2ScriptType<RetT>::get(ts);
    vector<IType*> argT = {CType2ScriptType<ArgT0>::get(ts)};
    SymbolTableManager::instance()->global()->addSymbol(name, ts->getFunc(retT, argT));
    GlobalEnvironment::instance()->registerFunction(name, FunctionPtr(
                new CFunction1<RetT, ArgT0>(f)));
}
template<typename RetT, typename ArgT0, typename ArgT1>
void registerFunctionSymbol(const string& name, RetT(*f)(ArgT0, ArgT1))
{
    TypeSystem* ts = TypeSystem::instance();
    IType *retT = CType2ScriptType<RetT>::get(ts);
    vector<IType*> argT = {CType2ScriptType<ArgT0>::get(ts), CType2ScriptType<ArgT1>::get(ts)};
    SymbolTableManager::instance()->global()->addSymbol(name, ts->getFunc(retT, argT));
    GlobalEnvironment::instance()->registerFunction(name, FunctionPtr(
                new CFunction2<RetT, ArgT0, ArgT1>(f)));
}

static int buildin_clock()
{
    return clock();
}
static int buildin_assert(int b)
{
    assert(b);
}
static int buildin_print(int i)
{
}
static int buildin_println(int i)
{
}

static void registerBuildin()
{
    registerFunctionSymbol("clock", &buildin_clock);
    registerFunctionSymbol("assert", &buildin_assert);
    registerFunctionSymbol("print", &buildin_print);
    registerFunctionSymbol("println", &buildin_println);
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
        // run();
    }
    catch (const exception& e) {
        cout << "Exception : " << e.what() << endl;
    }
}
