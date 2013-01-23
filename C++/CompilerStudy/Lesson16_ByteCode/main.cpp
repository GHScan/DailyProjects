
#include "pch.h"

#include <time.h>

#include "AST.h"
#include "Runtime.h"
#include "ByteCode.h"

ByteCodeFunction::~ByteCodeFunction()
{
    delete m_seq;
}
Value ByteCodeFunction::call(const vector<Value>& args)
{
    ASSERT(args.size() == m_argc);
    StackFrame frame(args);
    m_seq->run(&frame);
    return frame.retValue;
}

Value buildin_print(const vector<Value>& args)
{
    for (auto &v : args) cout << v.toString() << '\t';
    return Value();
}
Value buildin_println(const vector<Value>& args)
{
    buildin_print(args);
    cout << endl;
    return Value();
}
Value buildin_clock(const vector<Value>& args)
{
    return Value::createInt(clock());
}

void registerBuildin()
{
    GlobalEnvironment *g = GlobalEnvironment::instance();
    g->registerFunc("print", FunctionPtr(new CFunction(&buildin_print)));
    g->registerFunc("println", FunctionPtr(new CFunction(&buildin_println)));
    g->registerFunc("clock", FunctionPtr(new CFunction(&buildin_clock)));
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
        GlobalEnvironment::instance()->getFunc("main")->call(vector<Value>());
    }
    catch (const exception& e) {
        cout << "Exception : " << e.what() << endl;
    }
}
