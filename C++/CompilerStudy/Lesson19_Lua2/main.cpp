
#include "pch.h"

#include "LuaVM.h"
#include "GCObject.h"
#include "LuaValue.h"
#include "LuaTable.h"
#include "LuaFunction.h"

bool parseFile(const char *fname);

void buildin_print(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    for (auto &arg : args) cout << arg.toString() << '\t';
    cout << endl;
}
void buildin_collectgarbage(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto mgr = LuaVM::instance()->getGCObjManager();
    cout << "before gc: " << mgr->getObjCount() << endl;
    mgr->performFullGC();
    cout << "after gc: " << mgr->getObjCount() << endl;
}

static void registerFunction() {
    LuaVM::instance()->getGlobalTable()->set(LuaValue("print"), LuaValue(CFunction::create(&buildin_print)));
    LuaVM::instance()->getGlobalTable()->set(LuaValue("collectgarbage"), LuaValue(CFunction::create(&buildin_collectgarbage)));
}

int main(int argc, char *argv[])
{
    LuaVM::create();
    registerFunction();
    parseFile("test.lua");
    LuaVM::destroy();

    return 0;
}
