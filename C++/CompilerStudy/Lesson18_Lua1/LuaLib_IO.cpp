
#include "pch.h"

#include "LuaLibs.h"
#include "LuaValue.h"
#include "LuaTable.h"
#include "Runtime.h"
#include "Function.h"

static void io_close(const vector<LuaValue>& args, vector<LuaValue>& rets) {
}

extern void openLib_io() {
#define ENTRY(name) {#name, &io_##name}
    CFuncEntry entries[] = {
        // TODO
        ENTRY(close),
        // ENTRY(flush),
        // ENTRY(input),
        // ENTRY(lines),
        // ENTRY(open),
        // ENTRY(output),
        // ENTRY(popen),
        // ENTRY(read),
        // ENTRY(stderr),
        // ENTRY(stdin),
        // ENTRY(stdout),
        // ENTRY(tmpfile),
        // ENTRY(type),
        // ENTRY(write),
    };
#undef ENTRY
    auto table = LuaTable::create();
    Runtime::instance()->getGlobalTable()->set(LuaValue(string("io")), LuaValue(table));
    for (auto &entry : entries) {
        table->set(LuaValue(entry.name), LuaValue(CFunction::create(entry.func)));
    }
}
