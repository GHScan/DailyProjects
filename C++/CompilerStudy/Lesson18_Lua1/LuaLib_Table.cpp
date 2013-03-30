
#include "pch.h"

#include "LuaLibs.h"
#include "LuaValue.h"
#include "LuaTable.h"
#include "Runtime.h"
#include "Function.h"

extern int index_LuaValue2Int(const LuaValue& v, int len);

static void table_concat(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    string r;
    auto table = args[0].getTable();
    string sep;
    if (args.size() >= 2) sep = args[1].getString();
    int i = 0, j = table->size() - 1;
    if (args.size() >= 3) i = index_LuaValue2Int(args[2], table->size());
    if (args.size() >= 4) j = index_LuaValue2Int(args[3], table->size());

    if (i <= j) {
        LuaValue v(table->get(LuaValue(NumberType(i + 1))));
        ASSERT(v.isTypeOf(LVT_String) || v.isTypeOf(LVT_Number));
        r += v.toString();
        ++i;
    }
    for (; i <= j; ++i) {
        LuaValue v(table->get(LuaValue(NumberType(i + 1))));
        ASSERT(v.isTypeOf(LVT_String) || v.isTypeOf(LVT_Number));
        r += sep;
        r += v.toString();
    }

    rets.push_back(LuaValue(r));
}
static void table_insert(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto table = args[0].getTable();
    if (args.size() == 2) {
        table->arrayInsert(table->size(), args[1]);
    } else if (args.size() == 3) {
        table->arrayInsert(index_LuaValue2Int(args[1], table->size()), args[2]);
    } else ASSERT(0);
}
static void table_maxn(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    rets.push_back(LuaValue(NumberType(args[0].getTable()->size())));
}
static void table_remove(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto table = args[0].getTable();
    int index = table->size() - 1;
    if (args.size() >= 2) index = index_LuaValue2Int(args[1], table->size());
    rets.push_back(table->arrayRemove(index));
}
static void table_sort(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto table = args[0].getTable();
    if (args.size() == 1) {
        table->sort();
    } else if (args.size() == 2) {
        table->sort(args[1]);
    } else ASSERT(0);
}
static void table_foreach(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto table = args[0].getTable();
    auto func = args[1].getFunction();
    vector<LuaValue> _args, _rets;
    LuaValue k;
    for (;;) {
        LuaValue v(table->getNext(k));
        if (k.isTypeOf(LVT_Nil)) break;
        _args.push_back(k); _args.push_back(v);
        func->call(_args, _rets);
        _args.clear(); _rets.clear();
    }
}

extern void openLib_table() {
#define ENTRY(name) {#name, &table_##name}
    CFuncEntry entries[] = {
        ENTRY(concat),
        ENTRY(insert),
        ENTRY(maxn),
        ENTRY(remove),
        ENTRY(sort),
        ENTRY(foreach),
    };
#undef ENTRY
    auto table = LuaTable::create();
    Runtime::instance()->getGlobalTable()->set(LuaValue(string("table")), LuaValue(table));
    for (auto &entry : entries) {
        table->set(LuaValue(entry.name), LuaValue(CFunction::create(entry.func)));
    }
}
