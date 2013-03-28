
#include "pch.h"

#include "LuaLibs.h"
#include "Runtime.h"
#include "LuaValue.h"
#include "Function.h"
#include "LuaTable.h"

static void buildin_print(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    for (auto &arg : args) {
        printf("%s\t", arg.toString().c_str());
    }
    puts("");
}
static void buildin_type(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    switch (args[0].getType()) {
        case LVT_Nil: rets.push_back(LuaValue(string("nil"))); break;
        case LVT_Boolean: rets.push_back(LuaValue(string("boolean"))); break;
        case LVT_Number: rets.push_back(LuaValue(string("number"))); break;
        case LVT_String: rets.push_back(LuaValue(string("string"))); break;
        case LVT_Table: rets.push_back(LuaValue(string("table"))); break;
        case LVT_Function: rets.push_back(LuaValue(string("function"))); break;
        default: ASSERT(0); break;
    }
}
static int char2Int(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    else if (c >= 'a' && c <= 'z') return c - 'a' + 10;
    else if (c >= 'A' && c <= 'Z') return c - 'A' + 10;
    else ASSERT(0);
}
static void buildin_tonumber(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto &v = args[0];
    if (v.isTypeOf(LVT_Number)) rets.push_back(v);
    else if (v.isTypeOf(LVT_String)) {
        int base = 10;
        if (args.size() > 1) base = (int)args[1].getNumber();
        NumberType n = 0;
        for (const char *str = v.getString(); *str; ++str) {
            n *= base;
            n += char2Int(*str);
        }
        rets.push_back(LuaValue(n));
    } else rets.push_back(LuaValue::NIL);
}
static void buildin_tostring(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    rets.push_back(LuaValue(args[0].toString()));
}
static void buildin_assert(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    if (!args[0].getBoolean()) {
        if (args.size() > 1) ASSERT1(0, args[1].toString());
        else ASSERT(0);
    }
}
static void buildin_next(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto table = args[0].getTable();
    LuaValue k;
    if (args.size() > 1) k = args[1];
    LuaValue v(table->getNext(k));
    rets.push_back(k);
    if (!k.isTypeOf(LVT_Nil)) rets.push_back(v);
}
static void buildin__inext(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto table = args[0].getTable();
    LuaValue k;
    if (args.size() > 1) k = args[1];
    LuaValue v(table->getINext(k));
    rets.push_back(k);
    if (!k.isTypeOf(LVT_Nil)) rets.push_back(v);
}
static void buildin_ipairs(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    rets.push_back(Runtime::instance()->getGlobalTable()->get(LuaValue(string("_inext"))));
    rets.push_back(args[0]);
}
static void buildin_pairs(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    rets.push_back(Runtime::instance()->getGlobalTable()->get(LuaValue(string("next"))));
    rets.push_back(args[0]);
}
static void buildin_unpack(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto table = args[0].getTable();
    for (int i = 0; i < table->size(); ++i) {
        rets.push_back(table->get(LuaValue(NumberType(i + 1))));
    }
}
static void buildin_select(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto &index = args[0];
    if (index.isTypeOf(LVT_String) && index.toString() == "#") {
        rets.push_back(LuaValue(NumberType(args.size() - 1)));
    }
    else if (index.isTypeOf(LVT_Number)) {
        for (int i = (int)index.getNumber(); i < (int)args.size(); ++i) {
            rets.push_back(args[i]);
        }
    } else ASSERT(0);
}

extern void openLib_buildin() {
    string names[] = {
        "print",
        "type",
        "tonumber",
        "tostring",
        "assert",
        "next",
        "_inext",
        "pairs",
        "ipairs",
        "unpack",
        "select",
        // TODO: 
        //"load",
        //"loadstring",
        //"loadfile",
        //"dofile",
        //
        //"getfenv",
        //"getmetatable",
        //"rawequal",
        //"rawget",
        //"rawset",
        //"setfenv",
        //"setmetatable",
    };

    void (*funcs[])(const vector<LuaValue>& args, vector<LuaValue>& rets) = {
        &buildin_print,
        &buildin_type,
        &buildin_tonumber,
        &buildin_tostring,
        &buildin_assert,
        &buildin_next,
        &buildin__inext,
        &buildin_pairs,
        &buildin_ipairs,
        &buildin_unpack,
        &buildin_select,
    };
    for (int i = 0; i < COUNT_OF(names); ++i) {
        Runtime::instance()->getGlobalTable()->set(
                LuaValue(names[i]), 
                LuaValue(CFunction::create(funcs[i])));
    }

    Runtime::instance()->getGlobalTable()->addRef();
    Runtime::instance()->getGlobalTable()->set(LuaValue(string("_G")), LuaValue(Runtime::instance()->getGlobalTable()));

    Runtime::instance()->getGlobalTable()->set(LuaValue(string("_VERSION")), LuaValue(string("Lua 5.1 (by Scan)")));
}
