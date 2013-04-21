
#include "pch.h"

#include "LuaLibs.h"

static void buildin_print(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    for (auto &arg : args) {
        printf("%s\t", arg.toString().c_str());
    }
    puts("");
}
static void buildin_type(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    switch (args[0].getType()) {
        case LVT_Nil: rets.push_back(LuaValue("nil")); break;
        case LVT_Boolean: rets.push_back(LuaValue("boolean")); break;
        case LVT_Number: rets.push_back(LuaValue("number")); break;
        case LVT_String: rets.push_back(LuaValue("string")); break;
        case LVT_Table: rets.push_back(LuaValue("table")); break;
        case LVT_Function: rets.push_back(LuaValue("function")); break;
        case LVT_LightUserData: rets.push_back(LuaValue("lightuserdata")); break;
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
        for (const char *str = v.getString()->buf(); *str; ++str) {
            n *= base;
            n += char2Int(*str);
        }
        rets.push_back(LuaValue(n));
    } else rets.push_back(LuaValue::NIL);
}
static void buildin_tostring(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    rets.push_back(LuaValue(args[0].toString().c_str()));
}
static void buildin_assert(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    if (args[0].getBoolean()) {
        rets.assign(args.begin(), args.end());
    } else {
        if (args.size() > 1) ASSERT1(0, args[1].toString());
        else ASSERT1(0, "assertion failed!");
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
    rets.push_back(LuaVM::instance()->getGlobalTable()->get(LuaValue("_inext")));
    rets.push_back(args[0]);
}
static void buildin_pairs(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    rets.push_back(LuaVM::instance()->getGlobalTable()->get(LuaValue("next")));
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
static void buildin_loadstring(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    FILE *f = tmpfile();
    try {
        fprintf(f, "%s", args[0].getString()->buf());
        rewind(f);
        rets.push_back(loadFile(f));
        fclose(f);
    } catch(const exception& e)  {
        fclose(f);
        rets.push_back(LuaValue::NIL);
        rets.push_back(LuaValue(e.what()));
    }
}
static void buildin_loadfile(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    try {
        rets.push_back(loadFile(args[0].getString()->buf()));
    } catch(const exception& e) {
        rets.push_back(LuaValue::NIL);
        rets.push_back(LuaValue(e.what()));
    }
}
static void buildin_dofile(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    callFunc(loadFile(args[0].getString()->buf()), vector<LuaValue>(), rets);
}
// TODO:
static void buildin_getfenv(const vector<LuaValue>& args, vector<LuaValue>& rets) {
//    IFunction *func = NULL;
//    if (args.empty()) func = Runtime::instance()->getFrame(-2)->getFunc();
//    else if (args[0].isTypeOf(LVT_Number)) {
//        auto n = (int)args[0].getNumber();
//        ASSERT(n >= 0);
//        if (n > 0) func = Runtime::instance()->getFrame(-n - 1)->getFunc();
//    } else func = args[0].getFunction();
//
//    LuaTable* table = NULL;
//    if (func == NULL) table = LuaVM::instance()->getGlobalTable();
//    else table = func->getfenv();
//    table->addRef();
//    rets.push_back(LuaValue(table));
}
static void buildin_setfenv(const vector<LuaValue>& args, vector<LuaValue>& rets) {
//    IFunction *func = NULL;
//    if (args[0].isTypeOf(LVT_Number)) {
//        auto n = (int)args[0].getNumber();
//        ASSERT(n >= 0);
//        if (n > 0) func = Runtime::instance()->getFrame(-n - 1)->getFunc();
//    } else func = args[0].getFunction();
//
//    if (func == NULL) {
//        LuaVM::instance()->setGlobalTable(args[1].getTable());
//    } else {
//        func->setfenv(args[1].getTable());
//    }
}
// TODO:
static void buildin_setmetatable(const vector<LuaValue>& args, vector<LuaValue>& rets) {
//    args[0].getTable()->setMetaTable(args[1].getTable());
}
static void buildin_getmetatable(const vector<LuaValue>& args, vector<LuaValue>& rets) {
//    auto table = args[0].getTable()->getMetaTable();
//    if (table == NULL) rets.push_back(LuaValue::NIL);
//    else {
//        table->addRef();
//        rets.push_back(LuaValue(table));
//    }
}
static void buildin_rawequal(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto &a1(args[0]), &a2(args[1]);
    bool b = false;
    if (a1.isTypeOf(LVT_Table) && a2.isTypeOf(LVT_Table)) {
        b = a1.getTable() == a2.getTable();
    } else b = a1 == a2;
    rets.push_back(b ? LuaValue::TRUE : LuaValue::FALSE);
}
static void buildin_rawget(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    rets.push_back(args[0].getTable()->get(args[1], true));
}
static void buildin_rawset(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    args[0].getTable()->set(args[1], args[2], true);
}
static void buildin_pcall(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    try {
        vector<LuaValue> _args(args.begin() + 1, args.end());
        callFunc(args[0], _args, rets);
        rets.insert(rets.begin(), LuaValue::TRUE);
    } catch(const exception& e) {
        rets.push_back(LuaValue::FALSE);
        rets.push_back(LuaValue(e.what()));
    }
}
static void buildin_collectgarbage(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto mgr = LuaVM::instance()->getGCObjManager();
    int oldObjCount = mgr->getObjCount();
    mgr->performFullGC();
    int newObjCount = mgr->getObjCount();
    rets.push_back(LuaValue(oldObjCount));
    rets.push_back(LuaValue(newObjCount));
}

extern void openLib_buildin() {
#define ENTRY(name) {#name, &buildin_##name}
    CFuncEntry entries[] = {
        ENTRY(print), ENTRY(type), ENTRY(tonumber),
        ENTRY(tostring), ENTRY(assert), ENTRY(next),
        ENTRY(_inext), ENTRY(pairs), ENTRY(ipairs),
        ENTRY(unpack), ENTRY(select), ENTRY(loadstring),
        ENTRY(loadfile), ENTRY(dofile), ENTRY(getfenv),
        ENTRY(setfenv), ENTRY(getmetatable), ENTRY(setmetatable),
        ENTRY(rawequal), ENTRY(rawget), ENTRY(rawset),
        ENTRY(pcall), ENTRY(collectgarbage),
    };
#undef ENTRY
    for (auto &entry : entries) {
        LuaVM::instance()->getGlobalTable()->set(LuaValue(entry.name), LuaValue(CFunction::create(entry.func)));
    }

    LuaVM::instance()->getGlobalTable()->set(LuaValue("_G"), LuaValue(LuaVM::instance()->getGlobalTable()));
    LuaVM::instance()->getGlobalTable()->set(LuaValue("_VERSION"), LuaValue("Lua 5.1 (by Scan)"));
}
