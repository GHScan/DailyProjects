
#include "pch.h"

#include <time.h>

#include "LuaLibs.h"

static void os_clock(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    rets.push_back(LuaValue(NumberType(::clock()) / CLOCKS_PER_SEC));
}
static void os_date(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    const char *fmt = "%c";
    time_t time = ::time(NULL);
    if (args.size() >= 1) fmt = args[0].getString();
    if (args.size() >= 2) time = (time_t)args[1].getNumber();

    tm detail;
    if (*fmt == '!') {
        ++fmt;
        gmtime_r(&time, &detail);
    } else localtime_r(&time, &detail);

    if (strcmp(fmt, "*t") == 0) {
        auto table = LuaTable::create();
        table->set(LuaValue("year"), LuaValue(NumberType(detail.tm_year + 1900)));
        table->set(LuaValue("month"), LuaValue(NumberType(detail.tm_mon + 1)));
        table->set(LuaValue("day"), LuaValue(NumberType(detail.tm_mday)));
        table->set(LuaValue("hour"), LuaValue(NumberType(detail.tm_hour)));
        table->set(LuaValue("min"), LuaValue(NumberType(detail.tm_min)));
        table->set(LuaValue("sec"), LuaValue(NumberType(detail.tm_sec)));
        table->set(LuaValue("isdst"), detail.tm_isdst == 1 ? LuaValue::TRUE : LuaValue::FALSE);
        rets.push_back(LuaValue(table));
    } else {
        char buf[64] = "";
        strftime(buf, sizeof(buf), fmt, &detail);
        rets.push_back(LuaValue(buf));
    }
}
static void os_difftime(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    rets.push_back(args[1] - args[0]);
}
static void os_execute(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    const char *cmd = NULL;
    if (!args.empty()) cmd = args[0].getString();
    int status = ::system(cmd);
    rets.push_back(LuaValue(NumberType(status)));
}
static void os_exit(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    int code = 0;
    if (!args.empty()) code = (int)args[0].getNumber();
    ::exit(code);
}
static void os_getenv(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    rets.push_back(LuaValue(::getenv(args[0].getString())));
}
static void os_remove(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    const char *path = args[0].getString();
    int err = ::remove(path);
    if (err == 0) {
        rets.push_back(LuaValue::TRUE);
    } else {
        rets.push_back(LuaValue::NIL);
        rets.push_back(LuaValue("remove failed!"));
    }
}
static void os_rename(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    const char *oldPath = args[0].getString(), *newPath = args[1].getString();
    int err = ::rename(oldPath, newPath);
    if (err == 0) {
        rets.push_back(LuaValue::TRUE);
    } else {
        rets.push_back(LuaValue::NIL);
        rets.push_back(LuaValue("rename failed!"));
    }
}
static void os_setlocale(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    ASSERT(0);
}
static void os_time(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    if (args.empty()) {
        rets.push_back(LuaValue(NumberType(::time(NULL))));
    } else {
        auto table = args[0].getTable();
        tm detail = {0};
        detail.tm_year = (int)table->get(LuaValue("year")).getNumber() - 1900;
        detail.tm_mon = (int)table->get(LuaValue("month")).getNumber() - 1;
        detail.tm_mday = (int)table->get(LuaValue("day")).getNumber();
        LuaValue hour = table->get(LuaValue("hour"));
        if (!hour.isTypeOf(LVT_Nil)) {
            detail.tm_hour = (int)hour.getNumber();
        }
        LuaValue min = table->get(LuaValue("min"));
        if (!min.isTypeOf(LVT_Nil)) {
            detail.tm_min = (int)min.getNumber();
        }
        LuaValue sec = table->get(LuaValue("sec"));
        if (!sec.isTypeOf(LVT_Nil)) {
            detail.tm_sec = (int)sec.getNumber();
        }
        LuaValue isdst = table->get(LuaValue("isdst"));
        if (!sec.isTypeOf(LVT_Nil)) {
            detail.tm_isdst = sec.getBoolean() ? 1 : 0;
        }
        rets.push_back(LuaValue(NumberType(mktime(&detail))));
    }
}
static void os_tmpname(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    char *p = tmpnam(NULL);
    rets.push_back(LuaValue(p == NULL ? "" : p));
}

extern void openLib_os() {
    auto table = LuaTable::create();
    Runtime::instance()->getGlobalTable()->set(LuaValue("os"), LuaValue(table));

#define ENTRY(name) {#name, &os_##name}
    CFuncEntry entries[] = {
        ENTRY(clock), ENTRY(date), ENTRY(difftime),
        ENTRY(execute), ENTRY(exit), ENTRY(getenv),
        ENTRY(remove), ENTRY(rename), ENTRY(setlocale),
        ENTRY(time), ENTRY(tmpname),
    };
#undef ENTRY
    for (auto &entry : entries) table->set(LuaValue(entry.name), LuaValue(CFunction::create(entry.func)));
}
