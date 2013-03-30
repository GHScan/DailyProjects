
#include "pch.h"

#include <stdio.h>

#include "LuaLibs.h"
#include "LuaValue.h"
#include "LuaTable.h"
#include "Runtime.h"
#include "Function.h"

#define MAX_LINE 256
#define IOFIELD_IO_META "_filemeta"
#define IOFIELD_IDEFAULT "_idefault"
#define IOFIELD_ODEFAULT "_odefault"
#define FILEFIELD_TYPE "_type"
#define OBJ_TYPE "file"

static void __gc(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto f = (FILE*)args[0].getTable()->get(LuaValue(NumberType(1))).getLightUserData();
    if (f != NULL) fclose(f);
}
static void __lineIter(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto f = (FILE*)args[0].getTable()->get(LuaValue(NumberType(1))).getLightUserData();
    string line(MAX_LINE, 0); 
    char *p = fgets((char*)line.c_str(), line.size(), f);
    if (p != NULL) p[strlen(p) - 1] = 0;
    rets.push_back(p == NULL ? LuaValue::NIL : LuaValue(line));
}

static void file_close(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto table = args[0].getTable();
    auto f = (FILE*)table->get(LuaValue(NumberType(1))).getLightUserData();
    if (f != NULL) {
        fclose(f);
        table->set(LuaValue(NumberType(1)), LuaValue((LightUserData)NULL));
    }
}
static void file_flush(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto f = (FILE*)args[0].getTable()->get(LuaValue(NumberType(1))).getLightUserData();
    fflush(f);
}
static void file_lines(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    rets.push_back(LuaValue(CFunction::create(&__lineIter)));
    rets.push_back(args[0]);
}
static void file_read(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto f = (FILE*)args[0].getTable()->get(LuaValue(NumberType(1))).getLightUserData();

    if (args.size() > 1 && args[1].isTypeOf(LVT_Number)) {
        int n = (int)args[1].getNumber();
        string s(n + 1, 0);
        fread((char*)s.c_str(), n, 1, f);
        rets.push_back(LuaValue(s));
        return;
    }

    string fmt = "*a";
    if (args.size() > 1) fmt = args[1].getString();
    if (fmt == "*a") {
        long off = ftell(f);
        fseek(f, 0, SEEK_END);
        long off2 = ftell(f); 
        fseek(f, off, SEEK_SET);

        if (off2 > off) {
            string buf(off2 - off + 1, 0);
            fread((char*)buf.c_str(), buf.size(), 1, f);
            rets.push_back(LuaValue(string(buf)));
        } else rets.push_back(LuaValue::NIL);

    } else if (fmt == "*n") {
        double n;
        if (fscanf(f, "%lf", &n) > 0) {
            rets.push_back(LuaValue(NumberType(n)));
        } else rets.push_back(LuaValue::NIL);
    } else if (fmt == "*l") {
        string line(MAX_LINE, 0); 
        char *p = fgets((char*)line.c_str(), line.size(), f);
        if (p != NULL) p[strlen(p) - 1] = 0;
        rets.push_back(p == NULL ? LuaValue::NIL : LuaValue(string(p)));
    } else ASSERT(0);
}
static void file_seek(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto f = (FILE*)args[0].getTable()->get(LuaValue(NumberType(1))).getLightUserData();
    string where = "cur";
    int off = 0;
    if (args.size() >= 2) where = args[1].getString();
    if (args.size() >= 3) off = (int)args[2].getNumber();

    int err = 0;
    if (where == "cur") {
        err = fseek(f, off, SEEK_CUR);
    } else if (where == "set") {
        err = fseek(f, off, SEEK_SET);
    } else if (where == "end") {
        err = fseek(f, off, SEEK_END);
    } else ASSERT(0);
    if (err == 0) {
        rets.push_back(LuaValue(NumberType(ftell(f))));
    } else rets.push_back(LuaValue::NIL);
}
static void file_setvbuf(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    ASSERT(0);
}
static void file_write(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto f = (FILE*)args[0].getTable()->get(LuaValue(NumberType(1))).getLightUserData();
    for (int i = 1; i < (int)args.size(); ++i) {
        auto &arg(args[i]);
        if (arg.isTypeOf(LVT_String)) {
            const char *s = arg.getString();
            fwrite(s, strlen(s), 1, f);
        } else if (arg.isTypeOf(LVT_Number)) {
            fprintf(f, "%s", arg.toString().c_str());
        } else ASSERT(0);
    }
}

static LuaTable* getFileMetaTable() {
    auto ioTable = Runtime::instance()->getGlobalTable()->get(LuaValue(string("io"))).getTable();
    auto meta = ioTable->get(LuaValue(string(IOFIELD_IO_META)));
    if (meta.isTypeOf(LVT_Nil)) {
        auto table = LuaTable::create();
        table->set(LuaValue(string("__gc")), LuaValue(CFunction::create(&__gc)));
        table->addRef();
        table->set(LuaValue(string("__index")), LuaValue(table));

        table->set(LuaValue(string("close")), LuaValue(CFunction::create(&file_close)));
        table->set(LuaValue(string("flush")), LuaValue(CFunction::create(&file_flush)));
        table->set(LuaValue(string("lines")), LuaValue(CFunction::create(&file_lines)));
        table->set(LuaValue(string("read")), LuaValue(CFunction::create(&file_read)));
        table->set(LuaValue(string("seek")), LuaValue(CFunction::create(&file_seek)));
        table->set(LuaValue(string("setvbuf")), LuaValue(CFunction::create(&file_setvbuf)));
        table->set(LuaValue(string("write")), LuaValue(CFunction::create(&file_write)));

        ioTable->set(LuaValue(string(IOFIELD_IO_META)), LuaValue(table));
        meta = ioTable->get(LuaValue(string(IOFIELD_IO_META)));
    }
    return meta.getTable();
}

static LuaTable* attachFile(FILE *f) {
    ASSERT(f != NULL);
    auto t = LuaTable::create();
    t->set(LuaValue(NumberType(1)), LuaValue((LightUserData)f));
    t->set(LuaValue(string(FILEFIELD_TYPE)), LuaValue(string(OBJ_TYPE)));
    t->setMetaTable(getFileMetaTable());
    return t;
}
static LuaTable* openFile(const char *fname, const char *mode) {
    auto f = fopen(fname, mode);
    if (f == NULL) return NULL;
    return attachFile(f);
}

static void io_close(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    vector<LuaValue> _args(args);
    if (_args.empty()) {
        _args.push_back(
                Runtime::instance()->getGlobalTable()->get(LuaValue(string("io"))).getTable()->get(LuaValue(string(IOFIELD_ODEFAULT))));
    } 
    file_close(_args, rets);
}
static void io_flush(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    vector<LuaValue> _args(args);
    if (_args.empty()) {
        _args.push_back(
                Runtime::instance()->getGlobalTable()->get(LuaValue(string("io"))).getTable()->get(LuaValue(string(IOFIELD_ODEFAULT))));
    } 
    file_flush(_args, rets);
}
static void io_input(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto r = Runtime::instance()->getGlobalTable()->get(LuaValue(string("io"))).getTable()->get(LuaValue(string(IOFIELD_IDEFAULT)));
    if (!args.empty()) {
        LuaValue f(args[0]);
        if (f.isTypeOf(LVT_String)) {
            f = LuaValue(openFile(f.getString(), "r"));
        }
        ASSERT(f.isTypeOf(LVT_Table));
        Runtime::instance()->getGlobalTable()->get(LuaValue(string("io"))).getTable()->set(LuaValue(string(IOFIELD_IDEFAULT)), f);
    }
    rets.push_back(r);
}
static void io_lines(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    vector<LuaValue> _args;
    if (args.empty()) {
        _args.push_back(
                Runtime::instance()->getGlobalTable()->get(LuaValue(string("io"))).getTable()->get(LuaValue(string(IOFIELD_IDEFAULT))));
    } else {
        _args.push_back(LuaValue(openFile(args[0].getString(), "r")));
    }
    file_lines(_args, rets);
}
static void io_open(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    const char *fname = args[0].getString();
    const char *mode = "r";
    if (args.size() >= 2) mode = args[1].getString();
    if (auto t = openFile(fname, mode)) {
        rets.push_back(LuaValue(t));
    } else {
        rets.push_back(LuaValue::NIL);
        rets.push_back(LuaValue(string("fopen failed!!")));
    }
}
static void io_output(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto r = Runtime::instance()->getGlobalTable()->get(LuaValue(string("io"))).getTable()->get(LuaValue(string(IOFIELD_ODEFAULT)));
    if (!args.empty()) {
        LuaValue f(args[0]);
        if (f.isTypeOf(LVT_String)) {
            f = LuaValue(openFile(f.getString(), "w"));
        }
        ASSERT(f.isTypeOf(LVT_Table));
        Runtime::instance()->getGlobalTable()->get(LuaValue(string("io"))).getTable()->set(LuaValue(string(IOFIELD_ODEFAULT)), f);
    }
    rets.push_back(r);
}
static void io_popen(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    ASSERT(0);
}
static void io_read(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    vector<LuaValue> _args(args);
    auto file = Runtime::instance()->getGlobalTable()->get(LuaValue(string("io"))).getTable()->get(LuaValue(string(IOFIELD_IDEFAULT)));
    _args.insert(_args.begin(), file);
    file_read(_args, rets);
}
static void io_tmpfile(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    rets.push_back(LuaValue(attachFile(tmpfile())));
}
static void io_type(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto &arg(args[0]);
    if (arg.isTypeOf(LVT_Table)) {
        auto table = arg.getTable();
        LuaValue type = table->get(LuaValue(string(FILEFIELD_TYPE)));
        if (type.isTypeOf(LVT_String) && type.getString() == string(OBJ_TYPE)) {
            auto f = (FILE*)table->get(LuaValue(NumberType(1))).getLightUserData();
            rets.push_back(f == NULL ? LuaValue(string("closed file")) : LuaValue(string("file")));
            return;
        }
    }
    rets.push_back(LuaValue::NIL);
}
static void io_write(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    vector<LuaValue> _args(args);
    auto file = Runtime::instance()->getGlobalTable()->get(LuaValue(string("io"))).getTable()->get(LuaValue(string(IOFIELD_ODEFAULT)));
    _args.insert(_args.begin(), file);
    file_write(_args, rets);
}

extern void openLib_io() {
#define ENTRY(name) {#name, &io_##name}
    CFuncEntry entries[] = {
        ENTRY(close),
        ENTRY(flush),
        ENTRY(input),
        ENTRY(lines),
        ENTRY(open),
        ENTRY(output),
        ENTRY(popen),
        ENTRY(read),
        ENTRY(tmpfile),
        ENTRY(type),
        ENTRY(write),
    };
#undef ENTRY
    auto table = LuaTable::create();
    Runtime::instance()->getGlobalTable()->set(LuaValue(string("io")), LuaValue(table));
    for (auto &entry : entries) {
        table->set(LuaValue(entry.name), LuaValue(CFunction::create(entry.func)));
    }
    table->set(LuaValue(string("stdin")), LuaValue(attachFile(stdin)));
    table->set(LuaValue(string("stdout")), LuaValue(attachFile(stdout)));
    table->set(LuaValue(string("stderr")), LuaValue(attachFile(stderr)));

    table->set(LuaValue(string(IOFIELD_IDEFAULT)), table->get(LuaValue(string("stdin"))));
    table->set(LuaValue(string(IOFIELD_ODEFAULT)), table->get(LuaValue(string("stdout"))));
}
