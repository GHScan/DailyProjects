
#include "pch.h"

#include <time.h>

#include "JSMinusLexer.hpp"
#include "JSMinusParser.hpp"
#include "ByteCode.h"
#include "JSFunction.h"
#include "JSString.h"
#include "JSArray.h"
#include "GCObject.h"

static JSValue loadFile(const char* fileName) {
    JSMinusLexer::InputStreamType input((ANTLR_UINT8*)fileName, ANTLR_ENC_8BIT);
    JSMinusLexer lxr(&input); 
    JSMinusParser::TokenStreamType tstream(ANTLR_SIZE_HINT, lxr.get_tokSource() );
    JSMinusParser psr(&tstream); 
    return JSValue::fromFunction(JSFunction::create(psr.program(fileName)));
}

static int buildin_print(JSValue* begin, JSValue *end) {
    for (; begin != end; ++begin) {
        printf("%s\t", begin->toString().c_str());
    }
    return 0;
}
static int buildin_println(JSValue* begin, JSValue *end) {
    buildin_print(begin, end);
    puts("");
    return 0;
}
static int buildin_format(JSValue *begin, JSValue *end) {
    ASSERT(end - begin >= 1 && begin->type == JSVT_String);
    const char *fmt = begin->data.str->buf;
    string r, buf;
    int i = 1;
    for (; *fmt; ++fmt) {
        if (*fmt == '%') {
            r += buf;
            buf.clear();

            buf.push_back(*fmt);
            while (strchr("dsf", *++fmt) == NULL) buf.push_back(*fmt);
            buf.push_back(*fmt);
            JSValue v;
            if (i + begin < end) v = begin[i];

            switch (*fmt) {
                case 'd': r += format(buf.c_str(), (int)v.data.num); break;
                case 's': r += format(buf.c_str(), v.data.str->buf); break;
                case 'f': r += format(buf.c_str(), v.data.num); break;
                default: ASSERT(0); break;
            }

            ++i;
            buf.clear();
        }
        else {
            buf.push_back(*fmt);
        }
    }
    r += buf;
    *begin = JSValue::fromString(r.c_str());
    return 1;
}
static int buildin_clock(JSValue *begin, JSValue *end) {
    *begin = JSValue::fromNumber(::clock() / float(CLOCKS_PER_SEC));
    return 1;
}
static int buildin_disassemble(JSValue *begin, JSValue *end) {
    ostringstream so;
    JSFunction *func = NULL;
    if (end == begin) {
        func = JSVM::instance()->topFrame()->func;
    } else {
        ASSERT(begin->type == JSVT_Function && begin->data.func->funcType == Function::FT_JS);
        func = static_cast<JSFunction*>(begin->data.func);
    }
    disassemble(so, func->meta, 0);
    *begin = JSValue::fromString(so.str().c_str());
    return 1;
}
static int buildin_collectgarbage(JSValue *begin, JSValue *end) {
    auto mgr = GCObjectManager::instance();
    int oldCount = mgr->getObjectCount();
    mgr->performFullGC();
    *begin = JSValue::fromNumber(oldCount - mgr->getObjectCount());
    return 1;
}
static int buildin_readfile(JSValue *begin, JSValue *end) {
    ASSERT(end - begin >= 1);
    ASSERT(begin[0].type == JSVT_String);
    ifstream fi(begin[0].data.str->buf);
    string r;
    for (string line; getline(fi, line); r += line + '\n');
    *begin = JSValue::fromString(r.c_str());
    return 1;
}
static int buildin_writefile(JSValue *begin, JSValue *end) {
    ASSERT(end - begin >= 2);
    ASSERT(begin[0].type == JSVT_String && begin[1].type == JSVT_String);
    ofstream(begin[0].data.str->buf) << begin[1].data.str->buf;
    return 0;
}
static int buildin_loadfile(JSValue *begin, JSValue *end) {
    ASSERT(end - begin >= 1);
    ASSERT(begin[0].type == JSVT_String);
    *begin = loadFile(begin->data.str->buf);
    return 1;
}
static int buildin_type(JSValue *begin, JSValue *end) {
    ASSERT(end > begin);
    string str;
    switch (begin->type) {
        case JSVT_Nil: str = "nil"; break;
        case JSVT_Boolean: str = "boolean"; break;
        case JSVT_Number: str = "number"; break;
        case JSVT_String: str = "string"; break;
        case JSVT_Array: str = "array"; break;
        case JSVT_Function: str = "function"; break;
        default: ASSERT(0); break;
    }
    *begin = JSValue::fromString(str.c_str());
    return 1;
}
static int buildin_insert(JSValue *begin, JSValue *end) {
    JSValue *array, *value;
    int pos;
    if (end - begin == 2) {
        array = begin, value = begin + 1;
        ASSERT(array->type == JSVT_Array);
        pos = (int)array->data.array->array.size();
    } else if (end - begin > 2) {
        array = begin, value = begin + 2;
        ASSERT(array->type == JSVT_Array);
        ASSERT(begin[1].type == JSVT_Number);
        pos = (int)begin[1].data.num;
    } else {
        ASSERT(0);
    }
    array->data.array->array.insert(array->data.array->array.begin() + pos, *value);
    return 0;
}
static int buildin_remove(JSValue *begin, JSValue *end) {
    ASSERT(end - begin >= 1);
    JSValue *array = begin;
    ASSERT(array->type == JSVT_Array);
    int pos = (int)array->data.array->array.size() - 1;
    if (end - begin > 1) {
        ASSERT(begin[1].type == JSVT_Number);
        pos = (int)begin[1].data.num;
    }
    array->data.array->array.erase(array->data.array->array.begin() + pos);
    return 0;
}
static int buildin_tostring(JSValue *begin, JSValue *end) {
    ASSERT(end > begin);
    *begin = JSValue::fromString(begin->toString().c_str());
    return 1;
}
static int buildin_srand(JSValue *begin, JSValue *end) {
    ASSERT(end > begin);
    ASSERT(begin->type == JSVT_Number);
    ::srand((int)begin->data.num);
    return 0;
}
static int buildin_random(JSValue *begin, JSValue *end) {
    *begin = JSValue::fromNumber(::rand());
    return 1;
}
static int buildin_time(JSValue *begin, JSValue *end) {
    *begin = JSValue::fromNumber(::time(NULL));
    return 1;
}

static void registerBuildins() {
#define ENTRY(name) {#name, buildin_##name}
    CFuncEntry entries[] = {
        ENTRY(print), ENTRY(println), ENTRY(format), ENTRY(clock),
        ENTRY(disassemble), ENTRY(collectgarbage), ENTRY(readfile), ENTRY(writefile),
        ENTRY(loadfile), ENTRY(type), ENTRY(insert), ENTRY(remove),
        ENTRY(tostring), ENTRY(srand), ENTRY(random), ENTRY(time),
    };
#undef ENTRY
    for (auto entry : entries) {
        JSVM::instance()->setGlobal(JSValue::fromString(entry.name), JSValue::fromFunction(CFunction::create(entry.func)));
    }
}

static void runFile(int argc, char *argv[]) {
    JSVM::createInstance();
    registerBuildins();

    vector<JSValue> args;
    for (int i = 2; i < argc; ++i) {
        args.push_back(JSValue::fromString(argv[i]));
    }
    if (args.empty()) args.push_back(JSValue::NIL);
    loadFile(argv[1]).data.func->callFromC(&args[0], &args[0] + args.size());

    JSVM::destroyInstance();
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        puts("Usage : main filename [args ...]");
        return 1;
    }

#ifdef CHECK_MEMORY_LEAKS
#ifdef _MSC_VER
    _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
#endif
#endif

    try {
        runFile(argc, argv);
    } catch(const exception& e) {
        printf("Unhandled exception: %s\n", e.what());
    }
}
