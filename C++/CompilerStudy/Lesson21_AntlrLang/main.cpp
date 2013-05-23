
#include "pch.h"

#include <time.h>

#include "JSMinusLexer.hpp"
#include "JSMinusParser.hpp"
#include "ByteCode.h"
#include "JSFunction.h"
#include "JSString.h"

static JSValue loadFile(const char* fileName) {
    JSMinusLexer::InputStreamType input((ANTLR_UINT8*)fileName, ANTLR_ENC_8BIT);
    JSMinusLexer lxr(&input); 
    JSMinusParser::TokenStreamType tstream(ANTLR_SIZE_HINT, lxr.get_tokSource() );
    JSMinusParser psr(&tstream); 
    return JSValue::fromFunction(JSFunction::create(psr.program()));
}

int buildin_print(JSValue* begin, JSValue *end) {
    for (; begin != end; ++begin) {
        printf("%s\t", begin->toString().c_str());
    }
    return 0;
}
int buildin_println(JSValue* begin, JSValue *end) {
    buildin_print(begin, end);
    puts("");
    return 0;
}
int buildin_format(JSValue *begin, JSValue *end) {
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
int buildin_clock(JSValue *begin, JSValue *end) {
    *begin = JSValue::fromNumber(::clock() / float(CLOCKS_PER_SEC));
    return 1;
}

void registerBuildins() {
#define ENTRY(name) {#name, buildin_##name}
    CFuncEntry entries[] = {
        ENTRY(print), ENTRY(println), ENTRY(format), ENTRY(clock),
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
    auto func = loadFile(argv[1]);
    disassemble(ofstream("dis.txt"), static_cast<JSFunction*>(func.data.func)->meta, 0);
    func.data.func->callFromC(&args[0], &args[0] + args.size());

    JSVM::destroyInstance();
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        puts("Usage : main filename [args ...]");
        return 1;
    }

    runFile(argc, argv);
}
