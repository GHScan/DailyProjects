
#include "pch.h"
#include "JSMinusLexer.hpp"
#include "JSMinusParser.hpp"

static JSValue loadFile(const char* fileName) {
    JSMinusLexer::InputStreamType input((ANTLR_UINT8*)fileName, ANTLR_ENC_8BIT);
    JSMinusLexer lxr(&input); 
    JSMinusParser::TokenStreamType tstream(ANTLR_SIZE_HINT, lxr.get_tokSource() );
    JSMinusParser psr(&tstream); 
    return JSValue::fromFunction(JSFunction::create(psr.program()));
}
static void runFile(int argc, char *argv[]) {
    vector<JSValue> args;
    for (int i = 1; i < argc; ++i) {
        args.push_back(JSValue::fromString(argv[i]));
    }
    if (args.empty()) args.push_back(JSValue::NIL);
    loadFile(argv[0]).data.func->callFromC(&args[0], &args[0] + args.size());
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        puts("Usage : main filename [args ...]");
        return 1;
    }

    runFile(argc, argv);
}
