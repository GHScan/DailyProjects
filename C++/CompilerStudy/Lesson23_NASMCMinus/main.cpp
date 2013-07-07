
#include "pch.h"

#include "CMinusLexer.hpp"
#include "CMinusParser.hpp"
#include "BEx86FileBuilder.h"
#include "BEx86CodeGenerator.h"
#include "BEx86CodeOptimizer.h"
#include "BEx86ASMSerializer.h"

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("%s file [-O] [-v]\n" 
             "-O: optimize\n"
             "-v: dump bytecode\n", argv[0]);
        return 0;
    }

    bool isOptimize = false;
    bool isDumpByteCode = false;
    for (int i = 2; i < argc; ++i) {
        if (argv[i] == string("-v")) isDumpByteCode = true;
        else if (argv[i] == string("-O")) isOptimize = true;
    }
    (void)isDumpByteCode;


#ifdef CHECK_MEMORY_LEAKS
#ifdef _MSC_VER
    _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
#endif
#endif

    try {
        CMinusLexer::InputStreamType input((ANTLR_UINT8*)argv[1], ANTLR_ENC_8BIT);
        CMinusLexer lxr(&input); 
        CMinusParser::TokenStreamType tstream(ANTLR_SIZE_HINT, lxr.get_tokSource());
        CMinusParser psr(&tstream); 

        BEx86FileBuilder *builder = generatex86Code(psr.program().get());

        if (isOptimize) optimizex86Code(builder);

        ofstream fo(format("%s.asm", argv[1]).c_str());
        serializex86Code_nasm(fo, builder);

        delete builder;
    } catch(const exception &e) {
        printf("Unhandled exception : %s\n", e.what());
    }
}
