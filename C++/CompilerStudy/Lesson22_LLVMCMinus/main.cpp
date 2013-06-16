
#include "pch.h"

#include "CMinusLexer.hpp"
#include "CMinusParser.hpp"

int main() {
    CMinusLexer::InputStreamType input((ANTLR_UINT8*)"test/test.c", ANTLR_ENC_8BIT);
    CMinusLexer lxr(&input); 
    CMinusParser::TokenStreamType tstream(ANTLR_SIZE_HINT, lxr.get_tokSource() );
    CMinusParser psr(&tstream); 
    psr.program();
}
