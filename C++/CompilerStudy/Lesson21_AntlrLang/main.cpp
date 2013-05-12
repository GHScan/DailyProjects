
#include "pch.h"
#include "JSMinusLexer.hpp"
#include "JSMinusParser.hpp"

int main() {
    JSMinusLexer::InputStreamType input((ANTLR_UINT8*)"test.js", ANTLR_ENC_8BIT);
    JSMinusLexer lxr(&input); 
    JSMinusParser::TokenStreamType tstream(ANTLR_SIZE_HINT, lxr.get_tokSource() );
    JSMinusParser psr(&tstream); 
    psr.program();
}
