
#include "pch.h"
#include "ExprLexer.hpp"
#include "ExprParser.hpp"

int main() {
    ExprLexer::InputStreamType input((ANTLR_UINT8*)"input.txt", ANTLR_ENC_8BIT);
    ExprLexer lxr(&input); 
    ExprParser::TokenStreamType tstream(ANTLR_SIZE_HINT, lxr.get_tokSource() );
    ExprParser psr(&tstream); 
    psr.prog();
}
