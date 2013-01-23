#ifndef SYNTAXPARSER_H
#define SYNTAXPARSER_H

#include "Scanner.h"

class SyntaxParser
{
public:
    SyntaxParser(Scanner *scanner);
    ~SyntaxParser();
    bool parse();
private:
    class SyntaxParserImpl *m_impl;
};

#endif
