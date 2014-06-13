#ifndef SCHEME_PARSER_H
#define SCHEME_PARSER_H

#include "SchemeRef.h"

class SchemeScanner;
class SchemeMemoryManager;

class SchemeParser {
public:

    static SchemeRef parse(SchemeMemoryManager *mgr, SchemeScanner *scanner);
};

#endif
