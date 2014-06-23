#ifndef SSCRIPTFUNCTIONPROTO_H
#define SSCRIPTFUNCTIONPROTO_H

#include "SymbolTable.h"

struct SScriptFunctionProto {
    int formalCount;
    vector<string> locals;
    vector<VarAddress> freeAddresses;
};

#endif
