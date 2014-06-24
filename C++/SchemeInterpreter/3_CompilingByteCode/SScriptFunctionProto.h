#ifndef SSCRIPTFUNCTIONPROTO_H
#define SSCRIPTFUNCTIONPROTO_H

#include "SymbolTable.h"

struct SScriptFunctionProto {
    SScriptFunctionProto *parent;
    int formalCount;
    vector<string> locals;
    vector<uint8_t> bytes;
    vector<VarAddress> freeAddresses;

    SScriptFunctionProto(SScriptFunctionProto *_parent): 
        parent(_parent) {
    }
};
typedef shared_ptr<SScriptFunctionProto> SScriptFunctionProtoPtr;

#endif
