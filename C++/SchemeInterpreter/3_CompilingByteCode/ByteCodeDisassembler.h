#ifndef BYTECODEDISASSEMBLER_H
#define BYTECODEDISASSEMBLER_H

#include "SScriptFunctionProto.h"
#include "SValue.h"
class SymbolTable;

void disassembleByteCode(
        ostream& so,
        const SScriptFunctionProto *proto,
        SymbolTable *gSymTable,
        const vector<SScriptFunctionProtoPtr> &protos,
        const vector<SValue> &literals, 
        int indent = 0);

#endif
