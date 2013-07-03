
#include "pch.h"
#include "BEStorage.h"
#include "BESymbolTable.h"

BEVariable::~BEVariable() {
    if (placeFlag & PF_InRegister) {
        ASSERT(reg != NULL);
        reg->loadedVars.erase(this);
    }
}

const BEType* BELeftValueVariable::getType() {
    return symbol->type;
}

BESymbol* BERightValueVariable::getValidAddress() {
    if (symbol == NULL) {
        symbol = symbolTable->declare("temp", type);
    }
    return symbol;
}
BERightValueVariable::~BERightValueVariable() {
    if (symbol != NULL) {
        symbolTable->undeclare(symbol->name);
    }
}
