
#include "pch.h"
#include "BEStorage.h"
#include "BESymbolTable.h"

BEVariable::~BEVariable() {
    if (placeFlag & PF_InRegister) {
        ASSERT(reg != NULL);
        reg->loadedVars.erase(this);
    }
}

BEType* BEVariableLeftValue::getType() {
    return symbol->type;
}

BESymbol* BEVariableRightValue::getValidAddress() {
    if (symbol == NULL) {
        symbol = symbolTable->declare("temp", type);
    }
    return symbol;
}
BEVariableRightValue::~BEVariableRightValue() {
    if (symbol != NULL) {
        symbolTable->undeclare(symbol->name);
    }
}
