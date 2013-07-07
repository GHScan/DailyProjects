
#include "pch.h"
#include "BEStorage.h"
#include "BESymbolTable.h"

void BERegister::linkVariable(BEVariable *var) {
    ASSERT(var->reg == NULL);
    var->reg = this;
    var->placeFlag |= BEVariable::PF_InRegister;
    loadedVars.insert(var);
}
void BERegister::unlinkVariable(BEVariable *var) {
    ASSERT(this == var->reg);
    loadedVars.erase(var);
    var->reg = NULL;
    var->placeFlag &= ~BEVariable::PF_InRegister;
}

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
