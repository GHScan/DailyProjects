#include "pch.h"
#include "SSymbol.h"

SSymbolManager::SSymbolManager() {
    getSymbol("quote", SSymbol::ID_Quote);
    getSymbol("if", SSymbol::ID_If);
    getSymbol("begin", SSymbol::ID_Begin);
    getSymbol("lambda", SSymbol::ID_Lambda);
    getSymbol("define", SSymbol::ID_Define);
    getSymbol("set!", SSymbol::ID_Set);
}

