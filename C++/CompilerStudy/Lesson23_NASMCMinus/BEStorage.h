
#ifndef BE_STORAGE_H
#define BE_STORAGE_H

struct BESymbol;
struct BEVariable;

struct BERegister {
    BERegister(int _regType): regType(_regType){}
    const int regType;
    map<BESymbol*, BEVariable*> loadedVars;
};

struct BEVariable {
    enum PlaceFlag {
        PF_InMemory = 1 << 0,
        PF_InRegister = 1 << 1,
    };

    PlaceFlag placeFlag;
    BERegister *reg;
    BESymbol *symbol;

    BEVariable(BERegister *_reg): reg(_reg), symbol(NULL), placeFlag(PF_InRegister){}
    BEVariable(BESymboll *_symbol): reg(NULL), symbol(_symbol), placeFlag(PF_InMemory) {}
};

#endif
