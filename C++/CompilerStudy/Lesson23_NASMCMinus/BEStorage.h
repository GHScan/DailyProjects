
#ifndef BE_STORAGE_H
#define BE_STORAGE_H

struct BESymbol;
struct BEVariable;
struct BEType;
class BESymbolTable;

struct BERegister {
    BERegister(int _regType): regType(_regType){}
    const int regType;
    set<BEVariable*> loadedVars;

    // TODO: implement below, although it is instruction dependent
    void loadVariable(BEVariable *var);
    void flushVariables();

private:
    BERegister(const BERegister&);
    BERegister& operator = (const BERegister&);
};

struct BEVariable {
    enum PlaceFlag {
        PF_InRegister = 1 << 0,
        PF_InMemory = 1 << 1,
    };
    PlaceFlag placeFlag;
    BERegister *reg;
    virtual BESymbol* getValidAddress() = 0;
    virtual BEType* getType() = 0;

protected:
    BEVariable(PlaceFlag _placeFlag, BERegister *_reg): placeFlag(_placeFlag), reg(_reg) {}
    virtual ~BEVariable();
private:
    BEVariable(const BEVariable& o);
    BEVariable& operator = (const BEVariable& o);
};

struct BEVariableLeftValue: public BEVariable {
    BESymbol *symbol;
    BEVariableLeftValue(BESymbol *_symbol): BEVariable(PF_InMemory, NULL), symbol(_symbol){ ASSERT(symbol != NULL); }
    virtual BESymbol* getValidAddress() { return symbol; }
    virtual BEType* getType();
};
struct BEVariableRightValue: public BEVariable {
    BEType *type;
    BESymbolTable *symbolTable;
    BESymbol *symbol;
    BEVariableRightValue(BEType *_type, BERegister *_reg, BESymbolTable *_symbolTable): 
        BEVariable(PF_InRegister, _reg), type(_type), symbolTable(_symbolTable), symbol(NULL) {
        ASSERT(reg != NULL && symbolTable != NULL && type != NULL);
    }
    virtual BESymbol* getValidAddress();
    virtual BEType* getType() { return type; }
    ~BEVariableRightValue();
};

typedef shared_ptr<BEVariable> BEVariablePtr;
 
#endif
