
#ifndef BE_STORAGE_H
#define BE_STORAGE_H

struct BESymbol;
struct BEVariable;
struct BEType;
class BESymbolTable;

struct BERegister {
    BERegister(int _regType): regType(_regType), isWritten(false){}
    const int regType;
    set<BEVariable*> loadedVars;

    void linkVariable(BEVariable *var);
    bool isWritten;
private:
    BERegister(const BERegister&);
    BERegister& operator = (const BERegister&);
};

struct BEVariable {
    enum PlaceFlag {
        PF_InRegister = 1 << 0,
        PF_InMemory = 1 << 1,
    };
    int placeFlag;
    BERegister *reg;
    virtual BESymbol* getValidAddress() = 0;
    virtual const BEType* getType() = 0;
    virtual ~BEVariable();
protected:
    BEVariable(PlaceFlag _placeFlag, BERegister *_reg): placeFlag(_placeFlag), reg(_reg) {}
private:
    BEVariable(const BEVariable& o);
    BEVariable& operator = (const BEVariable& o);
};

struct BELeftValueVariable: public BEVariable {
    BESymbol *symbol;
    BELeftValueVariable(BESymbol *_symbol): BEVariable(PF_InMemory, NULL), symbol(_symbol){ ASSERT(symbol != NULL); }
    virtual BESymbol* getValidAddress() { return symbol; }
    virtual const BEType* getType();
};
struct BERightValueVariable: public BEVariable {
    const BEType *type;
    BESymbolTable *symbolTable;
    BESymbol *symbol;
    BERightValueVariable(const BEType *_type, BERegister *_reg, BESymbolTable *_symbolTable): 
        BEVariable(PF_InRegister, _reg), type(_type), symbolTable(_symbolTable), symbol(NULL) {
        ASSERT(reg != NULL && symbolTable != NULL && type != NULL);
    }
    virtual BESymbol* getValidAddress();
    virtual const BEType* getType() { return type; }
    ~BERightValueVariable();
};

typedef shared_ptr<BEVariable> BEVariablePtr;
 
#endif
