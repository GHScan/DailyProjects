
#ifndef BE_STORAGE_H
#define BE_STORAGE_H

struct BESymbol;
struct BEVariable;
struct BEType;
class BESymbolTable;
struct BEConstant;

struct BERegister: public Noncopyable {
    BERegister(int _regType): regType(_regType), isWritten(false){}
    const int regType;
    set<BEVariable*> loadedVars;

    void linkVariable(BEVariable *var);
    void unlinkVariable(BEVariable *var);
    bool isWritten;
};

struct BEVariable: public Noncopyable {
    enum PlaceFlag {
        PF_InRegister = 1 << 0,
        PF_InMemory = 1 << 1,
    };
    int placeFlag;
    BERegister *reg;
    bool isInRegister() const { return (placeFlag & PF_InRegister) != 0; }
    bool isInMemory() const {  return (placeFlag & PF_InMemory) != 0; }
    virtual void setMemoryValid() { placeFlag |= PF_InMemory; }
    virtual void setMemoryDirty() { placeFlag &= ~PF_InMemory;}
    virtual BEConstant* tryGetConstant() { return NULL; }
    virtual BESymbol* getValidAddress() = 0;
    virtual const BEType* getType() = 0;
    virtual ~BEVariable();
protected:
    BEVariable(PlaceFlag _placeFlag, BERegister *_reg): placeFlag(_placeFlag), reg(_reg) {}
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
        reg->loadedVars.insert(this);
    }
    virtual BESymbol* getValidAddress();
    virtual const BEType* getType() { return type; }
    ~BERightValueVariable();
};
struct BEConstantProxyVariable: public BEVariable {
    BEConstant *constant;
    BEConstantProxyVariable(BEConstant *_constant): BEVariable(PF_InMemory, NULL), constant(_constant){}
    virtual void setMemoryValid() { ASSERT(0); }
    virtual void setMemoryDirty() { ASSERT(0); }
    virtual BEConstant* tryGetConstant() { return constant; }
    virtual BESymbol* getValidAddress() { ASSERT(0); return NULL; }
    virtual const BEType* getType();
};

typedef shared_ptr<BEVariable> BEVariablePtr;
 
#endif
