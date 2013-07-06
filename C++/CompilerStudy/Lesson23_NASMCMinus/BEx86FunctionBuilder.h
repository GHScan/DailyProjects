
#ifndef BE_x86_FUNCTION_BUILDER_H
#define BE_x86_FUNCTION_BUILDER_H

struct BEType;
struct BERegister;
struct BESymbol;
struct BEVariable;
struct BEConstant;
struct BEx86Instruction;
class BEx86FileBuilder;
class BESymbolTable;
struct BEx86BasicBlock;

typedef shared_ptr<BEVariable> BEVariablePtr;

enum BEx86RegisterType {
    x86RT_EAX,
    x86RT_EBX,
    x86RT_ECX,
    x86RT_EDX,
    x86RT_ESI,
    x86RT_EDI,
    x86RT_ESP,
    x86RT_EBP,

    x86RT_GRCount = 6,
    x86RT_Count = 8,
};

enum BEx86InstructionType {
    x86IT_MOV,
    x86IT_LEA,

    x86IT_AND,
    x86IT_OR,

    x86IT_NOT,
    x86IT_INC,
    x86IT_DEC,

    x86IT_ADD,
    x86IT_SUB,
    x86IT_MUL,
    x86IT_DIV,
    x86IT_MOD,

    x86IT_SAL,
    x86IT_SAR,

    x86IT_CMP,

    x86IT_JMP,
    x86IT_JZ,
    x86IT_JNZ,
    x86IT_JE,
    x86IT_JNE,
    x86IT_JG,
    x86IT_JGE,
    x86IT_JL,
    x86IT_JLE,

    x86IT_NOP,
    x86IT_PUSH,
    x86IT_POP,
    x86IT_RET,
    x86IT_CALL,
};

enum BEx86OperandType {
    x86OT_Null,
    x86OT_Register,
    x86OT_Memory,
    x86OT_Constant,
    x86OT_BasicBlock,
};

struct BEx86Operand {
    BEx86OperandType type;
    union {
        BERegister *reg;
        BESymbol *symbol;
        BEConstant *constant;
        BEx86BasicBlock *basicBlock;
    };
    BEx86Operand(): type(x86OT_Null){}
    BEx86Operand(BERegister *reg): type(x86OT_Register){this->reg = reg;}
    BEx86Operand(BESymbol *symbol): type(x86OT_Memory){this->symbol = symbol;}
    BEx86Operand(BEConstant *constant): type(x86OT_Constant){this->constant = constant;}
    BEx86Operand(BEx86BasicBlock *basicBlock): type(x86OT_BasicBlock){this->basicBlock = basicBlock;}
};

struct BEx86Instruction {
    BEx86InstructionType type;
    BEx86Operand operands[2];

    BEx86Instruction(BEx86InstructionType _type): type(_type) {}
    BEx86Instruction(BEx86InstructionType _type, BEx86Operand operand0): type(_type) {
        operands[0] = operand0;
    }
    BEx86Instruction(BEx86InstructionType _type, BEx86Operand operand0, BEx86Operand operand1): type(_type) {
        operands[0] = operand0; operands[1] = operand1;
    }
};

struct BEx86BasicBlock {
    string name;
    vector<BEx86Instruction> instructions;
    BEx86BasicBlock(const string &_name): name(_name){}
};

class BEx86FunctionBuilder {
public:
    BEx86FunctionBuilder(BEx86FileBuilder *parent);
    ~BEx86FunctionBuilder();

    BEx86FileBuilder* getParent() { return m_parent; }

    void beginScope();
    void endScope();
    BEVariablePtr declareArgVariable(const string& name, const BEType *type);
    BEVariablePtr declareLocalVariable(const string &name, const BEType *type);
    BEVariablePtr getLocalVariable(const string &name);
    BEVariablePtr getGlobalVariable(const string &name);

    BESymbolTable* getArgSymbolTable();
    BESymbolTable* getTopLocalSymbolTable();

    BEx86BasicBlock* createBasicBlock(const string &name);
    void pushBasicBlock(BEx86BasicBlock *basicBlock);
public:
    void beginBuild();
    
    BEVariablePtr createTempFrom(BEVariablePtr src);
    BEVariablePtr loadConstant(BEConstant *constant);
    BEVariablePtr store(BEVariablePtr dest, BEVariablePtr src);

    BEVariablePtr createInc(BEVariablePtr &dest);
    BEVariablePtr createDec(BEVariablePtr &dest);
    BEVariablePtr createNot(BEVariablePtr &dest);
    BEVariablePtr createAdd(BEVariablePtr &dest, BEVariablePtr src);
    BEVariablePtr createSub(BEVariablePtr &dest, BEVariablePtr src);
    BEVariablePtr createMul(BEVariablePtr &dest, BEVariablePtr src);
    BEVariablePtr createDiv(BEVariablePtr &dest, BEVariablePtr src);
    BEVariablePtr createMod(BEVariablePtr &dest, BEVariablePtr src);

    BEVariablePtr createLt(BEVariablePtr &left, BEVariablePtr right);
    BEVariablePtr createLe(BEVariablePtr &left, BEVariablePtr right);
    BEVariablePtr createGt(BEVariablePtr &left, BEVariablePtr right);
    BEVariablePtr createGe(BEVariablePtr &left, BEVariablePtr right);
    BEVariablePtr createEq(BEVariablePtr &left, BEVariablePtr right);
    BEVariablePtr createNe(BEVariablePtr &left, BEVariablePtr right);

    void createJmp(BEx86BasicBlock *basicBlock);
    void createCJmp(BEVariablePtr &cond, BEx86BasicBlock *trueBlock, BEx86BasicBlock *falseBlock);

    void beginCall(int n);
    void createPush(BEVariablePtr var);
    BEVariablePtr endCall(const BEType *type, BESymbol *funcSymbol, int n);

    void createRet();
    void createRet(BEVariablePtr dest);

    void endBuild();
private:
    BEx86FunctionBuilder(const BEx86FunctionBuilder &);
    BEx86FunctionBuilder& operator = (const BEx86FunctionBuilder &);

private:
    void pushInstruction(const BEx86Instruction &ins);

    BERegister* findLeastUseRegister();
    BERegister* makeRegisterFree(BERegister *reg);
    void makeAllRegisterFree();
    BERegister* getFreeRegister();
    void makesureVariableInRegister(BEVariable *var);
    void makesureVariableInMemory(BEVariable *var);
    void makeVariableInMemoryOnly(BEVariable *var);
    void storeVariableFromRegister(BEVariable *dest, BERegister *src);
    void loadVariableToRegister(BERegister *reg, BEVariable *var);

    BEVariablePtr createArithmeticOp(BEx86InstructionType insType, BEVariablePtr &dest, BEVariablePtr src);
    BEVariablePtr createRelativeOp(BEx86InstructionType insType, BEVariablePtr &left, BEVariablePtr right);
private:
    BEx86FileBuilder *m_parent;
    BESymbolTable *m_topLocalSymbolTable, *m_argSymbolTable;
    vector<BESymbolTable*> m_usedSymbolTables;
    vector<BERegister*> m_registers;
    map<BESymbol*, BEVariablePtr> m_leftValueVars;
    int m_maxArgOff, m_maxLocalOff;
    vector<BEx86BasicBlock*> m_basicBlocks;
    BEx86BasicBlock *m_retBasicBlock;
};

#endif
