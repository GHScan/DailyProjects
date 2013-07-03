
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

typedef shared_ptr<BEVariable> BEVariablePtr;

enum BEx86RegisterType {
    x86RT_EAX = 1 << 0,
    x86RT_EBX = 1 << 1,
    x86RT_ECX = 1 << 2,
    x86RT_EDX = 1 << 3,
    x86RT_ESI = 1 << 4,
    x86RT_EDI = 1 << 5,
    x86RT_ESP = 1 << 6,
    x86RT_EBP = 1 << 7,

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

struct BEx86Label {
    const string labelName;
    BEx86Instruction *ins;
    BEx86Label(const string &_labelName, BEx86Instruction* _ins): labelName(_labelName), ins(_ins){}
};

enum BEx86OperandType {
    x86OT_Null,
    x86OT_Register,
    x86OT_Memory,
    x86OT_Constant,
    x86OT_Label,
};

struct BEx86InstructionOperand {
    BEx86OperandType type;
    union {
        BERegister *reg;
        BESymbol *symbol;
        BEConstant *constant;
        BEx86Label *label;
    };
    BEx86InstructionOperand(): type(x86OT_Null){}
    BEx86InstructionOperand(BERegister *reg): type(x86OT_Register){this->reg = reg;}
    BEx86InstructionOperand(BESymbol *symbol): type(x86OT_Memory){this->symbol = symbol;}
    BEx86InstructionOperand(BEConstant *constant): type(x86OT_Constant){this->constant = constant;}
    BEx86InstructionOperand(BEx86Label *label): type(x86OT_Label){this->label = label;}
};

struct BEx86Instruction {
    BEx86InstructionType type;
    BEx86InstructionOperand operands[2];

    BEx86Instruction(BEx86InstructionType _type): type(_type), next(NULL), prev(NULL){}
    BEx86Instruction(BEx86InstructionType _type, BEx86InstructionOperand operand0):
        type(_type), next(NULL), prev(NULL) {
        operands[0] = operand0;
    }
    BEx86Instruction(BEx86InstructionType _type, BEx86InstructionOperand operand0, BEx86InstructionOperand operand1):
        type(_type), next(NULL), prev(NULL) {
        operands[0] = operand0; operands[1] = operand1;
    }

    BEx86Instruction *next, *prev;
    void insertAfter(BEx86Instruction *i);
    void insertBefore(BEx86Instruction *i);
    BEx86Instruction* removeAfter();
    BEx86Instruction* removeBefore();
};

class BEx86FunctionBuilder {
public:
    BEx86FunctionBuilder(BEx86FileBuilder *parent);
    ~BEx86FunctionBuilder();

    void pushBlock();
    void popBlock();
    BEVariablePtr declareLocalVariable(const string &name, BEType *type);
    BEVariablePtr declareArgVariable(const string& name, BEType *type);
    BEVariablePtr getLocalVariable(const string &name);
    BEVariablePtr getGlobalVariable(const string &name);

    BESymbolTable* getArgSymbolTable();
    BESymbolTable* getTopLocalSymbolTable();

    BEx86Instruction* getFirstInstruction();
    BEx86Instruction* getLastInstruction();

public:
    // BERegister* createMOV(BERegister *reg, BERegister *reg);
    BEVariablePtr loadConstant(BEConstant *constant);
    void store(BEVariablePtr dest, BEVariablePtr src);
    // createLEA();

    BEVariablePtr createInc(BEVariablePtr &dest);
    BEVariablePtr createDec(BEVariablePtr &dest);
    BEVariablePtr createNot(BEVariablePtr &dest);
    BEVariablePtr createAnd(BEVariablePtr &dest, BEVariablePtr src);
    BEVariablePtr createOr(BEVariablePtr &dest, BEVariablePtr src);
    BEVariablePtr createAdd(BEVariablePtr &dest, BEVariablePtr src);
    BEVariablePtr createSub(BEVariablePtr &dest, BEVariablePtr src);
    BEVariablePtr createMul(BEVariablePtr &dest, BEVariablePtr src);
    BEVariablePtr createDiv(BEVariablePtr &dest, BEVariablePtr src);
    BEVariablePtr createMod(BEVariablePtr &dest, BEVariablePtr src);

    BEx86Label* createLabel(const string &labelName);
    BEx86Label* createLabel(const string &labelName, BEx86Instruction* ins);

    void createCmp(BEVariablePtr &left, BEVariablePtr right);
    void createJmp(BEx86Label *label);
    void createJz(BEx86Label *label);
    void createJnz(BEx86Label *label);
    void createJe(BEx86Label *label);
    void createJne(BEx86Label *label);
    void createJg(BEx86Label *label);
    void createJge(BEx86Label *label);
    void createJl(BEx86Label *label);
    void createJle(BEx86Label *label);

    BEx86Instruction* createNop();

    //void createPush(BERegister *reg);
    void createPush(BEVariablePtr var);
    //void createPop(BERegister *reg);
    void beginCall(int n);
    void endCall(int n);

    void createRet();
    void createRet(BEVariablePtr dest);

private:
    BEx86FunctionBuilder(const BEx86FunctionBuilder &);
    BEx86FunctionBuilder& operator = (const BEx86FunctionBuilder &);

private:
    BEx86Instruction* pushInstruction(BEx86Instruction* ins);

    BERegister* findLeastUseRegister();
    BERegister* makeRegisterFree(BERegister *reg);
    BERegister* getFreeRegister();
    void makesureVariableInRegister(BEVariable *var);
    void makesureVariableInMemory(BEVariable *var);
    void makeVariableInMemoryOnly(BEVariable *var);
    void storeVariableFromRegister(BEVariable *dest, BERegister *src);
    void loadVariableToRegister(BERegister *reg, BEVariable *var);

    BEVariablePtr createBinaryOp(BEx86InstructionType insType, BEVariablePtr &dest, BEVariablePtr src);
private:
    BEx86FileBuilder *m_parent;
    BEx86Instruction *m_insFirst, *m_insLast;
    vector<BERegister*> m_registers;
    BESymbolTable *m_topLocalSymbolTable, *m_argSymbolTable;
    map<BESymbol*, BEVariablePtr> m_leftValueVars;
    // instead of map, maybe should use vector here
    map<BEx86Instruction*, BEx86Label*> m_ins2Label;
};

#endif
