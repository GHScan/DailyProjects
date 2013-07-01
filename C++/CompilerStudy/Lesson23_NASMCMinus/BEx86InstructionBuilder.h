
#ifndef BE_x86_INSTRUCTION_BUILDER_H
#define BE_x86_INSTRUCTION_BUILDER_H

struct BEx86Register;
struct BESymbol;
struct BEx86Instruction;
struct BEVariable;
struct BEConstant;

enum BEx86InstructionType {
    x86IT_MOV,
    x86IT_LEA,

    x86IT_AND,
    x86IT_OR,

    x86IT_NOT,
    x86IT_INC,
    x86IT_DEC,

    x86IT_IADD,
    x86IT_ISUB,
    x86IT_IMUL,
    x86IT_IDIV,
    x86IT_IMOD,

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
    X86OT_Null,
    x86OT_Register,
    x86OT_Memory,
    x86OT_Constant,
    X86OT_Label,
};

struct BEx86Operand {
    BEx86OperandType type;
    union {
        BEx86Register *reg;
        BESymbol *symbol;
        BEx86Instruction *label;
        BEConstant *constant;
    };
};

struct BEx86Instruction {
    BEx86InstructionType type;
    BEx86Operand operands[2];

    BEx86Instruction *next, *prev;
    void insertAfter(BEx86Instruction *i);
    void insertBefore(BEx86Instruction *i);
    BEx86Instruction* removeAfter();
    BEx86Instruction* removeBefore();
};

class BEx86InstructionBuilder {
public:
    BEx86Instruction* createMOV();
    BEx86Instruction* createLEA();
    BEx86Instruction* createAND();
    BEx86Instruction* createOR();
    BEx86Instruction* createNOT();
    BEx86Instruction* createINC();
    BEx86Instruction* createDEC();
    BEx86Instruction* createIADD();
    BEx86Instruction* createISUB();
    BEx86Instruction* createIMUL();
    BEx86Instruction* createIDIV();
    BEx86Instruction* createIMOD();
    BEx86Instruction* createSAL();
    BEx86Instruction* createSAR();
    BEx86Instruction* createCMP();
    BEx86Instruction* createJMP();
    BEx86Instruction* createJZ();
    BEx86Instruction* createJNZ();
    BEx86Instruction* createJE();
    BEx86Instruction* createJNE();
    BEx86Instruction* createJG();
    BEx86Instruction* createJGE();
    BEx86Instruction* createJL();
    BEx86Instruction* createJLE();
    BEx86Instruction* createNOP();
    BEx86Instruction* createPUSH();
    BEx86Instruction* createPOP();
    BEx86Instruction* createRET();
    BEx86Instruction* createCALL();

private:
};

#endif
