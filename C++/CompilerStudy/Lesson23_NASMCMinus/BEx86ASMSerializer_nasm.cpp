
#include "pch.h"

#include "BEx86ASMSerializer.h"
#include "BEx86FileBuilder.h"
#include "BEx86FunctionBuilder.h"
#include "BEConstant.h"
#include "BESymbolTable.h"
#include "BEType.h"
#include "BEStorage.h"

class BEx86ASMSerializer_nasm {
public:
    BEx86ASMSerializer_nasm(ostream &so, BEx86FileBuilder *builder) {
        serializeExterns(so, builder);
        serializeDataSection(so, builder);
        serializeBssSection(so, builder);
        serializeTextSection(so, builder);
    }
private:
    void serializeExterns(ostream &so, BEx86FileBuilder *builder) {
        so << "; externs\n";

        for (auto symbol : builder->getGlobalSymbolTable()->getSymbols()) {
            if (!builder->isExternSymbol(symbol->name)) continue;
            so << format("extern %s\n", getPlatformDependentSymbolName(symbol).c_str());
        }
    }
    void serializeDataSection(ostream &so, BEx86FileBuilder *builder) {
        so << "\n\nsection .data\n";

        so << "; const strings\n";
        for (auto p : builder->getConstantPool()->getStrings()) {
            so << format("%s : %s `%s`, 0\n", p.second.name.c_str(), getStoragePrefixByType(p.second.type), escape(p.second.str).c_str());
        }

        so << "; globals\n";
        for (auto symbol : builder->getGlobalSymbolTable()->getSymbols()) {
            if (builder->isExternSymbol(symbol->name)) continue;
            if (builder->getFunctionBuilder(symbol->name) != NULL) continue;
            so << format("%s : %s %s\n", getPlatformDependentSymbolName(symbol).c_str(), getStoragePrefixByType(symbol->type), getStorageDefaultValueByType(symbol->type));
        }
    }
    void serializeBssSection(ostream &so, BEx86FileBuilder *builder) {
    }
    void serializeTextSection(ostream &so, BEx86FileBuilder *builder) {
        so << "\n\nsection .text\n";

        so << "; functions\n";
        for (auto symbol : builder->getGlobalSymbolTable()->getSymbols()) {
            if (builder->isExternSymbol(symbol->name)) continue;
            BEx86FunctionBuilder *funcBuilder = builder->getFunctionBuilder(symbol->name);
            if (funcBuilder == NULL) continue;
            if (symbol->name == "main") so << format("global %s\n", getPlatformDependentSymbolName(symbol).c_str());
            so << format("%s : \n", getPlatformDependentSymbolName(symbol).c_str());
            for (auto block : funcBuilder->getBasicBlocks()) {
                so << format(" %s : \n", block->name.c_str());
                for (auto &ins : block->instructions) {
                    string s = serializeInstructionType(ins.type);
                    string comment = s;
                    for (int i = 0; i < 2; ++i) {
                        if (ins.operands[i].type != x86OT_Null) {
                            if (i > 0) s += ", ", comment += ", ";
                            else s += " ", comment += " ";
                            s += serializeInstructionOperand(funcBuilder, ins.operands[i]);
                            comment += serializeInstructionOperandComment(funcBuilder, ins.operands[i]);
                        }
                    }
                    so << "\t" << s << " ;" << comment << "\n";
                }
            }
        }
    }

private:
    const char* serializeInstructionType(BEx86InstructionType type) {
        switch (type) {
            case x86IT_MOV: return "mov";
            case x86IT_LEA: return "lea";
            case x86IT_AND: return "and";
            case x86IT_OR:  return "or";
            case x86IT_NOT: return "not";
            case x86IT_INC: return "inc";
            case x86IT_DEC: return "dec";
            case x86IT_ADD: return "add";
            case x86IT_SUB: return "sub";
            case x86IT_MUL: return "imul";
            case x86IT_DIV: return "idiv";
            case x86IT_SAL: return "sal";
            case x86IT_SAR: return "sar";
            case x86IT_XOR: return "xor";
            case x86IT_CMP: return "cmp";
            case x86IT_JMP: return "jmp";
            case x86IT_JZ:  return "jz";
            case x86IT_JNZ: return "jnz";
            case x86IT_JE:  return "je";
            case x86IT_JNE: return "jne";
            case x86IT_JG:  return "jg";
            case x86IT_JGE: return "jge";
            case x86IT_JL:  return "jl";
            case x86IT_JLE: return "jle";
            case x86IT_NOP: return "nop";
            case x86IT_PUSH:return "push";
            case x86IT_POP: return "pop";
            case x86IT_RET: return "ret";
            case x86IT_CALL:return "call";
            default: ASSERT(0);
        }
        return "";
    }
    string serializeInstructionOperand(BEx86FunctionBuilder *funcBuilder, const BEx86Operand &operand) {
        switch (operand.type) {
            case x86OT_Register: return serializeInstructionOperand_Register(funcBuilder, operand.reg);
            case x86OT_Memory: return serializeInstructionOperand_Memory(funcBuilder, operand.symbol);
            case x86OT_Constant: return serializeInstructionOperand_Constant(funcBuilder, operand.constant);
            case x86OT_BasicBlock: return operand.basicBlock->name;
            default: ASSERT(0);
        }
        return "";
    }
    string serializeInstructionOperandComment(BEx86FunctionBuilder *funcBuilder, const BEx86Operand &operand) {
        switch (operand.type) {
            case x86OT_Register: return serializeInstructionOperand_Register(funcBuilder, operand.reg);
            case x86OT_Memory: return serializeInstructionOperandComment_Memory(funcBuilder, operand.symbol);
            case x86OT_Constant: return serializeInstructionOperand_Constant(funcBuilder, operand.constant);
            case x86OT_BasicBlock: return operand.basicBlock->name;
            default: ASSERT(0);
        }
        return "";
    }
    string serializeInstructionOperand_Register(BEx86FunctionBuilder *funcBuilder, const BERegister* reg) {
        switch (reg->regType) { 
            case x86RT_EAX: return "eax";
            case x86RT_EBX: return "ebx";
            case x86RT_ECX: return "ecx";
            case x86RT_EDX: return "edx";
            case x86RT_ESI: return "esi";
            case x86RT_EDI: return "edi";
            case x86RT_ESP: return "esp";
            case x86RT_EBP: return "ebp";
            default: ASSERT(0);
        }
        return "";
    }
    string serializeInstructionOperand_Memory(BEx86FunctionBuilder *funcBuilder, const BESymbol* symbol) {
        if (symbol->parent == funcBuilder->getArgSymbolTable()) {
            int regInUse = 0; // this calculation can be do once only
            for (int i = 1; i < x86RT_GRCount; ++i) {
                if (funcBuilder->getRegister(i)->isWritten) ++regInUse;
            }
            return format("%s [ebp+%d]", getAddressAccessPrefixByType(symbol->type), (regInUse + 1 + 1) * 4 + symbol->off);
        } else if (symbol->parent == funcBuilder->getParent()->getGlobalSymbolTable()) { 
            if (dynamic_cast<const BEType_Array*>(symbol->type) || symbol->type == BETypeManager::instance()->getFunc()) {
                return getPlatformDependentSymbolName(symbol);
            } else {
                return format("%s [%s]", getAddressAccessPrefixByType(symbol->type), getPlatformDependentSymbolName(symbol).c_str());
            }
        } else {
            int localOff = symbol->off - funcBuilder->getMaxArgOff();
            return format("%s [ebp-%d]", getAddressAccessPrefixByType(symbol->type), localOff + 4);
        }
    }
    string serializeInstructionOperandComment_Memory(BEx86FunctionBuilder *funcBuilder, const BESymbol* symbol) {
        if (symbol->parent == funcBuilder->getArgSymbolTable()) {
            return format("[%s]", getPlatformDependentSymbolName(symbol).c_str());
        } else if (symbol->parent == funcBuilder->getParent()->getGlobalSymbolTable()) { 
            if (dynamic_cast<const BEType_Array*>(symbol->type) || symbol->type == BETypeManager::instance()->getFunc()) {
                return getPlatformDependentSymbolName(symbol);
            } else {
                return format("[%s]", getPlatformDependentSymbolName(symbol).c_str());
            }
        } else {
            return format("[%s]", getPlatformDependentSymbolName(symbol).c_str());
        }
    }
    string serializeInstructionOperand_Constant(BEx86FunctionBuilder *funcBuilder, const BEConstant* constant) {
        if (auto p = dynamic_cast<const BEConstantInt*>(constant)) return format("%d", p->num);
        else if (auto p = dynamic_cast<const BEConstantString*>(constant)) return p->name;
        else ASSERT(0);
    }

    const char* getStoragePrefixByType(const BEType *type) {
        if (auto p = dynamic_cast<const BEType_Array*>(type)) return getStoragePrefixByType(p->elemType);
        switch (type->size) {
            case 1: return "db";
            case 4: return "dw";
            default: ASSERT(0);
        }
        return "";
    }
    const char* getStorageDefaultValueByType(const BEType *type) {
        if (auto p = dynamic_cast<const BEType_Array*>(type)) return getStorageDefaultValueByType(p->elemType);
        if (type == BETypeManager::instance()->get("int")) return "0";
        ASSERT(0);
        return "";
    }
    const char* getAddressAccessPrefixByType(const BEType *type) {
        if (dynamic_cast<const BEType_Array*>(type)) return "dword";
        switch (type->size) {
            case 1: return "byte";
            case 2: return "word";
            case 4: return "dword";
            default:
                ASSERT(0);
        }
        return "";
    }

    string getPlatformDependentSymbolName(const BESymbol *symbol) {
#ifdef _MSC_VER
        return "_" + symbol->name;
#elif __APPLE__
        return "_" + symbol->name;
#else
        return symbol->name;
#endif
    }

private:
};

void serializex86Code_nasm(ostream &so, BEx86FileBuilder *fileBuilder) {
    BEx86ASMSerializer_nasm(so, fileBuilder);
}
