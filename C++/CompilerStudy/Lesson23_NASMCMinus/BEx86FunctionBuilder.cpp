
#include "pch.h"
#include "BEx86FileBuilder.h"
#include "BEx86FunctionBuilder.h"
#include "BESymbolTable.h"
#include "BEType.h"
#include "BEStorage.h"
#include "IDGenerator.h"
#include "BEConstant.h"

void BEx86Instruction::insertAfter(BEx86Instruction *i) {
    if (next != NULL) {
        next->prev = i; i->next = next;
    }
    i->prev = this; next = i;
}
void BEx86Instruction::insertBefore(BEx86Instruction *i) {
    if (prev != NULL) {
        prev->next = i; i->prev = prev;
    }
    i->next = this; prev = i;
}
BEx86Instruction* BEx86Instruction::removeAfter() {
    BEx86Instruction* i = next;
    ASSERT(i != NULL);
    if (i->next != NULL) i->next->prev = this;
    next = i->next;
    return i;
}
BEx86Instruction* BEx86Instruction::removeBefore() {
    BEx86Instruction *i = prev;
    ASSERT(i != NULL);
    if (i->prev != NULL) i->prev->next = this;
    prev = i->prev;
    return i;
}
//==============================
BEx86FunctionBuilder::BEx86FunctionBuilder(BEx86FileBuilder *parent): 
    m_parent(parent), m_topLocalSymbolTable(NULL), m_argSymbolTable(NULL), m_maxArgOff(0), m_maxLocalOff(0) {
    m_insFirst = m_insLast = new BEx86Instruction(x86IT_RET);
    m_retLabel = createLabel("ret");

    for (int i = 0; i < x86RT_Count; ++i) m_registers.push_back(new BERegister(1 << i));

    pushBlock();
    m_argSymbolTable = m_topLocalSymbolTable;
}

BEx86FunctionBuilder::~BEx86FunctionBuilder() {
    for (auto p : m_ins2Label) delete p.second;

    m_maxArgOff = m_argSymbolTable->getMaxEndOff();
    popBlock();
    ASSERT(m_topLocalSymbolTable == NULL);
    m_argSymbolTable = NULL;

    for (auto p : m_registers) delete p;

    while (m_insFirst->next != NULL) {
        delete m_insFirst->removeAfter();
    }
    delete m_insFirst;
}

BEx86Instruction* BEx86FunctionBuilder::insertFrontInstruction(BEx86Instruction* ins) {
    m_insFirst->insertBefore(ins);
    m_insFirst = ins;
    return ins;
}
BEx86Instruction* BEx86FunctionBuilder::pushInstruction(BEx86Instruction* ins) {
    ASSERT(m_insFirst != NULL);
    if (m_insLast == m_insFirst) {
        return insertFrontInstruction(ins);
    } else {
        m_insLast->insertBefore(ins);
        return ins;
    }
}

BEx86Label* BEx86FunctionBuilder::createLabel(const string &labelName) {
    BEx86Label *label = new BEx86Label(IDGenerator::instance()->generateName(labelName));
    return label;
}
void BEx86FunctionBuilder::markLabel(BEx86Label *label, BEx86Instruction *ins) {
    ASSERT(label->ins == NULL);
    m_ins2Label[ins] = label;
    label->ins = ins;
}

void BEx86FunctionBuilder::pushBlock() {
    m_topLocalSymbolTable = new BESymbolTable(m_topLocalSymbolTable);
}
void BEx86FunctionBuilder::popBlock() {
    for (auto symbol : m_topLocalSymbolTable->getSymbols()) {
        m_leftValueVars.erase(symbol);
    }

    m_maxLocalOff = max(m_maxLocalOff, m_topLocalSymbolTable->getMaxEndOff());

    BESymbolTable *prevTable = m_topLocalSymbolTable->getPrevTable();
    delete m_topLocalSymbolTable;
    m_topLocalSymbolTable = prevTable;
}
BEVariablePtr BEx86FunctionBuilder::declareLocalVariable(const string &name, const BEType *type) {
    BESymbol* symbol = m_topLocalSymbolTable->declare(name, type);
    return m_leftValueVars[symbol] = BEVariablePtr(new BELeftValueVariable(symbol));
}
BEVariablePtr BEx86FunctionBuilder::declareArgVariable(const string& name, const BEType *type) {
    BESymbol* symbol = m_argSymbolTable->declare(name, type);
    return m_leftValueVars[symbol] = BEVariablePtr(new BELeftValueVariable(symbol));
}
BEVariablePtr BEx86FunctionBuilder::getLocalVariable(const string &name) {
    auto iter = m_leftValueVars.find(m_topLocalSymbolTable->get(name));
    if (iter != m_leftValueVars.end()) return iter->second;
    return BEVariablePtr();
}
BEVariablePtr BEx86FunctionBuilder::getGlobalVariable(const string &name) {
    BESymbol *symbol = m_parent->getGlobalSymbolTable()->get(name);
    if (symbol == NULL) return BEVariablePtr();
    auto iter = m_leftValueVars.find(symbol);
    if (iter != m_leftValueVars.end()) return iter->second;
    return m_leftValueVars[symbol] = BEVariablePtr(new BELeftValueVariable(symbol));
}

BESymbolTable* BEx86FunctionBuilder::getArgSymbolTable() {
    return m_argSymbolTable;
}
BESymbolTable* BEx86FunctionBuilder::getTopLocalSymbolTable() {
    return m_topLocalSymbolTable;
}

BEx86Instruction* BEx86FunctionBuilder::getFirstInstruction() {
    return m_insFirst;
}
BEx86Instruction* BEx86FunctionBuilder::getLastInstruction() {
    return m_insLast;
}

void BEx86FunctionBuilder::makesureVariableInRegister(BEVariable *var) {
    if ((var->placeFlag & BEVariable::PF_InRegister) == 0) {
        ASSERT(var->placeFlag & BEVariable::PF_InMemory);
        BERegister *reg = getFreeRegister();
        reg->linkVariable(var);
        reg->isWritten = true;
        pushInstruction(new BEx86Instruction(x86IT_MOV, reg, var->getValidAddress()));
    }
}
void BEx86FunctionBuilder::makesureVariableInMemory(BEVariable *var) {
    if ((var->placeFlag & BEVariable::PF_InMemory) == 0) {
        ASSERT(var->placeFlag & BEVariable::PF_InRegister);
        var->placeFlag |= BEVariable::PF_InMemory;
        pushInstruction(new BEx86Instruction(x86IT_MOV, var->getValidAddress(), var->reg));
    }
}
BERegister* BEx86FunctionBuilder::findLeastUseRegister() {
    BERegister *r = NULL;
    for (int i = 0; i < x86RT_GRCount; ++i) {
        BERegister *tr = m_registers[i];
        if (r == NULL || tr->loadedVars.size() < r->loadedVars.size()) r = tr;
    }
    ASSERT(r != NULL);
    return r;
}
BERegister* BEx86FunctionBuilder::makeRegisterFree(BERegister *reg) {
    vector<BEVariable*> vars(reg->loadedVars.begin(), reg->loadedVars.end());
    for (auto var : vars) makeVariableInMemoryOnly(var);
    ASSERT(reg->loadedVars.empty());
    return reg;
}
BERegister* BEx86FunctionBuilder::getFreeRegister() {
    return makeRegisterFree(findLeastUseRegister());
}
void BEx86FunctionBuilder::storeVariableFromRegister(BEVariable *dest, BERegister *src) {
    if (dest->placeFlag & BEVariable::PF_InRegister) {
        if (dest->reg == src) return;
        dest->reg->loadedVars.erase(dest);
    }
    dest->placeFlag = 0;
    src->linkVariable(dest);
}
void BEx86FunctionBuilder::loadVariableToRegister(BERegister *reg, BEVariable *var) {
    if (var->reg == reg) return;
    makeRegisterFree(reg);
    if (var->placeFlag & BEVariable::PF_InRegister) {
        pushInstruction(new BEx86Instruction(x86IT_MOV, reg, var->reg));
    } else {
        pushInstruction(new BEx86Instruction(x86IT_MOV, reg, var->getValidAddress()));
    }
    reg->linkVariable(var);
    reg->isWritten = true;
}
void BEx86FunctionBuilder::makeVariableInMemoryOnly(BEVariable *var) {
    makesureVariableInMemory(var);
    if (var->placeFlag & BEVariable::PF_InRegister) {
        var->reg->loadedVars.erase(var);
        var->reg = NULL;
        var->placeFlag &= ~BEVariable::PF_InRegister;
    }
}
//==============================
BEVariablePtr BEx86FunctionBuilder::loadConstant(BEConstant *constant) {
    BERegister *reg = getFreeRegister();
    pushInstruction(new BEx86Instruction(x86IT_MOV, reg, constant));
    reg->isWritten = true;
    return BEVariablePtr(new BERightValueVariable(constant->type, reg, m_topLocalSymbolTable));
}
BEVariablePtr BEx86FunctionBuilder::store(BEVariablePtr dest, BEVariablePtr src) {
    if ((dest->placeFlag & BEVariable::PF_InRegister) && (src->placeFlag & BEVariable::PF_InRegister) && dest->reg == src->reg) {
        ASSERT(0); 
        return BEVariablePtr();
    }
    makesureVariableInRegister(src.get());
    storeVariableFromRegister(dest.get(), src->reg);
    return dest;
}

BEVariablePtr BEx86FunctionBuilder::createInc(BEVariablePtr &dest) {
    ASSERT(dynamic_cast<BELeftValueVariable*>(dest.get()));
    makeVariableInMemoryOnly(dest.get());
    pushInstruction(new BEx86Instruction(x86IT_INC, dest->getValidAddress()));
    return dest;
}
BEVariablePtr BEx86FunctionBuilder::createDec(BEVariablePtr &dest) {
    ASSERT(dynamic_cast<BELeftValueVariable*>(dest.get()));
    makeVariableInMemoryOnly(dest.get());
    pushInstruction(new BEx86Instruction(x86IT_DEC, dest->getValidAddress()));
    return dest;
}
BEVariablePtr BEx86FunctionBuilder::createNot(BEVariablePtr &dest) {
    makesureVariableInRegister(dest.get());
    BERegister *reg = dest->reg;
    const BEType *type = dest->getType();
    dest.reset();
    makeRegisterFree(reg);
    pushInstruction(new BEx86Instruction(x86IT_NOT, reg));
    return BEVariablePtr(new BERightValueVariable(type, reg, m_topLocalSymbolTable));
}
BEVariablePtr BEx86FunctionBuilder::createArithmeticOp(BEx86InstructionType insType, BEVariablePtr &dest, BEVariablePtr src) {
    makesureVariableInRegister(dest.get());
    BERegister *reg = dest->reg;
    dest.reset();
    makeRegisterFree(reg);
    if (src->placeFlag & BEVariable::PF_InRegister) {
        pushInstruction(new BEx86Instruction(insType, reg, src->reg));
    } else {
        pushInstruction(new BEx86Instruction(insType, reg, src->getValidAddress()));
    }
    return BEVariablePtr(new BERightValueVariable(src->getType(), reg, m_topLocalSymbolTable));
}
BEVariablePtr BEx86FunctionBuilder::createAnd(BEVariablePtr &dest, BEVariablePtr src) {
    return createArithmeticOp(x86IT_AND, dest, src);
}
BEVariablePtr BEx86FunctionBuilder::createOr(BEVariablePtr &dest, BEVariablePtr src) {
    return createArithmeticOp(x86IT_OR, dest, src);
}
BEVariablePtr BEx86FunctionBuilder::createAdd(BEVariablePtr &dest, BEVariablePtr src) {
    return createArithmeticOp(x86IT_ADD, dest, src);
}
BEVariablePtr BEx86FunctionBuilder::createSub(BEVariablePtr &dest, BEVariablePtr src) {
    return createArithmeticOp(x86IT_SUB, dest, src);
}
BEVariablePtr BEx86FunctionBuilder::createMul(BEVariablePtr &dest, BEVariablePtr src) {
    return createArithmeticOp(x86IT_MUL, dest, src);
}
BEVariablePtr BEx86FunctionBuilder::createDiv(BEVariablePtr &dest, BEVariablePtr src) {
    return createArithmeticOp(x86IT_DIV, dest, src);
}
BEVariablePtr BEx86FunctionBuilder::createMod(BEVariablePtr &dest, BEVariablePtr src) {
    return createArithmeticOp(x86IT_MOD, dest, src);
}

BEVariablePtr BEx86FunctionBuilder::createLogicalOp(BEx86InstructionType insType, BEVariablePtr &left, BEVariablePtr right) {
    makesureVariableInRegister(left.get());
    if (right->placeFlag & BEVariable::PF_InRegister) {
        pushInstruction(new BEx86Instruction(x86IT_CMP, left->reg, right->reg));
    } else {
        pushInstruction(new BEx86Instruction(x86IT_CMP, left->reg, right->getValidAddress()));
    }
    left.reset();
    BERegister *reg = getFreeRegister();
    BEx86Label *label_true = createLabel("label_compare_true");
    BEx86Label *label_end = createLabel("label_compare_end");
    pushInstruction(new BEx86Instruction(insType, label_true));
    pushInstruction(new BEx86Instruction(x86IT_MOV, reg, m_parent->getConstantPool()->get(0)));
    pushInstruction(new BEx86Instruction(x86IT_JMP, label_end));
    markLabel(label_true, pushInstruction(new BEx86Instruction(x86IT_MOV, reg, m_parent->getConstantPool()->get(1))));
    markLabel(label_end, pushInstruction(new BEx86Instruction(x86IT_NOP)));
    return BEVariablePtr(new BERightValueVariable(BETypeManager::instance()->getType("int"), reg, m_topLocalSymbolTable));
}
BEVariablePtr BEx86FunctionBuilder::createLt(BEVariablePtr &left, BEVariablePtr right) {
    return createLogicalOp(x86IT_JL, left, right);
}
BEVariablePtr BEx86FunctionBuilder::createLe(BEVariablePtr &left, BEVariablePtr right) {
    return createLogicalOp(x86IT_JLE, left, right);
}
BEVariablePtr BEx86FunctionBuilder::createGt(BEVariablePtr &left, BEVariablePtr right) {
    return createLogicalOp(x86IT_JG, left, right);
}
BEVariablePtr BEx86FunctionBuilder::createGe(BEVariablePtr &left, BEVariablePtr right) {
    return createLogicalOp(x86IT_JGE, left, right);
}
BEVariablePtr BEx86FunctionBuilder::createEq(BEVariablePtr &left, BEVariablePtr right) {
    return createLogicalOp(x86IT_JE, left, right);
}
BEVariablePtr BEx86FunctionBuilder::createNe(BEVariablePtr &left, BEVariablePtr right) {
    return createLogicalOp(x86IT_JNE, left, right);
}
void BEx86FunctionBuilder::createJmp(BEx86Label *label) {
    pushInstruction(new BEx86Instruction(x86IT_JMP, label));
}
void BEx86FunctionBuilder::createCJmp(BEVariablePtr cond, BEx86Label *labelTrue, BEx86Label *labelFalse) {
    makesureVariableInRegister(cond.get());
    pushInstruction(new BEx86Instruction(x86IT_CMP, cond->reg, m_parent->getConstantPool()->get(0)));
    if (labelTrue != NULL && labelFalse != NULL) {
        pushInstruction(new BEx86Instruction(x86IT_JZ, labelFalse));
        pushInstruction(new BEx86Instruction(x86IT_JMP, labelTrue));
    } else if (labelTrue != NULL) {
        pushInstruction(new BEx86Instruction(x86IT_JNZ, labelTrue));
    } else if (labelFalse != NULL) {
        pushInstruction(new BEx86Instruction(x86IT_JZ, labelFalse));
    } else {
        ASSERT(0);
    }
}

BEx86Instruction* BEx86FunctionBuilder::createNop() {
    return pushInstruction(new BEx86Instruction(x86IT_NOP));
}

void BEx86FunctionBuilder::createPush(BEVariablePtr var) {
    if (var->placeFlag & BEVariable::PF_InRegister) {
        pushInstruction(new BEx86Instruction(x86IT_PUSH, var->reg));
    } else {
        pushInstruction(new BEx86Instruction(x86IT_PUSH, var->getValidAddress()));
    }
}
void BEx86FunctionBuilder::beginCall(int n) {
}
void BEx86FunctionBuilder::endCall(int n) {
    pushInstruction(new BEx86Instruction(x86IT_CALL));
}

void BEx86FunctionBuilder::createRet() {
    pushInstruction(new BEx86Instruction(x86IT_JMP, m_retLabel));
}
void BEx86FunctionBuilder::createRet(BEVariablePtr dest) {
    loadVariableToRegister(m_registers[x86RT_EAX], dest.get());
    pushInstruction(new BEx86Instruction(x86IT_JMP, m_retLabel));
}

void BEx86FunctionBuilder::beginBuild() {
}

void BEx86FunctionBuilder::endBuild() {
    if (m_insFirst == m_insLast) return;

    if (m_maxLocalOff > m_maxArgOff) {
        insertFrontInstruction(new BEx86Instruction(x86IT_SUB, m_registers[x86RT_ESP], m_parent->getConstantPool()->get(m_maxLocalOff - m_maxArgOff)));
    }
    insertFrontInstruction(new BEx86Instruction(x86IT_MOV, m_registers[x86RT_EBP], m_registers[x86RT_ESP]));
    insertFrontInstruction(new BEx86Instruction(x86IT_PUSH, m_registers[x86RT_EBP]));
    for (int i = 1; i < x86RT_GRCount; ++i) {
        if (m_registers[i]->isWritten) {
            insertFrontInstruction(new BEx86Instruction(x86IT_PUSH, m_registers[i]));
        }
    }

    BEx86Instruction* retIns = pushInstruction(new BEx86Instruction(x86IT_MOV, m_registers[x86RT_ESP], m_registers[x86RT_EBP]));
    pushInstruction(new BEx86Instruction(x86IT_POP, m_registers[x86RT_EBP]));
    for (int i = 1; i < x86RT_GRCount; ++i) {
        if (m_registers[i]->isWritten) {
            pushInstruction(new BEx86Instruction(x86IT_POP, m_registers[i]));
        }
    }

    markLabel(m_retLabel, retIns);
}
