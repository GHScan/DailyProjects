
#include "pch.h"
#include "BEx86FileBuilder.h"
#include "BEx86FunctionBuilder.h"
#include "BESymbolTable.h"
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
    m_parent(parent), m_topLocalSymbolTable(NULL), m_argSymbolTable(NULL) {
    m_insFirst = m_insLast = new BEx86Instruction(x86IT_RET);
    for (int i = 0; i < x86RT_Count; ++i) m_registers.push_back(new BERegister(1 << i));

    pushBlock();
    m_argSymbolTable = m_topLocalSymbolTable;
}

BEx86FunctionBuilder::~BEx86FunctionBuilder() {
    for (auto p : m_ins2Label) delete p.second;

    popBlock();
    ASSERT(m_topLocalSymbolTable == NULL);
    m_argSymbolTable = NULL;

    for (auto p : m_registers) delete p;

    while (m_insFirst->next != NULL) {
        delete m_insFirst->removeAfter();
    }
    delete m_insFirst;
}

BEx86Instruction* BEx86FunctionBuilder::pushInstruction(BEx86Instruction* ins) {
    ASSERT(m_insFirst != NULL);
    if (m_insLast == m_insFirst) {
        m_insFirst->insertBefore(ins);
        m_insFirst = ins;
    } else {
        m_insLast->insertBefore(ins);
    }
    return ins;
}

BEx86Label* BEx86FunctionBuilder::createLabel(const string &labelName) {
    BEx86Instruction* ins = pushInstruction(new BEx86Instruction(x86IT_NOP));
    BEx86Label *label = new BEx86Label(IDGenerator::instance()->generateName(labelName), ins);
    m_ins2Label[ins] = label;
    return label;
}
BEx86Label* BEx86FunctionBuilder::createLabel(const string &labelName, BEx86Instruction* ins) {
    BEx86Label *label = new BEx86Label(IDGenerator::instance()->generateName(labelName), ins);
    m_ins2Label[ins] = label;
    return label;
}

void BEx86FunctionBuilder::pushBlock() {
    m_topLocalSymbolTable = new BESymbolTable(m_topLocalSymbolTable);
}
void BEx86FunctionBuilder::popBlock() {
    for (auto symbol : m_topLocalSymbolTable->getSymbols()) {
        m_leftValueVars.erase(symbol);
    }

    BESymbolTable *prevTable = m_topLocalSymbolTable->getPrevTable();
    delete m_topLocalSymbolTable;
    m_topLocalSymbolTable = prevTable;
}
BEVariablePtr BEx86FunctionBuilder::declareLocalVariable(const string &name, BEType *type) {
    BESymbol* symbol = m_topLocalSymbolTable->declare(name, type);
    return m_leftValueVars[symbol] = BEVariablePtr(new BELeftValueVariable(symbol));
}
BEVariablePtr BEx86FunctionBuilder::declareArgVariable(const string& name, BEType *type) {
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
        reg->loadedVars.insert(var);
        var->reg = reg;
        var->placeFlag |= BEVariable::PF_InRegister;
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
    dest->reg = src;
    dest->placeFlag = BEVariable::PF_InRegister;
    src->loadedVars.insert(dest);
}
void BEx86FunctionBuilder::loadVariableToRegister(BERegister *reg, BEVariable *var) {
    if (var->reg == reg) return;
    makeRegisterFree(reg);
    if (var->placeFlag & BEVariable::PF_InRegister) {
        pushInstruction(new BEx86Instruction(x86IT_MOV, reg, var->reg));
    } else {
        pushInstruction(new BEx86Instruction(x86IT_MOV, reg, var->getValidAddress()));
    }
    reg->loadedVars.insert(var);
    var->reg = reg;
    var->placeFlag |= BEVariable::PF_InRegister;
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
    return BEVariablePtr(new BERightValueVariable(constant->type, reg, m_topLocalSymbolTable));
}
void BEx86FunctionBuilder::store(BEVariablePtr dest, BEVariablePtr src) {
    if ((dest->placeFlag & BEVariable::PF_InRegister) && (src->placeFlag & BEVariable::PF_InRegister) && dest->reg == src->reg) {
        ASSERT(0); 
        return;
    }
    makesureVariableInRegister(src.get());
    storeVariableFromRegister(dest.get(), src->reg);
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
BEVariablePtr BEx86FunctionBuilder::createBinaryOp(BEx86InstructionType insType, BEVariablePtr &dest, BEVariablePtr src) {
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
    return createBinaryOp(x86IT_AND, dest, src);
}
BEVariablePtr BEx86FunctionBuilder::createOr(BEVariablePtr &dest, BEVariablePtr src) {
    return createBinaryOp(x86IT_OR, dest, src);
}
BEVariablePtr BEx86FunctionBuilder::createAdd(BEVariablePtr &dest, BEVariablePtr src) {
    return createBinaryOp(x86IT_ADD, dest, src);
}
BEVariablePtr BEx86FunctionBuilder::createSub(BEVariablePtr &dest, BEVariablePtr src) {
    return createBinaryOp(x86IT_SUB, dest, src);
}
BEVariablePtr BEx86FunctionBuilder::createMul(BEVariablePtr &dest, BEVariablePtr src) {
    return createBinaryOp(x86IT_MUL, dest, src);
}
BEVariablePtr BEx86FunctionBuilder::createDiv(BEVariablePtr &dest, BEVariablePtr src) {
    return createBinaryOp(x86IT_DIV, dest, src);
}
BEVariablePtr BEx86FunctionBuilder::createMod(BEVariablePtr &dest, BEVariablePtr src) {
    return createBinaryOp(x86IT_MOD, dest, src);
}

void BEx86FunctionBuilder::createCmp(BEVariablePtr &left, BEVariablePtr right) {
    makesureVariableInRegister(left.get());
    if (right->placeFlag & BEVariable::PF_InRegister) {
        pushInstruction(new BEx86Instruction(x86IT_CMP, left->reg, right->reg));
    } else {
        pushInstruction(new BEx86Instruction(x86IT_CMP, left->reg, right->getValidAddress()));
    }
}
void BEx86FunctionBuilder::createJmp(BEx86Label *label) {
    pushInstruction(new BEx86Instruction(x86IT_JMP, label));
}
void BEx86FunctionBuilder::createJz(BEx86Label *label) {
    pushInstruction(new BEx86Instruction(x86IT_JZ, label));
}
void BEx86FunctionBuilder::createJnz(BEx86Label *label) {
    pushInstruction(new BEx86Instruction(x86IT_JNZ, label));
}
void BEx86FunctionBuilder::createJe(BEx86Label *label) {
    pushInstruction(new BEx86Instruction(x86IT_JE, label));
}
void BEx86FunctionBuilder::createJne(BEx86Label *label) {
    pushInstruction(new BEx86Instruction(x86IT_JNE, label));
}
void BEx86FunctionBuilder::createJg(BEx86Label *label) {
    pushInstruction(new BEx86Instruction(x86IT_JG, label));
}
void BEx86FunctionBuilder::createJge(BEx86Label *label) {
    pushInstruction(new BEx86Instruction(x86IT_JGE, label));
}
void BEx86FunctionBuilder::createJl(BEx86Label *label) {
    pushInstruction(new BEx86Instruction(x86IT_JL, label));
}
void BEx86FunctionBuilder::createJle(BEx86Label *label) {
    pushInstruction(new BEx86Instruction(x86IT_JLE, label));
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
    pushInstruction(new BEx86Instruction(x86IT_RET));
}
void BEx86FunctionBuilder::createRet(BEVariablePtr dest) {
    loadVariableToRegister(m_registers[x86RT_EAX], dest.get());
    pushInstruction(new BEx86Instruction(x86IT_RET));
}
