
#include "pch.h"
#include "BEx86FunctionBuilder.h"
#include "BESymbolTable.h"
#include "BEStorage.h"
#include "IDGenerator.h"

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
    m_parent(parent), m_topLocalSymbolTable(NULL), m_argSymbolTable(new BESymbolTable(NULL)) {
    for (int i = 0; i < x86RT_Count; ++i) m_registers.push_back(new BERegister(1 << i));
    m_insFirst = m_insLast = new BEx86Instruction(x86IT_RET);
}

BEx86FunctionBuilder::~BEx86FunctionBuilder() {
    while (m_insFirst->next != NULL) {
        delete m_insFirst->removeAfter();
    }
    delete m_insFirst;

    ASSERT(m_topLocalSymbolTable == NULL);
    for (auto p : m_registers) delete p;
    for (auto p : m_ins2Label) delete p.second;
    delete m_argSymbolTable;
}

BEx86Instruction* BEx86FunctionBuilder::pushInstruction(BEx86Instruction* ins) {
    ASSERT(m_insFirst != NULL);
    if (m_insLast == m_insFirst) {
        m_insLast->insertBefore(ins);
        m_insFirst = ins;
    } else {
        m_insLast->insertBefore(ins);
        m_insLast = ins;
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

void BEx86FunctionBuilder::beginBlock() {
    m_topLocalSymbolTable = new BESymbolTable(m_topLocalSymbolTable);
}
void BEx86FunctionBuilder::endBlock() {
    BESymbolTable *prevTable = m_topLocalSymbolTable->getPrevTable();
    delete m_topLocalSymbolTable;
    m_topLocalSymbolTable = prevTable;
}
BEVariable* BEx86FunctionBuilder::declareLocalVariable(const string &name) {
    return NULL;
}
BEVariable* BEx86FunctionBuilder::declareArgVariable(const string& name) {
    return NULL;
}
BEVariable* BEx86FunctionBuilder::getLocalVariable(const string &name) {
    return NULL;
}
BEVariable* BEx86FunctionBuilder::getGlobalVariable(const string &name) {
    return NULL;
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

BERegister* BEx86FunctionBuilder::getRegister(int regType) {
    return m_registers[regType];
}

BEVariable* BEx86FunctionBuilder::loadConstant(BEConstant *constant) {
    return NULL;
}
void BEx86FunctionBuilder::store(BEVariable *dest, BEVariable *src) {
}

BEVariable* BEx86FunctionBuilder::createAnd(BEVariable *dest, BEVariable *src) {
    return NULL;
}
BEVariable* BEx86FunctionBuilder::createOr(BEVariable *dest, BEVariable *src) {
    return NULL;
}
BEVariable* BEx86FunctionBuilder::createInc(BEVariable *dest) {
    return NULL;
}
BEVariable* BEx86FunctionBuilder::createDec(BEVariable *dest) {
    return NULL;
}
BEVariable* BEx86FunctionBuilder::createNot(BEVariable *dest) {
    return NULL;
}
BEVariable* BEx86FunctionBuilder::createAdd(BEVariable *dest, BEVariable *src) {
    return NULL;
}
BEVariable* BEx86FunctionBuilder::createSub(BEVariable *dest, BEVariable *src) {
    return NULL;
}
BEVariable* BEx86FunctionBuilder::createMul(BEVariable *dest, BEVariable *src) {
    return NULL;
}
BEVariable* BEx86FunctionBuilder::createDiv(BEVariable *dest, BEVariable *src) {
    return NULL;
}
BEVariable* BEx86FunctionBuilder::createMod(BEVariable *dest, BEVariable *src) {
    return NULL;
}

void BEx86FunctionBuilder::createCmp(BEVariable *left, BEVariable *right) {
}
void BEx86FunctionBuilder::createJmp(BEx86Label *label) {
}
void BEx86FunctionBuilder::createJz(BEx86Label *label) {
}
void BEx86FunctionBuilder::createJnz(BEx86Label *label) {
}
void BEx86FunctionBuilder::createJe(BEx86Label *label) {
}
void BEx86FunctionBuilder::createJne(BEx86Label *label) {
}
void BEx86FunctionBuilder::createJg(BEx86Label *label) {
}
void BEx86FunctionBuilder::createJge(BEx86Label *label) {
}
void BEx86FunctionBuilder::createJl(BEx86Label *label) {
}
void BEx86FunctionBuilder::createJle(BEx86Label *label) {
}

BEx86Instruction* BEx86FunctionBuilder::createNop() {
    return NULL;
}

void BEx86FunctionBuilder::createPush(BEVariable *var) {
}
void BEx86FunctionBuilder::beginCall(int n) {
}
void BEx86FunctionBuilder::endCall(int n) {
}

void BEx86FunctionBuilder::createRet() {
}
