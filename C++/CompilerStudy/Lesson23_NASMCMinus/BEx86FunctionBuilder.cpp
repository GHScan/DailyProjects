
#include "pch.h"
#include "BEx86FileBuilder.h"
#include "BEx86FunctionBuilder.h"
#include "BESymbolTable.h"
#include "BEType.h"
#include "BEStorage.h"
#include "IDGenerator.h"
#include "BEConstant.h"

#define REG_FLAG(regType)  (1 << regType)

BEx86Operand::BEx86Operand(BESymbol *symbol): type(x86OT_Memory) {
    this->symbol = new BESymbol(*symbol);
}
BEx86Operand::~BEx86Operand() {
    switch (type) {
        case x86OT_Memory: delete symbol; break;
        default: break;
    }
}
BEx86Operand::BEx86Operand(const BEx86Operand &o): type(x86OT_Null) {
    *this = o;
}
BEx86Operand& BEx86Operand::operator = (const BEx86Operand &o) {
    if (this == &o) return *this;
    type = o.type;
    reg = o.reg;
    switch (type) {
        case x86OT_Memory: symbol = new BESymbol(*o.symbol); break;
        default: break;
    }
    return *this;
}
//==============================
BEx86FunctionBuilder::BEx86FunctionBuilder(BEx86FileBuilder *parent): 
    m_parent(parent), m_topLocalSymbolTable(NULL), m_argSymbolTable(NULL), m_maxArgOff(0), m_maxLocalOff(0), m_retBasicBlock(NULL) {
    for (int i = 0; i < x86RT_Count; ++i) m_registers.push_back(new BERegister(i));
}

BEx86FunctionBuilder::~BEx86FunctionBuilder() {
    for (auto block : m_basicBlocks) delete block;
    for (auto p : m_registers) delete p;
}

void BEx86FunctionBuilder::beginBuild() {
    beginScope();
    m_argSymbolTable = m_topLocalSymbolTable;
    m_retBasicBlock = createBasicBlock("label_RET");
}

void BEx86FunctionBuilder::endBuild() {
    m_maxArgOff = m_argSymbolTable->getMaxEndOff();
    endScope();
    ASSERT(m_topLocalSymbolTable == NULL);

    for (auto p : m_leftValueVars) {
        ASSERT(p.first->parent == m_parent->getGlobalSymbolTable());
    }
    m_leftValueVars.clear();
    //==============================
    BEx86BasicBlock *entryBasicBlock = createBasicBlock("label_ENTRY");
    int pushedRegCount = 1;
    for (int i = 1; i < x86RT_GRCount; ++i) { 
        if (m_registers[i]->isWritten) {
            entryBasicBlock->instructions.push_back(BEx86Instruction(x86IT_PUSH, m_registers[i]));
            ++pushedRegCount;
        }
    }
    entryBasicBlock->instructions.push_back(BEx86Instruction(x86IT_PUSH, m_registers[x86RT_EBP]));
    entryBasicBlock->instructions.push_back(BEx86Instruction(x86IT_MOV, m_registers[x86RT_EBP], m_registers[x86RT_ESP]));
    int stackFix = getStackAlignmentFix((pushedRegCount + 1) * 4 + m_maxLocalOff - m_maxArgOff);
    if (m_maxLocalOff - m_maxArgOff + stackFix > 0) {
        entryBasicBlock->instructions.push_back(BEx86Instruction(x86IT_SUB, m_registers[x86RT_ESP], m_parent->getConstantPool()->get(m_maxLocalOff - m_maxArgOff + stackFix)));
    }
    m_basicBlocks.insert(m_basicBlocks.begin(), entryBasicBlock);

    pushBasicBlock(m_retBasicBlock);
    m_retBasicBlock = NULL;
    pushInstruction(BEx86Instruction(x86IT_MOV, m_registers[x86RT_ESP], m_registers[x86RT_EBP]));
    pushInstruction(BEx86Instruction(x86IT_POP, m_registers[x86RT_EBP]));
    for (int i = x86RT_GRCount - 1; i > 0; --i) {
        if (m_registers[i]->isWritten) {
            pushInstruction(BEx86Instruction(x86IT_POP, m_registers[i]));
        }
    }
    pushInstruction(BEx86Instruction(x86IT_RET));
}

void BEx86FunctionBuilder::beginScope() {
    m_topLocalSymbolTable = new BESymbolTable(m_topLocalSymbolTable);
}
void BEx86FunctionBuilder::endScope() {
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

BEx86BasicBlock* BEx86FunctionBuilder::createBasicBlock(const string &name) {
    return new BEx86BasicBlock(IDGenerator::instance()->generateName(name));
}
void BEx86FunctionBuilder::pushBasicBlock(BEx86BasicBlock *basicBlock) {
    ASSERT(find(m_basicBlocks.begin(), m_basicBlocks.end(), basicBlock) == m_basicBlocks.end());
    m_basicBlocks.push_back(basicBlock);
}

void BEx86FunctionBuilder::pushInstruction(const BEx86Instruction &ins) {
    m_basicBlocks.back()->instructions.push_back(ins);
}
int BEx86FunctionBuilder::getStackAlignmentFix(int n) {
    int stackAlignment = m_parent->getBuildConfig()->stackAlignment;
    return n % stackAlignment > 0 ? (stackAlignment - n % stackAlignment) : 0;
}

void BEx86FunctionBuilder::makesureVariableInRegister(BEVariable *var) {
    if (!var->isInRegister()) {
        ASSERT(var->isInMemory());
        loadVariableToRegister(findLFURegister(0), var);
    }
}
void BEx86FunctionBuilder::makesureVariableInMemory(BEVariable *var) {
    if (!var->isInMemory()) {
        ASSERT(var->isInRegister());
        var->setMemoryValid();
        pushInstruction(BEx86Instruction(x86IT_MOV, var->getValidAddress(), var->reg));
    }
}
BERegister* BEx86FunctionBuilder::findLFURegister(int excludeRegFlags) {
    BERegister *r = NULL;
    for (int i = 0; i < x86RT_GRCount; ++i) {
        if (REG_FLAG(i) & excludeRegFlags) continue;
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
void BEx86FunctionBuilder::makeAllRegisterFree() {
    for (int i = 0; i < x86RT_GRCount; ++i) makeRegisterFree(m_registers[i]);
}
void BEx86FunctionBuilder::storeVariableFromRegister(BEVariable *dest, BERegister *src) {
    if (dest->isInRegister()) {
        if (dest->reg == src) return;
        dest->reg->unlinkVariable(dest);
    }
    dest->setMemoryDirty();
    src->linkVariable(dest);
}
void BEx86FunctionBuilder::loadVariableToRegister(BERegister *reg, BEVariable *var) {
    if (var->reg == reg) return;
    makeRegisterFree(reg);
    if (var->isInRegister()) {
        pushInstruction(BEx86Instruction(x86IT_MOV, reg, var->reg));
        var->reg->unlinkVariable(var);
    } else {
        if (var->tryGetConstant()) pushInstruction(BEx86Instruction(x86IT_MOV, reg, var->tryGetConstant()));
        else pushInstruction(BEx86Instruction(x86IT_MOV, reg, var->getValidAddress()));
    }
    reg->linkVariable(var);
    reg->isWritten = true;
}
void BEx86FunctionBuilder::makeVariableInMemoryOnly(BEVariable *var) {
    makesureVariableInMemory(var);
    if (var->isInRegister()) {
        var->reg->unlinkVariable(var);
    }
}
//==============================
BEVariablePtr BEx86FunctionBuilder::loadConstant(BEConstant *constant) {
    return BEVariablePtr(new BEConstantProxyVariable(constant));
}
BEVariablePtr BEx86FunctionBuilder::store(BEVariablePtr dest, BEVariablePtr src) {
    if (src->isInRegister() && dest->reg == src->reg) {
        return dest;
    }
    makesureVariableInRegister(src.get());
    storeVariableFromRegister(dest.get(), src->reg);
    return dest;
}

BEVariablePtr BEx86FunctionBuilder::createInc(BEVariablePtr &dest) {
    ASSERT(dynamic_cast<BELeftValueVariable*>(dest.get()));
    if (dest->isInRegister() && dest->reg->isSharedByVariables()) {
        makeVariableInMemoryOnly(dest.get());
    }
    makesureVariableInRegister(dest.get());
    pushInstruction(BEx86Instruction(x86IT_INC, dest->reg));
    dest->setMemoryDirty();
    return dest;
}
BEVariablePtr BEx86FunctionBuilder::createDec(BEVariablePtr &dest) {
    ASSERT(dynamic_cast<BELeftValueVariable*>(dest.get()));
    if (dest->isInRegister() && dest->reg->isSharedByVariables()) {
        makeVariableInMemoryOnly(dest.get());
    }
    makesureVariableInRegister(dest.get());
    pushInstruction(BEx86Instruction(x86IT_DEC, dest->reg));
    dest->setMemoryDirty();
    return dest;
}
BEVariablePtr BEx86FunctionBuilder::createNot(BEVariablePtr &dest) {
    makesureVariableInRegister(dest.get());
    BERegister *reg = dest->reg;
    const BEType *type = dest->getType();
    dest.reset();
    makeRegisterFree(reg);
    pushInstruction(BEx86Instruction(x86IT_NOT, reg));
    return BEVariablePtr(new BERightValueVariable(type, reg, m_topLocalSymbolTable));
}
BEVariablePtr BEx86FunctionBuilder::createArithmeticOp(BEx86InstructionType insType, BEVariablePtr &dest, BEVariablePtr src) {
    makesureVariableInRegister(dest.get());
    BERegister *reg = dest->reg;
    dest.reset();
    makeRegisterFree(reg);
    if (src->isInRegister()) {
        pushInstruction(BEx86Instruction(insType, reg, src->reg));
    } else {
        if (src->tryGetConstant()) pushInstruction(BEx86Instruction(insType, reg, src->tryGetConstant()));
        else pushInstruction(BEx86Instruction(insType, reg, src->getValidAddress()));
    }
    return BEVariablePtr(new BERightValueVariable(src->getType(), reg, m_topLocalSymbolTable));
}
BEVariablePtr BEx86FunctionBuilder::createSal(BEVariablePtr &dest, BEVariablePtr src) {
    return createArithmeticOp(x86IT_SAL, dest, src);
}
BEVariablePtr BEx86FunctionBuilder::createSar(BEVariablePtr &dest, BEVariablePtr src) {
    return createArithmeticOp(x86IT_SAR, dest, src);
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
    if (!src->isInRegister() && src->tryGetConstant()) {
        loadVariableToRegister(findLFURegister(REG_FLAG(x86RT_EAX) | REG_FLAG(x86RT_EDX)), src.get());
    }

    loadVariableToRegister(m_registers[x86RT_EAX], dest.get());
    dest.reset();
    makeRegisterFree(m_registers[x86RT_EDX]);
    pushInstruction(BEx86Instruction(x86IT_XOR, m_registers[x86RT_EDX], m_registers[x86RT_EDX]));
    if (src->isInRegister()) {
        pushInstruction(BEx86Instruction(x86IT_DIV, src->reg));
    } else {
        pushInstruction(BEx86Instruction(x86IT_DIV, src->getValidAddress()));
    }
    return BEVariablePtr(new BERightValueVariable(src->getType(), m_registers[x86RT_EAX], m_topLocalSymbolTable));
}
BEVariablePtr BEx86FunctionBuilder::createMod(BEVariablePtr &dest, BEVariablePtr src) {
    createDiv(dest, src);
    return BEVariablePtr(new BERightValueVariable(src->getType(), m_registers[x86RT_EDX], m_topLocalSymbolTable));
}

BEVariablePtr BEx86FunctionBuilder::createRelativeOp(BEx86InstructionType insType, BEVariablePtr &left, BEVariablePtr right) {
    makesureVariableInRegister(left.get());
    if (right->isInRegister()) {
        pushInstruction(BEx86Instruction(x86IT_CMP, left->reg, right->reg));
    } else {
        if (right->tryGetConstant()) pushInstruction(BEx86Instruction(x86IT_CMP, left->reg, right->tryGetConstant()));
        else pushInstruction(BEx86Instruction(x86IT_CMP, left->reg, right->getValidAddress()));
    }
    left.reset();
    BERegister *reg = findLFURegister(0);
    makeRegisterFree(reg);
    BEx86BasicBlock *trueBlock = createBasicBlock("label_compare_true");
    BEx86BasicBlock *endBlock = createBasicBlock("label_compare_end");
    pushInstruction(BEx86Instruction(insType, trueBlock));
    pushInstruction(BEx86Instruction(x86IT_MOV, reg, m_parent->getConstantPool()->get(0)));
    pushInstruction(BEx86Instruction(x86IT_JMP, endBlock));
    pushBasicBlock(trueBlock);
    pushInstruction(BEx86Instruction(x86IT_MOV, reg, m_parent->getConstantPool()->get(1)));
    pushBasicBlock(endBlock);
    return BEVariablePtr(new BERightValueVariable(BETypeManager::instance()->get("int"), reg, m_topLocalSymbolTable));
}
BEVariablePtr BEx86FunctionBuilder::createLt(BEVariablePtr &left, BEVariablePtr right) {
    return createRelativeOp(x86IT_JL, left, right);
}
BEVariablePtr BEx86FunctionBuilder::createLe(BEVariablePtr &left, BEVariablePtr right) {
    return createRelativeOp(x86IT_JLE, left, right);
}
BEVariablePtr BEx86FunctionBuilder::createGt(BEVariablePtr &left, BEVariablePtr right) {
    return createRelativeOp(x86IT_JG, left, right);
}
BEVariablePtr BEx86FunctionBuilder::createGe(BEVariablePtr &left, BEVariablePtr right) {
    return createRelativeOp(x86IT_JGE, left, right);
}
BEVariablePtr BEx86FunctionBuilder::createEq(BEVariablePtr &left, BEVariablePtr right) {
    return createRelativeOp(x86IT_JE, left, right);
}
BEVariablePtr BEx86FunctionBuilder::createNe(BEVariablePtr &left, BEVariablePtr right) {
    return createRelativeOp(x86IT_JNE, left, right);
}
void BEx86FunctionBuilder::createJmp(BEx86BasicBlock *basicBlock) {
    makeAllRegisterFree();
    pushInstruction(BEx86Instruction(x86IT_JMP, basicBlock));
}
void BEx86FunctionBuilder::createCJmp(BEVariablePtr &cond, BEx86BasicBlock *trueBlock, BEx86BasicBlock *falseBlock) {
    ASSERT(trueBlock != NULL && falseBlock != NULL);
    makesureVariableInRegister(cond.get());
    pushInstruction(BEx86Instruction(x86IT_CMP, cond->reg, m_parent->getConstantPool()->get(0)));
    cond.reset();
    makeAllRegisterFree();
    pushInstruction(BEx86Instruction(x86IT_JZ, falseBlock));
    pushInstruction(BEx86Instruction(x86IT_JMP, trueBlock));
}

void BEx86FunctionBuilder::createPush(BEVariablePtr var) {
    if (var->isInRegister()) {
        pushInstruction(BEx86Instruction(x86IT_PUSH, var->reg));
    } else {
        if (var->tryGetConstant()) pushInstruction(BEx86Instruction(x86IT_PUSH, var->tryGetConstant()));
        else pushInstruction(BEx86Instruction(x86IT_PUSH, var->getValidAddress()));
    }
}
void BEx86FunctionBuilder::beginCall(int n) {
    int stackFix = getStackAlignmentFix(n * 4);
    if (stackFix > 0) {
        pushInstruction(BEx86Instruction(x86IT_SUB, m_registers[x86RT_ESP], m_parent->getConstantPool()->get(stackFix)));
    }
}
BEVariablePtr BEx86FunctionBuilder::endCall(const BEType *type, BESymbol *funcSymbol, int n) {
    BERegister *reg = makeRegisterFree(m_registers[x86RT_EAX]);
    pushInstruction(BEx86Instruction(x86IT_CALL, funcSymbol));
    int stackFix = getStackAlignmentFix(n * 4);
    if (n * 4 + stackFix > 0) {
        pushInstruction(BEx86Instruction(x86IT_ADD, m_registers[x86RT_ESP], m_parent->getConstantPool()->get(n * 4 + stackFix)));
    }
    return BEVariablePtr(new BERightValueVariable(type, reg, m_topLocalSymbolTable));
}

void BEx86FunctionBuilder::createRet() {
    pushInstruction(BEx86Instruction(x86IT_JMP, m_retBasicBlock));
}
void BEx86FunctionBuilder::createRet(BEVariablePtr dest) {
    loadVariableToRegister(m_registers[x86RT_EAX], dest.get());
    pushInstruction(BEx86Instruction(x86IT_JMP, m_retBasicBlock));
}

