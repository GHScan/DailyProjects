
#include "pch.h"
#include "BEx86CodeOptimizer.h"
#include "BEx86FileBuilder.h"
#include "BEx86FunctionBuilder.h"
#include "BESymbolTable.h"

class x86CodeOptimizer_DeadCodeCleaner {
public:
    x86CodeOptimizer_DeadCodeCleaner(BEx86FunctionBuilder *builder): m_builder(builder) {
        for (int pass = 0; pass < 2; ++pass) {
            redirectJmpTarget();
            removeDeadInstructions();
            removeTailJmp();
        }
    }
private:
    void redirectJmpTarget() {
        for (auto block : m_builder->getBasicBlocks()) {
            for (auto &ins : block->instructions) {
                switch (ins.type) {
                    case x86IT_JMP: ins.operands[0].basicBlock = getRedirectedBasicBlock(ins.operands[0].basicBlock); break;
                    case x86IT_JE: ins.operands[0].basicBlock = getRedirectedBasicBlock(ins.operands[0].basicBlock); break;
                    case x86IT_JNE: ins.operands[0].basicBlock = getRedirectedBasicBlock(ins.operands[0].basicBlock); break;
                    case x86IT_JG: ins.operands[0].basicBlock = getRedirectedBasicBlock(ins.operands[0].basicBlock); break;
                    case x86IT_JGE: ins.operands[0].basicBlock = getRedirectedBasicBlock(ins.operands[0].basicBlock); break;
                    case x86IT_JL: ins.operands[0].basicBlock = getRedirectedBasicBlock(ins.operands[0].basicBlock); break;
                    case x86IT_JLE: ins.operands[0].basicBlock = getRedirectedBasicBlock(ins.operands[0].basicBlock); break;
                    default: break;
                }
            }
        }
    }
    void removeDeadInstructions() {
        set<BEx86BasicBlock*> reachedBasicBlocks;
        set<BEx86Instruction*> activeIns;
        maskActiveInstructionForBasicBlock(m_builder->getBasicBlocks()[0], reachedBasicBlocks, activeIns);

        vector<BEx86BasicBlock*> newBasicBlocks;
        for (auto block : m_builder->getBasicBlocks()) {
            if (reachedBasicBlocks.count(block) == 0 || block->instructions.size() == 0) {
                delete block;
                continue;
            } 
            newBasicBlocks.push_back(block);

            vector<BEx86Instruction> newIns;
            for (auto &ins : block->instructions) {
                if (activeIns.count(&ins) > 0) {
                    newIns.push_back(ins);
                }
            }
            block->instructions = newIns;
        }
        ASSERT(newBasicBlocks.size() > 0);
        m_builder->getBasicBlocks() = newBasicBlocks;
    }
    void removeTailJmp() {
        auto& basicBlocks = m_builder->getBasicBlocks();
        for (int i = 0; i < (int)basicBlocks.size() - 1; ++i) {
            BEx86BasicBlock *basicBlock = basicBlocks[i];
            auto& ins = basicBlock->instructions;
            if (!ins.empty() && ins.back().type == x86IT_JMP && ins.back().operands[0].basicBlock == basicBlocks[i + 1]) {
                ins.pop_back();
            }
        }
    }
private:
    BEx86BasicBlock* getRedirectedBasicBlock(BEx86BasicBlock *basicBlock) {
        for (;;) {
            auto ins = getFirstValidInstruction(basicBlock);
            if (ins == NULL) basicBlock = findNextBasicBlock(basicBlock);
            else {
                if (ins->type == x86IT_JMP) basicBlock = ins->operands[0].basicBlock;
                else break;
            }
        }
        return basicBlock;
    }
    BEx86BasicBlock* findNextBasicBlock(BEx86BasicBlock *basicBlock) {
        auto& basicBlocks = m_builder->getBasicBlocks();
        auto iter = find(basicBlocks.begin(), basicBlocks.end(), basicBlock);
        ASSERT(iter != basicBlocks.end());
        ++iter;
        return iter == basicBlocks.end() ? NULL : *iter;
    }
    BEx86Instruction* getFirstValidInstruction(BEx86BasicBlock *basicBlock) {
        int i = 0;
        for (; i < (int)basicBlock->instructions.size() && basicBlock->instructions[i].type == x86IT_NOP; ++i);
        return i < (int)basicBlock->instructions.size() ? &basicBlock->instructions[i] : NULL;
    }
    void maskActiveInstructionForBasicBlock(BEx86BasicBlock *basicBlock, set<BEx86BasicBlock*> &reachedBasicBlocks, set<BEx86Instruction*> &activeIns) {
        if (basicBlock == NULL) return;
        if (reachedBasicBlocks.count(basicBlock) > 0) return;
        reachedBasicBlocks.insert(basicBlock);

        int i = 0;
        for (; i < (int)basicBlock->instructions.size(); ++i) {
            BEx86Instruction* ins = &basicBlock->instructions[i];

            if (ins->type != x86IT_NOP) {
                activeIns.insert(ins);
            }

            switch (ins->type) {
                case x86IT_JMP:
                    maskActiveInstructionForBasicBlock(ins->operands[0].basicBlock, reachedBasicBlocks, activeIns);
                    goto label_endFor;
                    break;
                case x86IT_JE:
                case x86IT_JNE:
                case x86IT_JG:
                case x86IT_JGE:
                case x86IT_JL:
                case x86IT_JLE:
                    maskActiveInstructionForBasicBlock(ins->operands[0].basicBlock, reachedBasicBlocks, activeIns);
                    break;
                default: break;
            }
        }
label_endFor:

        if (i == (int)basicBlock->instructions.size()) {
            maskActiveInstructionForBasicBlock(findNextBasicBlock(basicBlock), reachedBasicBlocks, activeIns);
        }
    }
private:
    BEx86FunctionBuilder *m_builder;
};

void optimizex86Code(BEx86FileBuilder *builder, int optTypeFlag) {
    for (auto symbol : builder->getGlobalSymbolTable()->getSymbols()) {
        BEx86FunctionBuilder *funcBuilder = builder->getFunctionBuilder(symbol->name);
        if (funcBuilder == NULL) continue;

        for (int i = 0; i < x86IOT_Count; ++i) {
            if (((1 << i) & optTypeFlag) == 0) continue;
            switch (1 << i) {
                case x86IOT_DeadCode: (x86CodeOptimizer_DeadCodeCleaner(funcBuilder)); break;
                default: ASSERT(0); break;
            }
        }
    }
}
