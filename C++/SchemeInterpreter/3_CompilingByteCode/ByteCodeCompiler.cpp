#include "pch.h"
#include "ByteCodeCompiler.h"
#include "ByteCodeDefinition.h"

class ASTNodeVisitor_ByteCodeCompiler: public IASTVisitor {
public:
    ASTNodeVisitor_ByteCodeCompiler(
            SScriptFunctionProto *parent,
            ASTNode_Lambda *lambda, 
            vector<SScriptFunctionProtoPtr> *protos):
        mProtos(protos) {

        mProto = make_shared<SScriptFunctionProto>(parent);
        mProto->formalCount = lambda->formalCount;
        mProto->locals = lambda->locals;

        mTailFlags.push_back(true);
        lambda->body->acceptVisitor(this);
        mTailFlags.pop_back();

        mProtos->push_back(mProto);
    }

    virtual void visit(ASTNode_Literal *node) {
        emit(ByteCode_LoadLiteral(node->index));
    }

    virtual void visit(ASTNode_If *node) {
        mTailFlags.push_back(false);
        node->predNode->acceptVisitor(this);
        mTailFlags.pop_back();

        int jmp2Then = preEmit<ByteCode_TrueJmp>();

        mTailFlags.push_back(true);
        node->elseNode->acceptVisitor(this);
        mTailFlags.pop_back();
        int jmp2End = preEmit<ByteCode_Jmp>();

        postEmit(jmp2Then, ByteCode_TrueJmp(currentBytesOff()));
        mTailFlags.push_back(true);
        node->thenNode->acceptVisitor(this);
        mTailFlags.pop_back();

        postEmit(jmp2End, ByteCode_Jmp(currentBytesOff()));
    }

    virtual void visit(ASTNode_Lambda *node) {
        compileToByteCode(mProto.get(), node, mProtos);

        emit(ByteCode_LoadLambda((int)mProtos->size() - 1));
    }

    virtual void visit(ASTNode_Begin *node) {
        for (int i = 0; i < (int)node->nodes.size() - 1; ++i) {
            mTailFlags.push_back(false);
            node->nodes[i]->acceptVisitor(this);
            mTailFlags.pop_back();

            emit(ByteCode_Pop());
        }

        mTailFlags.push_back(true);
        node->nodes.back()->acceptVisitor(this);
        mTailFlags.pop_back();
    }

    virtual void visit(ASTNode_GetVar *node) {
        if (node->address.isLocal()) {
            emit(ByteCode_LoadLocal(node->address.getVarIndex()));
        } else if (node->address.isFree()) {
            int freeIndex = getFreeIndex(node->address);
            emit(ByteCode_LoadFree(freeIndex));
        } else {
            ASSERT(node->address.isGlobal());
            emit(ByteCode_LoadGlobal(node->address.getVarIndex()));
        }
    }

    virtual void visit(ASTNode_SetVar *node) {
        mTailFlags.push_back(false);
        node->rightNode->acceptVisitor(this);
        mTailFlags.pop_back();

        if (node->address.isLocal()) {
            emit(ByteCode_StoreLocal(node->address.getVarIndex()));
        } else if (node->address.isFree()) {
            int freeIndex = getFreeIndex(node->address);
            emit(ByteCode_StoreFree(freeIndex));
        } else {
            ASSERT(node->address.isGlobal());
            emit(ByteCode_StoreGlobal(node->address.getVarIndex()));
        }

        emit(ByteCode_LoadLiteral(0));
    }

    virtual void visit(ASTNode_Application *node) {
        mTailFlags.push_back(false);
        node->func->acceptVisitor(this);
        mTailFlags.pop_back();

        for (auto n : node->actuals) {
            mTailFlags.push_back(false);
            n->acceptVisitor(this);
            mTailFlags.pop_back();
        }

        if (all_of(mTailFlags.begin(), mTailFlags.end(), [](bool b){ return b;})) {
            emit(ByteCode_Tail());
        }

        emit(ByteCode_Call((int)node->actuals.size()));
    }

private:
    template<typename ByteCodeT>
    void emit(ByteCodeT v) {
        int off = preEmit<ByteCodeT>();
        postEmit(off, v);
    }

    template<typename ByteCodeT>
    int preEmit() {
        int off = (int)mProto->bytes.size();
        mProto->bytes.resize(off + sizeof(ByteCodeT));
        return off;
    }

    template<typename ByteCodeT>
    void postEmit(int off, ByteCodeT v) {
        memcpy(&mProto->bytes[off], &v, sizeof(v));
    }

    int currentBytesOff() const {
        return (int)mProto->bytes.size();
    }

    int getFreeIndex(VarAddress address) {
        ASSERT(address.isFree());

        auto &freeAddresses = mProto->freeAddresses;
        auto iter = find(freeAddresses.begin(), freeAddresses.end(), address);
        if (iter == freeAddresses.end()) {
            iter = freeAddresses.insert(iter, address);
        }

        return int(iter - freeAddresses.begin());
    }

private:
    SScriptFunctionProtoPtr mProto;
    vector<SScriptFunctionProtoPtr> *mProtos;
    vector<bool> mTailFlags;
};

void compileToByteCode(
        SScriptFunctionProto* parent,
        ASTNode_Lambda *lambda, 
        vector<SScriptFunctionProtoPtr> *protos) {

    ASTNodeVisitor_ByteCodeCompiler compiler(parent, lambda, protos);
}
