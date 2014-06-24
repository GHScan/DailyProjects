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

        lambda->body->acceptVisitor(this);

        mProtos->push_back(mProto);
    }

    virtual void visit(ASTNode_Literal *node) {
        emit(ByteCode_LoadLiteral(node->index));
    }

    virtual void visit(ASTNode_If *node) {
        node->predNode->acceptVisitor(this);
        int jmp2Then = preEmit<ByteCode_TrueJmp>();

        node->elseNode->acceptVisitor(this);
        int jmp2End = preEmit<ByteCode_Jmp>();

        postEmit(jmp2Then, ByteCode_TrueJmp(currentBytesOff()));
        node->thenNode->acceptVisitor(this);

        postEmit(jmp2End, ByteCode_Jmp(currentBytesOff()));
    }

    virtual void visit(ASTNode_Lambda *node) {
        compileToByteCode(mProto.get(), node, mProtos);

        emit(ByteCode_LoadLambda((int)mProtos->size() - 1));
    }

    virtual void visit(ASTNode_Begin *node) {
        for (int i = 0; i < (int)node->nodes.size() - 1; ++i) {
            node->nodes[i]->acceptVisitor(this);
            emit(ByteCode_Pop());
        }

        node->nodes.back()->acceptVisitor(this);
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
        node->rightNode->acceptVisitor(this);

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
        node->func->acceptVisitor(this);
        for (auto n : node->actuals) {
            n->acceptVisitor(this);
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
};

void compileToByteCode(
        SScriptFunctionProto* parent,
        ASTNode_Lambda *lambda, 
        vector<SScriptFunctionProtoPtr> *protos) {

    ASTNodeVisitor_ByteCodeCompiler compiler(parent, lambda, protos);
}
