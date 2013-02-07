
#include "pch.h"
#include "AST.h"
#include "SymbolTable.h"
#include "TypeSystem.h"
#include "ByteCode.h"
#include "Runtime.h"

class ConstantPool
{
public:
    static ConstantPool* instance()
    {
        static ConstantPool s_ins;
        return &s_ins;
    }

    int cacheString(const string& s)
    {
        auto iter = m_str2ID.lower_bound(s);
        if (iter == m_str2ID.end() || iter->first != s) {
            iter = m_str2ID.insert(iter, pair<string, int>(s, m_id2Data.size()));
            m_id2Data.push_back(m_data.size());
            auto oldSize = m_data.size();
            m_data.resize(oldSize + s.size() + 1);
            strcpy(&m_data[oldSize], s.c_str());
        }
        return iter->second;
    }
    int cacheInt(int i)
    {
        auto iter = m_int2ID.lower_bound(i);
        if (iter == m_int2ID.end() || iter->first != i) {
            iter = m_int2ID.insert(iter, pair<int, int>(i, m_id2Data.size()));
            m_id2Data.push_back(i);
        }
        return iter->second;
    }

    const char* getString(int id)
    {
        return &m_data[m_id2Data[id]];
    }
    int getInt(int id)
    {
        return m_id2Data[id];
    }
private:
    map<string, int> m_str2ID;
    map<int, int> m_int2ID;
    vector<int> m_id2Data;
    vector<char> m_data;
};

#include "ByteCodeDefine.h"

//==============================
class ExpNodeVisitor_CodeGen:
    public IExpNodeVisitor
{
public:
    ExpNodeVisitor_CodeGen(vector<int>& codes, ExpNodePtr exp):
        m_codes(codes), m_lval(false), m_type(NULL), m_addrOff(0)
    {
        exp->acceptVisitor(this);
    }
private:
    virtual void visit(ExpNode_ConstantInt* node)
    {
        if (isLargeInt(node->value)) {
            m_codes.push_back(ByteCode_SizeIndepend<BCT_PushIntLarge>::emit(node->value));
        }
        else m_codes.push_back(ByteCode_SizeIndepend<BCT_PushInt>::emit(node->value));
        m_type = TypeSystem::instance()->getType("int");
        m_lval = false;
    }
    virtual void visit(ExpNode_ConstantLiteral* node)
    {
        m_codes.push_back(ByteCode_SizeIndepend<BCT_PushLiteral>::emit(node->str));
        m_type = TypeSystem::instance()->getPointer(TypeSystem::instance()->getType("char"));
        m_lval = false;
    }
    virtual void visit(ExpNode_Variable* node)
    {
        if (auto symbol = SymbolTableManager::instance()->stack()->getSymbol(node->name)) {
            m_type = symbol->type;
            m_codes.push_back(ByteCode_SizeIndepend<BCT_PushLocalAddr>::emit(symbol->off));
        }
        else {
            if (auto symbol = SymbolTableManager::instance()->global()->getSymbol(node->name)) {
                m_type = symbol->type;
                m_codes.push_back(ByteCode_SizeIndepend<BCT_PushGlobalAddr>::emit(symbol->off));
            }
            else ASSERT(0);
        }
        m_lval = true;
        m_addrOff = 0;
    }
    virtual void visit(ExpNode_Conversion* node)
    {
        node->left->acceptVisitor(this);
        forceConvertTo(node->type);
    }
    virtual void visit(ExpNode_BinaryOp* node)
    {
        if (node->op == ExpNode_BinaryOp::BO_And || 
                node->op == ExpNode_BinaryOp::BO_Or) {
            /*
            exp1
            push -1
            jumpZ (for "And", or jumpN for "Or")
            pop 1
            exp2
l_end:
             * */
            node->left->acceptVisitor(this);
            toRval();
            tryImplicitConvertTo(TypeSystem::instance()->getType("int"));
            ASSERT(m_type == TypeSystem::instance()->getType("int"));
            m_codes.push_back(ByteCode_SizeDepend<BCT_PushI, 4>::emit(-1));

            int jump = m_codes.size();
            m_codes.push_back(0);

            m_codes.push_back(ByteCode_SizeDepend<BCT_PopN, 4>::emit(1));
            {
                ExpNodeVisitor_CodeGen codeGen(m_codes, node->right);
                codeGen.toRval();
                codeGen.tryImplicitConvertTo(TypeSystem::instance()->getType("int"));
                ASSERT(codeGen.m_type == TypeSystem::instance()->getType("int"));
            }

            if (node->op == ExpNode_BinaryOp::BO_And) {
                m_codes[jump] = ByteCode_SizeIndepend<BCT_JumpZ>::emit(m_codes.size());
            }
            else {
                m_codes[jump] = ByteCode_SizeIndepend<BCT_JumpN>::emit(m_codes.size());
            }
            return;
        }

        node->left->acceptVisitor(this);
        toRval();

        ExpNodeVisitor_CodeGen rcodeGen(m_codes, node->right);
        rcodeGen.toRval();
        tryImplicitConvert(&rcodeGen);

        if (auto ptype = dynamic_cast<PointerType*>(m_type)) {
            if (auto ptyp2 = dynamic_cast<PointerType*>(rcodeGen.m_type)) {
                ASSERT(node->op == ExpNode_BinaryOp::BO_Less ||
                        node->op == ExpNode_BinaryOp::BO_LessEq ||
                        node->op == ExpNode_BinaryOp::BO_Equal ||
                        node->op == ExpNode_BinaryOp::BO_NotEqual ||
                        node->op == ExpNode_BinaryOp::BO_Greater ||
                        node->op == ExpNode_BinaryOp::BO_GreaterEq);
            }
            else {
                if (node->op == ExpNode_BinaryOp::BO_Add || 
                        node->op == ExpNode_BinaryOp::BO_Sub) {
                    rcodeGen.m_codes.push_back(ByteCode_SizeIndepend<BCT_PushInt>::emit(ptype->refType->getSize()));
                    rcodeGen.m_codes.push_back(ByteCode_SizeDepend<BCT_Mul, 4>::emit());
                }
                else ASSERT(0);
            }
        }
        else {
            ASSERT(m_type == rcodeGen.m_type);
        }

        switch (node->op) {
            case ExpNode_BinaryOp::BO_Add:
                m_codes.push_back(m_type->getSize() == 1 ?
                        ByteCode_SizeDepend<BCT_Add, 1>::emit() : ByteCode_SizeDepend<BCT_Add, 4>::emit());
                break;
            case ExpNode_BinaryOp::BO_Sub:
                m_codes.push_back(m_type->getSize() == 1 ?
                        ByteCode_SizeDepend<BCT_Sub, 1>::emit() : ByteCode_SizeDepend<BCT_Sub, 4>::emit());
                break;
            case ExpNode_BinaryOp::BO_Mul:
                m_codes.push_back(m_type->getSize() == 1 ?
                        ByteCode_SizeDepend<BCT_Mul, 1>::emit() : ByteCode_SizeDepend<BCT_Mul, 4>::emit());
                break;
            case ExpNode_BinaryOp::BO_Div:
                m_codes.push_back(m_type->getSize() == 1 ?
                        ByteCode_SizeDepend<BCT_Div, 1>::emit() : ByteCode_SizeDepend<BCT_Div, 4>::emit());
                break;
            case ExpNode_BinaryOp::BO_Mod:
                m_codes.push_back(m_type->getSize() == 1 ?
                        ByteCode_SizeDepend<BCT_Mod, 1>::emit() : ByteCode_SizeDepend<BCT_Mod, 4>::emit());
                break;

            case ExpNode_BinaryOp::BO_Less:
                m_codes.push_back(m_type->getSize() == 1 ?
                        ByteCode_SizeDepend<BCT_Less, 1>::emit() : ByteCode_SizeDepend<BCT_Less, 4>::emit());
                m_type = TypeSystem::instance()->getType("int");
                break;
            case ExpNode_BinaryOp::BO_LessEq:
                m_codes.push_back(m_type->getSize() == 1 ?
                        ByteCode_SizeDepend<BCT_LessEq, 1>::emit() : ByteCode_SizeDepend<BCT_LessEq, 4>::emit());
                m_type = TypeSystem::instance()->getType("int");
                break;
            case ExpNode_BinaryOp::BO_Equal:
                m_codes.push_back(m_type->getSize() == 1 ?
                        ByteCode_SizeDepend<BCT_Equal, 1>::emit() : ByteCode_SizeDepend<BCT_Equal, 4>::emit());
                m_type = TypeSystem::instance()->getType("int");
                break;
            case ExpNode_BinaryOp::BO_NotEqual:
                m_codes.push_back(m_type->getSize() == 1 ?
                        ByteCode_SizeDepend<BCT_NotEqual, 1>::emit() : ByteCode_SizeDepend<BCT_NotEqual, 4>::emit());
                m_type = TypeSystem::instance()->getType("int");
                break;
            case ExpNode_BinaryOp::BO_Greater:
                m_codes.push_back(m_type->getSize() == 1 ?
                        ByteCode_SizeDepend<BCT_Greater, 1>::emit() : ByteCode_SizeDepend<BCT_Greater, 4>::emit());
                m_type = TypeSystem::instance()->getType("int");
                break;
            case ExpNode_BinaryOp::BO_GreaterEq:
                m_codes.push_back(m_type->getSize() == 1 ?
                        ByteCode_SizeDepend<BCT_GreaterEq, 1>::emit() : ByteCode_SizeDepend<BCT_GreaterEq, 4>::emit());
                m_type = TypeSystem::instance()->getType("int");
                break;

            default:
                ASSERT(0);
                break;
        }
    }
    virtual void visit(ExpNode_UnaryOp* node)
    {
        if (node->op == ExpNode_UnaryOp::UO_Not) {
            node->left->acceptVisitor(this);
            toRval();
            m_codes.push_back(m_type->getSize() == 1 ?
                    ByteCode_SizeDepend<BCT_Not, 1>::emit() :
                    ByteCode_SizeDepend<BCT_Not, 4>::emit());
            m_type = TypeSystem::instance()->getType("int");
        }
        else if (node->op == ExpNode_UnaryOp::UO_Inc) {
            node->left->acceptVisitor(this);
            ASSERT(m_lval);
            if (auto ptype = dynamic_cast<PointerType*>(m_type)) {
                m_codes.push_back(ByteCode_SizeDepend<BCT_PushI, 4>::emit(-1));
                m_codes.push_back(ByteCode_SizeDepend<BCT_PushI, 4>::emit(-1));
                m_codes.push_back(ByteCode_SizeDepend<BCT_ReadAddr, 4>::emit());
                m_codes.push_back(ByteCode_SizeIndepend<BCT_PushInt>::emit(ptype->refType->getSize()));
                m_codes.push_back(ByteCode_SizeDepend<BCT_Add, 4>::emit());
                m_codes.push_back(ByteCode_SizeDepend<BCT_WriteAddr, 4>::emit());
            }
            else {
                m_codes.push_back(m_type->getSize() == 1 ?
                        ByteCode_SizeDepend<BCT_Inc, 1>::emit() :
                        ByteCode_SizeDepend<BCT_Inc, 4>::emit());
            }
        }
        else if (node->op == ExpNode_UnaryOp::UO_Dec) {
            node->left->acceptVisitor(this);
            ASSERT(m_lval);
            if (auto ptype = dynamic_cast<PointerType*>(m_type)) {
                m_codes.push_back(ByteCode_SizeDepend<BCT_PushI, 4>::emit(-1));
                m_codes.push_back(ByteCode_SizeDepend<BCT_PushI, 4>::emit(-1));
                m_codes.push_back(ByteCode_SizeDepend<BCT_ReadAddr, 4>::emit());
                m_codes.push_back(ByteCode_SizeIndepend<BCT_PushInt>::emit(ptype->refType->getSize()));
                m_codes.push_back(ByteCode_SizeDepend<BCT_Sub, 4>::emit());
                m_codes.push_back(ByteCode_SizeDepend<BCT_WriteAddr, 4>::emit());
            }
            else {
                m_codes.push_back(m_type->getSize() == 1 ?
                        ByteCode_SizeDepend<BCT_Dec, 1>::emit() :
                        ByteCode_SizeDepend<BCT_Dec, 4>::emit());
            }
        }
    }
    virtual void visit(ExpNode_Addr* node)
    {
        node->left->acceptVisitor(this);
        ASSERT(m_lval);
        clearAddrOff();
        m_lval = false;
        m_type = TypeSystem::instance()->getPointer(m_type);
    }
    virtual void visit(ExpNode_Unref* node)
    {
        node->left->acceptVisitor(this);
        toRval();
        m_lval = true;
        m_type = dynamic_cast<PointerType*>(m_type)->refType;
    }
    virtual void visit(ExpNode_Field* node)
    {
        node->left->acceptVisitor(this);
        ASSERT(m_lval);
        if (node->dot) {
            auto stype = dynamic_cast<StructType*>(m_type);
            ASSERT(stype != NULL);
            auto table = SymbolTableManager::instance()->getTypeTable(stype);
            auto symbol = table->getSymbol(node->fieldName);
            m_addrOff += symbol->off;
            m_type = symbol->type;
        }
        else {
            auto ptype = dynamic_cast<PointerType*>(m_type);
            ASSERT(ptype != NULL);
            auto stype = dynamic_cast<StructType*>(ptype->refType);
            ASSERT(stype != NULL);
            toRval();
            auto table = SymbolTableManager::instance()->getTypeTable(stype);
            auto symbol = table->getSymbol(node->fieldName);
            m_codes.push_back(ByteCode_SizeIndepend<BCT_PushInt>::emit(symbol->off));
            m_codes.push_back(ByteCode_SizeDepend<BCT_Add, 4>::emit());
            m_lval = true;
            m_type = symbol->type;
        }
    }
    virtual void visit(ExpNode_ArrayElem* node)
    {
        node->left->acceptVisitor(this);

        if (auto ptype = dynamic_cast<PointerType*>(m_type)) {
            toRval();

            if (auto p = dynamic_cast<ExpNode_ConstantInt*>(node->right.get())) {
                m_addrOff += p->value * ptype->refType->getSize();
            }
            else {
                clearAddrOff();

                ExpNodeVisitor_CodeGen rcodeGen(m_codes, node->right);
                rcodeGen.toRval();
                rcodeGen.tryImplicitConvertTo(TypeSystem::instance()->getType("int"));
                ASSERT(rcodeGen.m_type == TypeSystem::instance()->getType("int"));
                rcodeGen.m_codes.push_back(ByteCode_SizeIndepend<BCT_PushInt>::emit(ptype->refType->getSize()));
                rcodeGen.m_codes.push_back(ByteCode_SizeDepend<BCT_Mul, 4>::emit());

                m_codes.push_back(ByteCode_SizeDepend<BCT_Add, 4>::emit());
            }

            m_lval = true;
            m_type = ptype->refType;
            return;
        }

        auto atype = dynamic_cast<ArrayType*>(m_type);
        ASSERT(atype != NULL);
        ASSERT(m_lval);

        if (auto p = dynamic_cast<ExpNode_ConstantInt*>(node->right.get())) {
            m_addrOff += p->value * atype->elemType->getSize();
        }
        else {
            clearAddrOff();

            ExpNodeVisitor_CodeGen rcodeGen(m_codes, node->right);
            rcodeGen.toRval();
            rcodeGen.tryImplicitConvertTo(TypeSystem::instance()->getType("int"));
            ASSERT(rcodeGen.m_type == TypeSystem::instance()->getType("int"));
            rcodeGen.m_codes.push_back(ByteCode_SizeIndepend<BCT_PushInt>::emit(atype->elemType->getSize()));
            rcodeGen.m_codes.push_back(ByteCode_SizeDepend<BCT_Mul, 4>::emit());

            m_codes.push_back(ByteCode_SizeDepend<BCT_Add, 4>::emit());
        }

        m_type = atype->elemType;
    }
    virtual void visit(ExpNode_Call* node)
    {
        auto symbol = SymbolTableManager::instance()->global()->getSymbol(node->name);
        ASSERT(symbol != NULL);
        auto ftype = dynamic_cast<FunctionType*>(symbol->type);
        int retArgSize = ftype->retT->getSize();
        m_codes.push_back(ByteCode_SizeIndepend<BCT_PushInt>::emit(0));
        if (retArgSize != 4) {
            ASSERT(retArgSize == 1);
            m_codes.push_back(ByteCode_SizeIndepend<BCT_Convert4To1>::emit());
        }

        if (ftype->isVarLengOfArg) {
            ASSERT(node->args.size() >= ftype->argsT.size());
        }
        else ASSERT(node->args.size() == ftype->argsT.size());

        for (int i = 0; i < node->args.size(); ++i) {
            ExpNodeVisitor_CodeGen codeGen(m_codes, node->args[i]);
            codeGen.toRval();
            if (i < ftype->argsT.size()) {
                codeGen.tryImplicitConvertTo(ftype->argsT[i]);
            }
            retArgSize += codeGen.m_type->getSize();
        }

        m_codes.push_back(ByteCode_SizeIndepend<BCT_Call>::emit(
                    node->name, ftype->retT->getSize(), retArgSize));
        m_type = ftype->retT;
        m_lval = false;
        m_addrOff = 0;
    }
    virtual void visit(ExpNode_Assign* node) 
    {
        node->left->acceptVisitor(this);
        ASSERT(m_lval);
        clearAddrOff();
        m_codes.push_back(ByteCode_SizeDepend<BCT_PushI, 4>::emit(-1));

        ExpNodeVisitor_CodeGen rcodeGen(m_codes, node->right);
        rcodeGen.toRval();
        rcodeGen.tryImplicitConvertTo(m_type);
        ASSERT(m_type == rcodeGen.m_type);

        m_codes.push_back(m_type->getSize() == 1 ? 
                ByteCode_SizeDepend<BCT_WriteAddr, 1>::emit() :
                ByteCode_SizeDepend<BCT_WriteAddr, 4>::emit());
    }
    virtual void visit(ExpNode_Sizeof* node)
    {
        vector<int> codes;
        ExpNodeVisitor_CodeGen v(codes, node->left);
        m_codes.push_back(ByteCode_SizeIndepend<BCT_PushInt>::emit(v.m_type->getSize()));
        m_type = TypeSystem::instance()->getType("int");
        m_lval = false;
    }
public:
    IType* getType() { return m_type;}
    void clearAddrOff()
    {
        if (m_lval && m_addrOff > 0) {
            m_codes.push_back(ByteCode_SizeIndepend<BCT_PushInt>::emit(m_addrOff));
            m_codes.push_back(ByteCode_SizeDepend<BCT_Add, 4>::emit());
            m_addrOff = 0;
        }
    }
    void toRval()
    {
        if (m_lval) {
            clearAddrOff();
            if (auto atype = dynamic_cast<ArrayType*>(m_type)) {
                m_type = atype->elemType;
                while (atype = dynamic_cast<ArrayType*>(m_type)) {
                    m_type = atype->elemType;
                }
                m_type = TypeSystem::instance()->getPointer(m_type);
            }
            else {
                m_codes.push_back(m_type->getSize() == 1 ?
                        ByteCode_SizeDepend<BCT_ReadAddr, 1>::emit() :
                        ByteCode_SizeDepend<BCT_ReadAddr, 4>::emit());
            }
            m_lval = false;
        }
    }
    void tryImplicitConvert(ExpNodeVisitor_CodeGen *o)
    {
        if (m_type == o->m_type) return;
        tryImplicitConvertTo(o->m_type);
        o->tryImplicitConvertTo(m_type);
    }
    void tryImplicitConvertTo(IType *type)
    {
        if (type == TypeSystem::instance()->getType("int") &&
                m_type == TypeSystem::instance()->getType("char")) {
            forceConvertTo(type);
        }
    }
    void forceConvertTo(IType *type)
    {
        toRval();
        if (type->getSize() == 1 && m_type->getSize() == 4) {
            m_codes.push_back(ByteCode_SizeIndepend<BCT_Convert4To1>::emit());
        }
        else if (type->getSize() == 4 && m_type->getSize() == 1) {
            m_codes.push_back(ByteCode_SizeIndepend<BCT_Convert1To4>::emit());
        }
        m_type = type;
    }
    void popValue()
    {
        m_codes.push_back(m_type->getSize() == 1 ?
                ByteCode_SizeDepend<BCT_PopN, 1>::emit(1) :
                ByteCode_SizeDepend<BCT_PopN, 4>::emit(1));
        m_type = NULL;
        m_lval = false;
        m_addrOff = 0;
    }
private:
    vector<int> &m_codes;
    bool m_lval;
    int m_addrOff;
    IType *m_type;
};

class StmtNodeVisitor_CodeGen:
    public IStmtNodeVisitor
{
public:
    StmtNodeVisitor_CodeGen(vector<int>& codes, StmtNodePtr stmt):
        m_codes(codes), m_symbolMaxOff(0)
    {
        stmt->acceptVisitor(this);

        for (auto off : m_retJumps) {
            m_codes[off] = ByteCode_SizeIndepend<BCT_Jump>::emit(m_codes.size());
        }
        m_retJumps.clear();
    }
    int getSymbolMaxOff() { return m_symbolMaxOff; }
private:
    virtual void visit(StmtNode_Exp* node) 
    {
        ExpNodeVisitor_CodeGen codeGen(m_codes, node->exp);
        codeGen.popValue();
    }
    virtual void visit(StmtNode_Block* node) 
    {
        auto tableStack = SymbolTableManager::instance()->stack();
        tableStack->push();
        for (auto stmt : node->stmts) {
            stmt->acceptVisitor(this);
        }
        tableStack->pop();
    }
    virtual void visit(StmtNode_DefineLocal* node) 
    {
        auto tableStack = SymbolTableManager::instance()->stack();
        tableStack->addSymbol(node->name, node->type);
        m_symbolMaxOff = max(m_symbolMaxOff, tableStack->getOffset());
    }
    virtual void visit(StmtNode_Break* node) 
    {
        m_breakJumps.push_back(m_codes.size());
        m_codes.push_back(0);
    }
    virtual void visit(StmtNode_Continue* node) 
    {
        m_continueJumps.push_back(m_codes.size());
        m_codes.push_back(0);
    }
    virtual void visit(StmtNode_Return* node) 
    {
        m_retJumps.push_back(m_codes.size());
        m_codes.push_back(0);
    }
    virtual void visit(StmtNode_For* node) 
    {
        /*
         exp1
         popvalue

l_loop:
         exp2
         jumpZ l_break
         body
l_continue:
         exp3
         popvalue
         jump l_loop
l_break:
         * */
        if (node->exp1 != NULL) {
            ExpNodeVisitor_CodeGen codeGen(m_codes, node->exp1);
            codeGen.popValue();
        }

        int lloop = (int)m_codes.size();
        if (node->exp2 != NULL) {
            ExpNodeVisitor_CodeGen codeGen(m_codes, node->exp2);
            codeGen.toRval();
            codeGen.tryImplicitConvertTo(TypeSystem::instance()->getType("int"));
            ASSERT(codeGen.getType() == TypeSystem::instance()->getType("int"));
        }
        else m_codes.push_back(ByteCode_SizeIndepend<BCT_PushInt>::emit(1));

        int jumpZ = m_codes.size();
        m_codes.push_back(0);

        if (node->body != NULL) node->body->acceptVisitor(this);

        int lcontinue = m_codes.size();
        for (auto off : m_continueJumps) {
            m_codes[off] = ByteCode_SizeIndepend<BCT_Jump>::emit(lcontinue);
        }
        m_continueJumps.clear();

        if (node->exp3 != NULL) {
            ExpNodeVisitor_CodeGen codeGen(m_codes, node->exp3);
            codeGen.popValue();
        }

        m_codes.push_back(ByteCode_SizeIndepend<BCT_Jump>::emit(lloop));

        m_codes[jumpZ] = ByteCode_SizeIndepend<BCT_JumpZ>::emit(m_codes.size());
        for (auto off : m_breakJumps) {
            m_codes[off] = ByteCode_SizeIndepend<BCT_Jump>::emit(m_codes.size());
        }
        m_breakJumps.clear();
    }
    virtual void visit(StmtNode_IfElse* node) 
    {
        /*
         exp
         jumpZ l1
         ifStmt
         jump l2
l1:
         elseStmt
l2:
         * */
        {
            ExpNodeVisitor_CodeGen codeGen(m_codes, node->exp);
            codeGen.toRval();
            codeGen.tryImplicitConvertTo(TypeSystem::instance()->getType("int"));
            ASSERT(codeGen.getType() == TypeSystem::instance()->getType("int"));
        }

        int jumpZ = (int)m_codes.size();
        m_codes.push_back(0);

        if (node->ifStmt != NULL) {
            node->ifStmt->acceptVisitor(this);
        }

        int jump = m_codes.size();
        m_codes.push_back(0);

        m_codes[jumpZ] = ByteCode_SizeIndepend<BCT_JumpZ>::emit(m_codes.size());

        if (node->elseStmt != NULL) {
            node->elseStmt->acceptVisitor(this);
        }

        m_codes[jump] = ByteCode_SizeIndepend<BCT_Jump>::emit(m_codes.size());
    }
    virtual void visit(StmtNode_Switch* node) 
    {
        /*
         exp
         jump l_cmp
l_case1:
        stmt1
        jump l_end
l_case2:
        stmt2
        jump l_end
        ...
l_caseN:
        stmtN
        jump l_end
l_cmp:
        push -1
        push v1
        equal
        jumpN l_case1
        push -1
        push v2
        equal
        jumpN l_case2
        ...
        defaultStmt
l_end:
        popvalue
         * */
        {
            ExpNodeVisitor_CodeGen codeGen(m_codes, node->exp);
            codeGen.toRval();
            codeGen.tryImplicitConvertTo(TypeSystem::instance()->getType("int"));
            ASSERT(codeGen.getType() == TypeSystem::instance()->getType("int"));
        }
        int jumpCmp = m_codes.size();
        m_codes.push_back(0);

        vector<int> lcases;
        vector<int> jumpEnds;
        for (auto p : node->caseMap) {
            lcases.push_back(m_codes.size());
            p.second->acceptVisitor(this);
            jumpEnds.push_back(m_codes.size());
            m_codes.push_back(0);
        }

        m_codes[jumpCmp] = ByteCode_SizeIndepend<BCT_Jump>::emit(m_codes.size());

        int idx = 0;
        for (auto p : node->caseMap) {
            m_codes.push_back(ByteCode_SizeDepend<BCT_PushI, 4>::emit(-1));
            if (isLargeInt(p.first)) {
                m_codes.push_back(ByteCode_SizeIndepend<BCT_PushIntLarge>::emit(p.first));
            }
            else {
                m_codes.push_back(ByteCode_SizeIndepend<BCT_PushInt>::emit(p.first));
            }
            m_codes.push_back(ByteCode_SizeDepend<BCT_Equal, 4>::emit());
            m_codes.push_back(ByteCode_SizeIndepend<BCT_JumpN>::emit(lcases[idx]));
            ++idx;
        }

        if (node->defaultStmt != NULL) {
            node->defaultStmt->acceptVisitor(this);
        }

        for (auto off : jumpEnds) {
            m_codes[off] = ByteCode_SizeIndepend<BCT_Jump>::emit(m_codes.size());
        }
        m_codes.push_back(ByteCode_SizeDepend<BCT_PopN, 4>::emit(1));
    }
private:
    vector<int> &m_codes;
    vector<int> m_retJumps;
    vector<int> m_continueJumps;
    vector<int> m_breakJumps;
    int m_symbolMaxOff;
};

ByteCodeSeq::ByteCodeSeq(StmtNodePtr node):
    m_frameSize(0)
{
    m_frameSize = StmtNodeVisitor_CodeGen(m_codes, node).getSymbolMaxOff();
}
ByteCodeSeq::~ByteCodeSeq()
{
}

template<int bits>
struct SizeDependByteCodeDispatcher
{
    static void disassemble(int code, ostream &so)
    {
        switch ((code >> 24) & 0x7f) {
            case BCT_ReadAddr: ByteCode_SizeDepend<BCT_ReadAddr, bits>::disassemble(code, so); break;
            case BCT_WriteAddr: ByteCode_SizeDepend<BCT_WriteAddr, bits>::disassemble(code, so); break;
            case BCT_PushLocal: ByteCode_SizeDepend<BCT_PushLocal, bits>::disassemble(code, so); break;
            case BCT_PushGlobal: ByteCode_SizeDepend<BCT_PushGlobal, bits>::disassemble(code, so); break;
            case BCT_PopLocal: ByteCode_SizeDepend<BCT_PopLocal, bits>::disassemble(code, so); break;
            case BCT_PopGlobal: ByteCode_SizeDepend<BCT_PopGlobal, bits>::disassemble(code, so); break;
            case BCT_PushI: ByteCode_SizeDepend<BCT_PushI, bits>::disassemble(code, so); break;
            case BCT_PopN: ByteCode_SizeDepend<BCT_PopN, bits>::disassemble(code, so); break;
            case BCT_Add: ByteCode_SizeDepend<BCT_Add, bits>::disassemble(code, so); break;
            case BCT_Sub: ByteCode_SizeDepend<BCT_Sub, bits>::disassemble(code, so); break;
            case BCT_Mul: ByteCode_SizeDepend<BCT_Mul, bits>::disassemble(code, so); break;
            case BCT_Div: ByteCode_SizeDepend<BCT_Div, bits>::disassemble(code, so); break;
            case BCT_Mod: ByteCode_SizeDepend<BCT_Mod, bits>::disassemble(code, so); break;
            case BCT_Inc: ByteCode_SizeDepend<BCT_Inc, bits>::disassemble(code, so); break;
            case BCT_Dec: ByteCode_SizeDepend<BCT_Dec, bits>::disassemble(code, so); break;
            case BCT_Less: ByteCode_SizeDepend<BCT_Less, bits>::disassemble(code, so); break;
            case BCT_LessEq: ByteCode_SizeDepend<BCT_LessEq, bits>::disassemble(code, so); break;
            case BCT_Equal: ByteCode_SizeDepend<BCT_Equal, bits>::disassemble(code, so); break;
            case BCT_NotEqual: ByteCode_SizeDepend<BCT_NotEqual, bits>::disassemble(code, so); break;
            case BCT_Greater: ByteCode_SizeDepend<BCT_Greater, bits>::disassemble(code, so); break;
            case BCT_GreaterEq: ByteCode_SizeDepend<BCT_GreaterEq, bits>::disassemble(code, so); break;
            case BCT_Not: ByteCode_SizeDepend<BCT_Not, bits>::disassemble(code, so); break;
            default: ASSERT(0); break;
        }
    }
    static void execute(int code, RuntimeEnv *env)
    {
        switch ((code >> 24) & 0x7f) {
            case BCT_ReadAddr: ByteCode_SizeDepend<BCT_ReadAddr, bits>::execute(code, env); break;
            case BCT_WriteAddr: ByteCode_SizeDepend<BCT_WriteAddr, bits>::execute(code, env); break;
            case BCT_PushLocal: ByteCode_SizeDepend<BCT_PushLocal, bits>::execute(code, env); break;
            case BCT_PushGlobal: ByteCode_SizeDepend<BCT_PushGlobal, bits>::execute(code, env); break;
            case BCT_PopLocal: ByteCode_SizeDepend<BCT_PopLocal, bits>::execute(code, env); break;
            case BCT_PopGlobal: ByteCode_SizeDepend<BCT_PopGlobal, bits>::execute(code, env); break;
            case BCT_PushI: ByteCode_SizeDepend<BCT_PushI, bits>::execute(code, env); break;
            case BCT_PopN: ByteCode_SizeDepend<BCT_PopN, bits>::execute(code, env); break;
            case BCT_Add: ByteCode_SizeDepend<BCT_Add, bits>::execute(code, env); break;
            case BCT_Sub: ByteCode_SizeDepend<BCT_Sub, bits>::execute(code, env); break;
            case BCT_Mul: ByteCode_SizeDepend<BCT_Mul, bits>::execute(code, env); break;
            case BCT_Div: ByteCode_SizeDepend<BCT_Div, bits>::execute(code, env); break;
            case BCT_Mod: ByteCode_SizeDepend<BCT_Mod, bits>::execute(code, env); break;
            case BCT_Inc: ByteCode_SizeDepend<BCT_Inc, bits>::execute(code, env); break;
            case BCT_Dec: ByteCode_SizeDepend<BCT_Dec, bits>::execute(code, env); break;
            case BCT_Less: ByteCode_SizeDepend<BCT_Less, bits>::execute(code, env); break;
            case BCT_LessEq: ByteCode_SizeDepend<BCT_LessEq, bits>::execute(code, env); break;
            case BCT_Equal: ByteCode_SizeDepend<BCT_Equal, bits>::execute(code, env); break;
            case BCT_NotEqual: ByteCode_SizeDepend<BCT_NotEqual, bits>::execute(code, env); break;
            case BCT_Greater: ByteCode_SizeDepend<BCT_Greater, bits>::execute(code, env); break;
            case BCT_GreaterEq: ByteCode_SizeDepend<BCT_GreaterEq, bits>::execute(code, env); break;
            case BCT_Not: ByteCode_SizeDepend<BCT_Not, bits>::execute(code, env); break;
            default: ASSERT(0); break;
        }
    }
};

void ByteCodeSeq::disassemble(ostream& so)
{
    for (int i = 0; i < (int)m_codes.size(); ++i) {
        int code = m_codes[i];
        switch ((code >> 24) & 0xff) {
            case BCT_Convert4To1: ByteCode_SizeIndepend<BCT_Convert4To1>::disassemble(code, so); break;
            case BCT_Convert1To4: ByteCode_SizeIndepend<BCT_Convert1To4>::disassemble(code, so); break;
            case BCT_PushInt: ByteCode_SizeIndepend<BCT_PushInt>::disassemble(code, so); break;
            case BCT_PushIntLarge: ByteCode_SizeIndepend<BCT_PushIntLarge>::disassemble(code, so); break;
            case BCT_PushLiteral: ByteCode_SizeIndepend<BCT_PushLiteral>::disassemble(code, so); break;
            case BCT_Jump: ByteCode_SizeIndepend<BCT_Jump>::disassemble(code, so); break;
            case BCT_JumpZ: ByteCode_SizeIndepend<BCT_JumpZ>::disassemble(code, so); break;
            case BCT_JumpN: ByteCode_SizeIndepend<BCT_JumpN>::disassemble(code, so); break;
            case BCT_Call: ByteCode_SizeIndepend<BCT_Call>::disassemble(code, so); break;
            case BCT_PushLocalAddr: ByteCode_SizeIndepend<BCT_PushLocalAddr>::disassemble(code, so); break;
            case BCT_PushGlobalAddr: ByteCode_SizeIndepend<BCT_PushGlobalAddr>::disassemble(code, so); break;
            default: {
                     if (code >> 31) SizeDependByteCodeDispatcher<4>::disassemble(code, so);
                     else SizeDependByteCodeDispatcher<1>::disassemble(code, so);
                     break;
                 }
        }
    }
}
void ByteCodeSeq::execute(RuntimeEnv *env)
{
    while (env->pc < m_codes.size()) {
        int code = m_codes[env->pc];
        switch ((code >> 24) & 0xff) {
            case BCT_Convert4To1: ByteCode_SizeIndepend<BCT_Convert4To1>::execute(code, env); break;
            case BCT_Convert1To4: ByteCode_SizeIndepend<BCT_Convert1To4>::execute(code, env); break;
            case BCT_PushInt: ByteCode_SizeIndepend<BCT_PushInt>::execute(code, env); break;
            case BCT_PushIntLarge: ByteCode_SizeIndepend<BCT_PushIntLarge>::execute(code, env); break;
            case BCT_PushLiteral: ByteCode_SizeIndepend<BCT_PushLiteral>::execute(code, env); break;
            case BCT_Jump: ByteCode_SizeIndepend<BCT_Jump>::execute(code, env); break;
            case BCT_JumpZ: ByteCode_SizeIndepend<BCT_JumpZ>::execute(code, env); break;
            case BCT_JumpN: ByteCode_SizeIndepend<BCT_JumpN>::execute(code, env); break;
            case BCT_Call: ByteCode_SizeIndepend<BCT_Call>::execute(code, env); break;
            case BCT_PushLocalAddr: ByteCode_SizeIndepend<BCT_PushLocalAddr>::execute(code, env); break;
            case BCT_PushGlobalAddr: ByteCode_SizeIndepend<BCT_PushGlobalAddr>::execute(code, env); break;
            default: {
                     if (code >> 31) SizeDependByteCodeDispatcher<4>::execute(code, env);
                     else SizeDependByteCodeDispatcher<1>::execute(code, env);
                     break;
                 }
        }
        ++env->pc;
    }
}
