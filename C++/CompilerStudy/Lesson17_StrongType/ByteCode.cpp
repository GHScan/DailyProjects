
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

//TODO : consider 64 bits?
enum ByteCodeType
{
    // Size independ
    BCT_Convert4To1,
    BCT_Convert1To4,
    BCT_PushInt,
    BCT_PushIntLarge,
    BCT_PushLiteral,
    BCT_Jump,
    BCT_JumpZ,
    BCT_JumpN,
    BCT_Call,
    BCT_PushLocalAddr,
    BCT_PushGlobalAddr,
    // Size depend
    BCT_ReadAddr,
    BCT_WriteAddr,
    BCT_PushLocal,
    BCT_PushGlobal,
    BCT_PopLocal,
    BCT_PopGlobal,
    BCT_PushI,
    BCT_PopN,
    BCT_Add,
    BCT_Sub,
    BCT_Mul,
    BCT_Div,
    BCT_Mod,
    BCT_Inc,
    BCT_Dec,
    BCT_Less,
    BCT_LessEq,
    BCT_Equal,
    BCT_Greater,
    BCT_GreaterEq,
};
// Size independ
template<int n>
struct ByteCode_SizeIndepend;

template<>
struct ByteCode_SizeIndepend<BCT_Convert4To1>
{
    static int emit()
    {
        return BCT_Convert4To1 << 24;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        int i; env->popValue(i);
        env->pushValue((char)i);
    }
    static void disassemble(ostream& so)
    {
    }
};
template<>
struct ByteCode_SizeIndepend<BCT_Convert1To4>
{
    static bool canImplicitConvert(IType *s, IType *d)
    {
        if (d == TypeSystem::instance()->getType("int") &&
                s == TypeSystem::instance()->getType("char")) {
            return true;
        }
        return false;
    }
    static int emit()
    {
        return BCT_Convert1To4 << 24;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        char c; env->popValue(c);
        env->pushValue((int)c);
    }
    static void disassemble(ostream& so)
    {
    }
};
static bool isLargeInt(int i)
{
    return abs(i) >= (1 << 23);
}
template<>
struct ByteCode_SizeIndepend<BCT_PushInt>
{
    static int emit(int i)
    {
        int ai = abs(i);
        int sig = i > 0 ? 0 : 1;
        assert(ai < (1 << 23));
        return (BCT_PushInt << 24) | (sig << 23) | (ai);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        int i = code & 0x7fffff;
        if ((code >> 23) & 1) {
            env->pushValue(-i);
        }
        else env->pushValue(i);
    }
    static void disassemble(ostream& so)
    {
    }
};
template<>
struct ByteCode_SizeIndepend<BCT_PushIntLarge>
{
    static int emit(int i)
    {
        int id = ConstantPool::instance()->cacheInt(i);
        ASSERT(id < (1 << 24));
        return (BCT_PushIntLarge << 24) | id;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        int i = ConstantPool::instance()->getInt(code & 0xffffff);
        env->pushValue(i);
    }
    static void disassemble(ostream& so)
    {
    }
};
template<>
struct ByteCode_SizeIndepend<BCT_PushLiteral>
{
    static int emit(const string& s)
    {
        int id = ConstantPool::instance()->cacheString(s);
        return (BCT_PushLiteral << 24) | id;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        const char *s = ConstantPool::instance()->getString(code & 0xffffff);
        env->pushValue(s);
    }
    static void disassemble(ostream& so)
    {
    }
};
template<>
struct ByteCode_SizeIndepend<BCT_Jump>
{
    static int emit(int pc)
    {
        ASSERT(pc < (1 << 24));
        return (BCT_Jump << 24) | pc;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        env->pc = code & 0xffffff;
    }
    static void disassemble(ostream& so)
    {
    }
};
template<>
struct ByteCode_SizeIndepend<BCT_JumpZ>
{
    static int emit(int pc)
    {
        ASSERT(pc < (1 << 24));
        return (BCT_JumpZ << 24) | pc;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        int v; env->popValue(v);
        if (!v) env->pc = code & 0xffffff;
    }
    static void disassemble(ostream& so)
    {
    }
};
template<>
struct ByteCode_SizeIndepend<BCT_JumpN>
{
    static int emit(int pc)
    {
        ASSERT(pc < (1 << 24));
        return (BCT_JumpN << 24) | pc;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        int v; env->popValue(v);
        if (v) env->pc = code & 0xffffff;
    }
    static void disassemble(ostream& so)
    {
    }
};
template<>
struct ByteCode_SizeIndepend<BCT_Call>
{
    static int emit(const string& name, int retSize, int retArgSize)
    {
        int nameID = ConstantPool::instance()->cacheString(name);
        int numID = ConstantPool::instance()->cacheInt((retSize << 24) | retArgSize);
        ASSERT(nameID < (1 << 12) && numID < (1 << 12) && retSize < (1 << 8) && retArgSize < (1 << 24));
        return (BCT_Call << 24) | (nameID << 12) | numID;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        const char *name = ConstantPool::instance()->getString((code >> 12) & 0xfff);
        int numID = ConstantPool::instance()->getInt(code & 0xfff);
        int retSize = numID >> 24, retArgSize = numID & 0xffffff;
        env->pushFrame(retArgSize);
        CodeManager::instance()->getFunc(name)->call(env);
        env->popFrame(retSize);
    }
    static void disassemble(ostream& so)
    {
    }
};
template<>
struct ByteCode_SizeIndepend<BCT_PushLocalAddr>
{
    static int emit(int off)
    {
        ASSERT(off < (1 << 24));
        return (BCT_PushLocalAddr << 24) | off;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        env->pushValue(env->frameBase() + (code & 0xffffff));
    }
    static void disassemble(ostream& so)
    {
    }
};
template<>
struct ByteCode_SizeIndepend<BCT_PushGlobalAddr>
{
    static int emit(int off)
    {
        ASSERT(off < (1 << 24));
        return (BCT_PushGlobalAddr << 24) | off;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        env->pushValue(env->globalBase() + (code & 0xffffff));
    }
    static void disassemble(ostream& so)
    {
    }
};
//////////

template<int n>
struct Bytes2Type;
template<>
struct Bytes2Type<1>
{
    typedef char Type;
};
template<>
struct Bytes2Type<4>
{
    typedef int Type;
};

template<int code, int bits>
struct ByteCode_SizeDepend;
template<int bits>
struct ByteCode_SizeDepend<BCT_ReadAddr, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return (BCT_ReadAddr << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type* v; env->popValue(v);
        env->pushValue(*v);
    }
    static void disassemble(ostream& so)
    {
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_WriteAddr, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return (BCT_WriteAddr << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type v; env->popValue(v);
        Type *addr; env->popValue(addr);
        *addr = v;
    }
    static void disassemble(ostream& so)
    {
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_PushLocal, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit(int off)
    {
        ASSERT(off < (1 << 24));
        return (BCT_PushLocal << 24) | off;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        env->pushValue(
                env->localVariable<Type>(code & 0xffffff));
    }
    static void disassemble(ostream& so)
    {
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_PushGlobal, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit(int off)
    {
        ASSERT(off < (1 << 24));
        return (BCT_PushGlobal << 24) | off;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        env->pushValue(
                env->globalVariable<Type>(code & 0xffffff));
    }
    static void disassemble(ostream& so)
    {
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_PopLocal, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit(int off)
    {
        ASSERT(off < (1 << 24));
        return (BCT_PopLocal << 24) | off;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type v; env->popValue(v);
        env->localVariable<Type>(code & 0xffffff) = v;
    }
    static void disassemble(ostream& so)
    {
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_PopGlobal, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit(int off)
    {
        ASSERT(off < (1 << 24));
        return (BCT_PopGlobal << 24) | off;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type v; env->popValue(v);
        env->globalVariable<Type>(code & 0xffffff) = v;
    }
    static void disassemble(ostream& so)
    {
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_PushI, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit(int i)
    {
        assert(i < 0);
        return (BCT_PushI << 24) | -i;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        env->pushValue(env->topValue<Type>(- (code & 0xffffff)));
    }
    static void disassemble(ostream& so)
    {
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_PopN, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit(int n)
    {
        ASSERT(n < (1 << 24));
        return (BCT_PopN << 24) | n;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        int n = code & 0xffffff;
        while (--n >= 0) env->popValue<Type>();
    }
    static void disassemble(ostream& so)
    {
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Add, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return BCT_Add;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type v; env->popValue(v);
        env->topValue<Type>(-1) += v;
    }
    static void disassemble(ostream& so)
    {
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Sub, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return BCT_Sub;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type v; env->popValue(v);
        env->topValue<Type>(-1) -= v;
    }
    static void disassemble(ostream& so)
    {
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Mul, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return BCT_Mul;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type v; env->popValue(v);
        env->topValue<Type>(-1) *= v;
    }
    static void disassemble(ostream& so)
    {
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Div, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return BCT_Div;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type v; env->popValue(v);
        env->topValue<Type>(-1) /= v;
    }
    static void disassemble(ostream& so)
    {
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Mod, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return BCT_Mod;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type v; env->popValue(v);
        env->topValue<Type>(-1) %= v;
    }
    static void disassemble(ostream& so)
    {
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Inc, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return BCT_Inc;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type *v; env->popValue(v);
        ++*v;
    }
    static void disassemble(ostream& so)
    {
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Dec, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return BCT_Dec;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type *v; env->popValue(v);
        --*v;
    }
    static void disassemble(ostream& so)
    {
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Less, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return BCT_Less;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type l, r;
        env->popValue(r);
        env->popValue(l);
        env->pushValue(int(l < r));
    }
    static void disassemble(ostream& so)
    {
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_LessEq, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return BCT_LessEq;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type l, r;
        env->popValue(r);
        env->popValue(l);
        env->pushValue(int(l <= r));
    }
    static void disassemble(ostream& so)
    {
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Equal, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return BCT_Equal;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type l, r;
        env->popValue(r);
        env->popValue(l);
        env->pushValue(int(l == r));
    }
    static void disassemble(ostream& so)
    {
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Greater, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return BCT_Greater;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type l, r;
        env->popValue(r);
        env->popValue(l);
        env->pushValue(int(l > r));
    }
    static void disassemble(ostream& so)
    {
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_GreaterEq, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return BCT_GreaterEq;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type l, r;
        env->popValue(r);
        env->popValue(l);
        env->pushValue(int(l >= r));
    }
    static void disassemble(ostream& so)
    {
    }
};
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
        convertTo(node->type);
    }
    virtual void visit(ExpNode_BinaryOp* node)
    {
        // TODO
        ASSERT(node->op != ExpNode_BinaryOp::BO_And && node->op != ExpNode_BinaryOp::BO_Or);

        node->left->acceptVisitor(this);
        vector<int> codes;
        ExpNodeVisitor_CodeGen rcodeGen(codes, node->right);
        checkConversion(&rcodeGen);

        if (auto ptype = dynamic_cast<PointerType*>(m_type)) {
            if (auto ptyp2 = dynamic_cast<PointerType*>(rcodeGen.m_type)) {
                ASSERT(node->op == ExpNode_BinaryOp::BO_Less ||
                        node->op == ExpNode_BinaryOp::BO_LessEq ||
                        node->op == ExpNode_BinaryOp::BO_Equal ||
                        node->op == ExpNode_BinaryOp::BO_Greater ||
                        node->op == ExpNode_BinaryOp::BO_GreaterEq);
                mergeFrom(&rcodeGen);
            }
            else {
                if (node->op == ExpNode_BinaryOp::BO_Add || 
                        node->op == ExpNode_BinaryOp::BO_Sub) {
                    rcodeGen.m_codes.push_back(ByteCode_SizeIndepend<BCT_PushInt>::emit(ptype->refType->getSize()));
                    rcodeGen.m_codes.push_back(ByteCode_SizeDepend<BCT_Mul, 4>::emit());
                    mergeFrom(&rcodeGen);
                }
                else ASSERT(0);
            }
        }
        else {
            ASSERT(m_type == rcodeGen.m_type);
            mergeFrom(&rcodeGen);
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
                break;
            case ExpNode_BinaryOp::BO_LessEq:
                m_codes.push_back(m_type->getSize() == 1 ?
                        ByteCode_SizeDepend<BCT_LessEq, 1>::emit() : ByteCode_SizeDepend<BCT_LessEq, 4>::emit());
                break;
            case ExpNode_BinaryOp::BO_Equal:
                m_codes.push_back(m_type->getSize() == 1 ?
                        ByteCode_SizeDepend<BCT_Equal, 1>::emit() : ByteCode_SizeDepend<BCT_Equal, 4>::emit());
                break;
            case ExpNode_BinaryOp::BO_Greater:
                m_codes.push_back(m_type->getSize() == 1 ?
                        ByteCode_SizeDepend<BCT_Greater, 1>::emit() : ByteCode_SizeDepend<BCT_Greater, 4>::emit());
                break;
            case ExpNode_BinaryOp::BO_GreaterEq:
                m_codes.push_back(m_type->getSize() == 1 ?
                        ByteCode_SizeDepend<BCT_GreaterEq, 1>::emit() : ByteCode_SizeDepend<BCT_GreaterEq, 4>::emit());
                break;

            default:
                ASSERT(0);
                break;
        }
    }
    virtual void visit(ExpNode_UnaryOp* node)
    {
        // TODO
        ASSERT(node->op != ExpNode_UnaryOp::UO_Not);
        if (node->op == ExpNode_UnaryOp::UO_Inc) {
            node->left->acceptVisitor(this);
            ASSERT(m_lval);
            m_codes.push_back(m_type->getSize() == 1 ?
                    ByteCode_SizeDepend<BCT_Inc, 1>::emit() :
                    ByteCode_SizeDepend<BCT_Inc, 4>::emit());
        }
        else if (node->op == ExpNode_UnaryOp::UO_Dec) {
            node->left->acceptVisitor(this);
            ASSERT(m_lval);
            m_codes.push_back(m_type->getSize() == 1 ?
                    ByteCode_SizeDepend<BCT_Dec, 1>::emit() :
                    ByteCode_SizeDepend<BCT_Dec, 4>::emit());
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
        ASSERT(m_lval);
        auto atype = dynamic_cast<ArrayType*>(m_type);
        ASSERT(atype != NULL);

        if (auto p = dynamic_cast<ExpNode_ConstantInt*>(node->right.get())) {
            m_addrOff += p->value * atype->elemType->getSize();
        }
        else {
            clearAddrOff();

            vector<int> codes;
            ExpNodeVisitor_CodeGen rcodeGen(codes, node->right);
            rcodeGen.convertTo(TypeSystem::instance()->getType("int"));
            ASSERT(rcodeGen.m_type == TypeSystem::instance()->getType("int"));
            this->mergeFrom(&rcodeGen);
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

        if (ftype->isVarLengOfArg) {
            ASSERT(node->args.size() >= ftype->argsT.size());
        }
        else ASSERT(node->args.size() == ftype->argsT.size());

        for (int i = 0; i < node->args.size(); ++i) {
            vector<int> codes;
            ExpNodeVisitor_CodeGen codeGen(codes, node->args[i]);
            codeGen.toRval();
            if (i < ftype->argsT.size() && 
                    ftype->argsT[i] == TypeSystem::instance()->getType("int") &&
                    codeGen.m_type == TypeSystem::instance()->getType("char")) {
                codeGen.convertTo(TypeSystem::instance()->getType("int"));
            }
            retArgSize += codeGen.m_type->getSize();
            mergeFrom(&codeGen);
        }

        m_codes.push_back(ByteCode_SizeIndepend<BCT_Call>::emit(
                    node->name, ftype->retT->getSize(), retArgSize));
    }
    virtual void visit(ExpNode_Assign* node) 
    {
        node->left->acceptVisitor(this);
        ASSERT(m_lval);
        clearAddrOff();

        vector<int> codes;
        ExpNodeVisitor_CodeGen rcodeGen(codes, node->right);
        rcodeGen.toRval();
        if (m_type == TypeSystem::instance()->getType("int")) {
            if (rcodeGen.m_type == TypeSystem::instance()->getType("char")) {
                rcodeGen.convertTo(TypeSystem::instance()->getType("int"));
            }
        }
        ASSERT(m_type == rcodeGen.m_type);

        mergeFrom(&rcodeGen);
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
private:
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
            m_codes.push_back(m_type->getSize() == 1 ?
                    ByteCode_SizeDepend<BCT_ReadAddr, 1>::emit() :
                    ByteCode_SizeDepend<BCT_ReadAddr, 4>::emit());
            m_lval = false;
        }
    }
    void checkConversion(ExpNodeVisitor_CodeGen *o)
    {
        if (m_type == o->m_type) return;
        if (m_type == TypeSystem::instance()->getType("int") &&
                o->m_type == TypeSystem::instance()->getType("char")) {
            o->convertTo(TypeSystem::instance()->getType("int"));
        }
        else if (o->m_type == TypeSystem::instance()->getType("int") &&
                m_type == TypeSystem::instance()->getType("char")) {
            this->convertTo(TypeSystem::instance()->getType("int"));
        }
    }
    // TODO: fix it's invoke
    void convertTo(IType *type)
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
    void mergeFrom(ExpNodeVisitor_CodeGen *v)
    {
        m_codes.insert(m_codes.end(), v->m_codes.begin(), v->m_codes.end());
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
        m_codes(codes)
    {
        stmt->acceptVisitor(this);
    }
private:
    virtual void visit(StmtNode_Exp* node) 
    {
    }
    virtual void visit(StmtNode_Block* node) 
    {
    }
    virtual void visit(StmtNode_DefineLocal* node) 
    {
    }
    virtual void visit(StmtNode_Break* node) 
    {
    }
    virtual void visit(StmtNode_Continue* node) 
    {
    }
    virtual void visit(StmtNode_Return* node) 
    {
    }
    virtual void visit(StmtNode_For* node) 
    {
    }
    virtual void visit(StmtNode_IfElse* node) 
    {
    }
    virtual void visit(StmtNode_Switch* node) 
    {
    }
private:
    vector<int> &m_codes;
};

ByteCodeSeq::ByteCodeSeq(StmtNodePtr node):
    m_frameSize(0)
{
}
ByteCodeSeq::~ByteCodeSeq()
{
}

void ByteCodeSeq::disassemble(ostream& so)
{
}
void ByteCodeSeq::execute(RuntimeEnv *env)
{
}
