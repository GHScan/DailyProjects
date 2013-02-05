
#include "pch.h"
#include "AST.h"
#include "SymbolTable.h"
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
        auto iter = m_int2ID.lower_bound(s);
        if (iter == m_int2ID.end() || iter->first != s) {
            iter = m_int2ID.insert(iter, pair<int, int>(i, m_id2Data.size()));
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
    BCT_Convert32To8,
    BCT_Convert8To32,
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
struct ByteCode_SizeIndepend<BCT_Convert32To8>
{
    static int emit()
    {
        return BCT_Convert32To8 << 24;
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
struct ByteCode_SizeIndepend<BCT_Convert8To32>
{
    static int emit()
    {
        return BCT_Convert8To32 << 24;
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
struct ByteCode_SizeIndepend<BCT_Jump>
{
    static int emit(int pc)
    {
        ASSERT(pc < (1 << 24));
        return (BCT_Jump << 24) | pc;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        env->ip = code & 0xffffff;
    }
    static void disassemble(ostream& so)
    {
    }
};
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
        if (!v) env->ip = code & 0xffffff;
    }
    static void disassemble(ostream& so)
    {
    }
};
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
        if (v) env->ip = code & 0xffffff;
    }
    static void disassemble(ostream& so)
    {
    }
};
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
struct Bits2Type;
template<>
struct Bits2Type<8>
{
    typedef char Type;
};
template<>
struct Bits2Type<32>
{
    typedef int Type;
};

template<int code, int bits>
struct ByteCodeE_SizeDepend;
template<int bits>
struct ByteCodeE_SizeDepend<BCT_ReadAddr, bits>: public Bits2Type<bits>
{
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
struct ByteCodeE_SizeDepend<BCT_WriteAddr, bits>: public Bits2Type<bits>
{
    static int emit()
    {
        return (BCT_WriteAddr << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type v; env->popValue(v);
        *env->topValue<Type*>(-1) = v;
    }
    static void disassemble(ostream& so)
    {
    }
};
template<int bits>
struct ByteCodeE_SizeDepend<BCT_PushLocal, bits>: public Bits2Type<bits>
{
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
struct ByteCodeE_SizeDepend<BCT_PushGlobal, bits>: public Bits2Type<bits>
{
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
struct ByteCodeE_SizeDepend<BCT_PopLocal, bits>: public Bits2Type<bits>
{
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
struct ByteCodeE_SizeDepend<BCT_PopGlobal, bits>: public Bits2Type<bits>
{
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
struct ByteCodeE_SizeDepend<BCT_PushI, bits>: public Bits2Type<bits>
{
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
struct ByteCodeE_SizeDepend<BCT_PopN, bits>: public Bits2Type<bits>
{
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
struct ByteCodeE_SizeDepend<BCT_Add, bits>: public Bits2Type<bits>
{
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
struct ByteCodeE_SizeDepend<BCT_Sub, bits>: public Bits2Type<bits>
{
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
struct ByteCodeE_SizeDepend<BCT_Mul, bits>: public Bits2Type<bits>
{
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
struct ByteCodeE_SizeDepend<BCT_Div, bits>: public Bits2Type<bits>
{
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
struct ByteCodeE_SizeDepend<BCT_Mod, bits>: public Bits2Type<bits>
{
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
struct ByteCodeE_SizeDepend<BCT_Inc, bits>: public Bits2Type<bits>
{
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
struct ByteCodeE_SizeDepend<BCT_Dec, bits>: public Bits2Type<bits>
{
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
struct ByteCodeE_SizeDepend<BCT_Less, bits>: public Bits2Type<bits>
{
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
struct ByteCodeE_SizeDepend<BCT_LessEq, bits>: public Bits2Type<bits>
{
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
struct ByteCodeE_SizeDepend<BCT_Equal, bits>: public Bits2Type<bits>
{
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
struct ByteCodeE_SizeDepend<BCT_Greater, bits>: public Bits2Type<bits>
{
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
struct ByteCodeE_SizeDepend<BCT_GreaterEq, bits>: public Bits2Type<bits>
{
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
    ExpNodeVisitor_CodeGen(vector<int>& codes):
        m_codes(codes)
    {
    }
private:
    virtual void visit(ExpNode_ConstantInt* node) 
    {
    }
    virtual void visit(ExpNode_ConstantLiteral* node) 
    {
    }
    virtual void visit(ExpNode_Variable* node) 
    {
    }
    virtual void visit(ExpNode_Conversion* node) 
    {
    }
    virtual void visit(ExpNode_BinaryOp* node) 
    {
    }
    virtual void visit(ExpNode_UnaryOp* node) 
    {
    }
    virtual void visit(ExpNode_Addr* node) 
    {
    }
    virtual void visit(ExpNode_Unref* node) 
    {
    }
    virtual void visit(ExpNode_Field* node) 
    {
    }
    virtual void visit(ExpNode_ArrayElem* node) 
    {
    }
    virtual void visit(ExpNode_Call* node) 
    {
    }
    virtual void visit(ExpNode_Assign* node) 
    {
    }
    virtual void visit(ExpNode_Sizeof* node) 
    {
    }
private:
    vector<int> &m_codes;
};

class StmtNodeVisitor_CodeGen:
    public IStmtNodeVisitor
{
public:
    StmtNodeVisitor_CodeGen(vector<int>& codes):
        m_codes(codes)
    {
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
