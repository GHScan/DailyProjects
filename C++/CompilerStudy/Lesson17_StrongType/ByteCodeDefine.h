
#ifndef BYTE_CODE_DEFINE_H
#define BYTE_CODE_DEFINE_H

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
    BCT_NotEqual,
    BCT_Greater,
    BCT_GreaterEq,
    BCT_Not,
};
// Size independ
template<int n>
struct ByteCode_SizeIndepend;

template<>
struct ByteCode_SizeIndepend<BCT_Convert4To1>
{
    static int emit()
    {
        return (BCT_Convert4To1 << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        int i; env->popValue(i);
        env->pushValue((char)i);
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("conv 4->1");
    }
};
template<>
struct ByteCode_SizeIndepend<BCT_Convert1To4>
{
    static int emit()
    {
        return (BCT_Convert1To4 << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        char c; env->popValue(c);
        env->pushValue((int)c);
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("conv 1->4");
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
    static void disassemble(int code, ostream& so)
    {
        int i = code & 0x7fffff;
        so << format("pushInt %d", (code >> 23) & 1 ? -i : i);
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
    static void disassemble(int code, ostream& so)
    {
        so << format("pushInt %d", ConstantPool::instance()->getInt(code & 0xffffff));
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
    static void disassemble(int code, ostream& so)
    {
        so << format("pushStr '%s'", ConstantPool::instance()->getString(code & 0xffffff));
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
        env->pc = (code & 0xffffff) - 1;
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("jmp %d", (code & 0xffffff) + 1);
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
        if (!v) env->pc = (code & 0xffffff) - 1;
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("jz %d", (code & 0xffffff) + 1);
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
        if (v) env->pc = (code & 0xffffff) - 1;
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("jnz %d", (code & 0xffffff) + 1);
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
    static void disassemble(int code, ostream& so)
    {
        const char *name = ConstantPool::instance()->getString((code >> 12) & 0xfff);
        int numID = ConstantPool::instance()->getInt(code & 0xfff);
        int retSize = numID >> 24, retArgSize = numID & 0xffffff;
        so << format("call '%s', %d, %d", name, retSize, retArgSize);
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
    static void disassemble(int code, ostream& so)
    {
        so << format("pushLAddr %d", (code & 0xffffff));
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
    static void disassemble(int code, ostream& so)
    {
        so << format("pushGAddr %d", (code & 0xffffff));
    }
};
//////////

template<int n>
struct Bytes2Type;
template<>
struct Bytes2Type<1>
{
    typedef char Type;
    enum {
        E_CodeMask = 0 << 31,
    };
};
template<>
struct Bytes2Type<4>
{
    typedef int Type;
    enum {
        E_CodeMask = 1 << 31,
    };
};

template<int code, int bits>
struct ByteCode_SizeDepend;
template<int bits>
struct ByteCode_SizeDepend<BCT_ReadAddr, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return Bytes2Type<bits>::E_CodeMask | (BCT_ReadAddr << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type* v; env->popValue(v);
        env->pushValue(*v);
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("readAddr(%d)", bits);
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_WriteAddr, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return Bytes2Type<bits>::E_CodeMask | (BCT_WriteAddr << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type v; env->popValue(v);
        Type *addr; env->popValue(addr);
        *addr = v;
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("writeAddr(%d)", bits);
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_PushLocal, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit(int off)
    {
        ASSERT(off < (1 << 24));
        return Bytes2Type<bits>::E_CodeMask | (BCT_PushLocal << 24) | off;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        env->pushValue(
                env->localVariable<Type>(code & 0xffffff));
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("pushL(%d) %d", bits, code & 0xffffff);
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_PushGlobal, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit(int off)
    {
        ASSERT(off < (1 << 24));
        return Bytes2Type<bits>::E_CodeMask | (BCT_PushGlobal << 24) | off;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        env->pushValue(
                env->globalVariable<Type>(code & 0xffffff));
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("pushG(%d) %d", bits, code & 0xffffff);
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_PopLocal, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit(int off)
    {
        ASSERT(off < (1 << 24));
        return Bytes2Type<bits>::E_CodeMask | (BCT_PopLocal << 24) | off;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type v; env->popValue(v);
        env->localVariable<Type>(code & 0xffffff) = v;
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("popL(%d) %d", bits, code & 0xffffff);
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_PopGlobal, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit(int off)
    {
        ASSERT(off < (1 << 24));
        return Bytes2Type<bits>::E_CodeMask | (BCT_PopGlobal << 24) | off;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type v; env->popValue(v);
        env->globalVariable<Type>(code & 0xffffff) = v;
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("popG(%d) %d", bits, code & 0xffffff);
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_PushI, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit(int i)
    {
        assert(i < 0);
        return Bytes2Type<bits>::E_CodeMask | (BCT_PushI << 24) | -i;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        env->pushValue(env->topValue<Type>(- (code & 0xffffff)));
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("pushPrev(%d) %d", bits, -(code & 0xffffff));
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_PopN, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit(int n)
    {
        ASSERT(n < (1 << 24));
        return Bytes2Type<bits>::E_CodeMask | (BCT_PopN << 24) | n;
    }
    static void execute(int code, RuntimeEnv *env)
    {
        int n = code & 0xffffff;
        while (--n >= 0) env->popValue<Type>();
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("popN(%d) %d", bits, code & 0xffffff);
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Add, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return Bytes2Type<bits>::E_CodeMask | (BCT_Add << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type v; env->popValue(v);
        env->topValue<Type>(-1) += v;
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("add(%d)", bits);
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Sub, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return Bytes2Type<bits>::E_CodeMask | (BCT_Sub << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type v; env->popValue(v);
        env->topValue<Type>(-1) -= v;
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("sub(%d)", bits);
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Mul, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return Bytes2Type<bits>::E_CodeMask | (BCT_Mul << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type v; env->popValue(v);
        env->topValue<Type>(-1) *= v;
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("mul(%d)", bits);
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Div, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return Bytes2Type<bits>::E_CodeMask | (BCT_Div << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type v; env->popValue(v);
        env->topValue<Type>(-1) /= v;
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("div(%d)", bits);
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Mod, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return Bytes2Type<bits>::E_CodeMask | (BCT_Mod << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type v; env->popValue(v);
        env->topValue<Type>(-1) %= v;
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("mod(%d)", bits);
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Inc, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return Bytes2Type<bits>::E_CodeMask | (BCT_Inc << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        ++*env->topValue<Type*>(-1);
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("inc(%d)", bits);
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Dec, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return Bytes2Type<bits>::E_CodeMask | (BCT_Dec << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        --*env->topValue<Type*>(-1);
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("dec(%d)", bits);
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Less, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return Bytes2Type<bits>::E_CodeMask | (BCT_Less << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type l, r;
        env->popValue(r);
        env->popValue(l);
        env->pushValue(int(l < r));
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("less(%d)", bits);
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_LessEq, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return Bytes2Type<bits>::E_CodeMask | (BCT_LessEq << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type l, r;
        env->popValue(r);
        env->popValue(l);
        env->pushValue(int(l <= r));
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("lessEq(%d)", bits);
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Equal, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return Bytes2Type<bits>::E_CodeMask | (BCT_Equal << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type l, r;
        env->popValue(r);
        env->popValue(l);
        env->pushValue(int(l == r));
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("equal(%d)", bits);
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_NotEqual, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return Bytes2Type<bits>::E_CodeMask | (BCT_NotEqual << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type l, r;
        env->popValue(r);
        env->popValue(l);
        env->pushValue(int(l != r));
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("nEqual(%d)", bits);
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Greater, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return Bytes2Type<bits>::E_CodeMask | (BCT_Greater << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type l, r;
        env->popValue(r);
        env->popValue(l);
        env->pushValue(int(l > r));
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("greater(%d)", bits);
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_GreaterEq, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return Bytes2Type<bits>::E_CodeMask | (BCT_GreaterEq << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type l, r;
        env->popValue(r);
        env->popValue(l);
        env->pushValue(int(l >= r));
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("greaterEq(%d)", bits);
    }
};
template<int bits>
struct ByteCode_SizeDepend<BCT_Not, bits>: public Bytes2Type<bits>
{
    typedef typename Bytes2Type<bits>::Type Type;
    static int emit()
    {
        return Bytes2Type<bits>::E_CodeMask | (BCT_Not << 24);
    }
    static void execute(int code, RuntimeEnv *env)
    {
        Type v; env->popValue(v);
        env->pushValue(int(!v));
    }
    static void disassemble(int code, ostream& so)
    {
        so << format("not(%d)", bits);
    }
};

#endif
