
#include "pch.h"
#include "Utils.h"
#include "StackBasedInterpreter.h"

enum SB_Code {
    SBC_Add, 
    SBC_Sub, 
    SBC_Mul, 
    SBC_Div,

    SBC_EQ, 
    SBC_NE,

    SBC_PushLocal, 
    SBC_PopLocal,
    SBC_PushInt,

    SBC_Jmp, 
    SBC_TJmp,

    SBC_Repeat, 

    SBC_Nop,
    SBC_EOF,

    SBC_COUNT,
};

#pragma pack(push, 1)
template<SB_Code>
struct SB_Instruction;
template<>
struct SB_Instruction<SBC_Add> {
    CodeType code;
};
template<>
struct SB_Instruction<SBC_Sub> {
    CodeType code;
};
template<>
struct SB_Instruction<SBC_Mul> {
    CodeType code;
};
template<>
struct SB_Instruction<SBC_Div> {
    CodeType code;
};
template<>
struct SB_Instruction<SBC_EQ> {
    CodeType code;
};
template<>
struct SB_Instruction<SBC_NE> {
    CodeType code;
};
template<>
struct SB_Instruction<SBC_PushLocal> {
    CodeType code;
    LocalIdxType local;
};
template<>
struct SB_Instruction<SBC_PopLocal> {
    CodeType code;
    LocalIdxType local;
};
template<>
struct SB_Instruction<SBC_PushInt> {
    CodeType code;
    int i;
};
template<>
struct SB_Instruction<SBC_Jmp> {
    CodeType code;
    JmpOffType jmpOff;
};
template<>
struct SB_Instruction<SBC_TJmp> {
    CodeType code;
    JmpOffType jmpOff;
};
template<>
struct SB_Instruction<SBC_Repeat> {
    CodeType code;
    LocalIdxType loopCounter, iter, step;
    JmpOffType jmpOff;
};
template<>
struct SB_Instruction<SBC_Nop> {
    CodeType code;
};
template<>
struct SB_Instruction<SBC_EOF> {
    CodeType code;
};
#pragma pack(pop)

//==============================

FORCE_INLINE static void handle_Add(int *&stackTop, int locals[LocalStackSize], char *&ip) {
    stackTop[-2] += stackTop[-1];
    --stackTop;
    ip += sizeof(SB_Instruction<SBC_Add>);
}
FORCE_INLINE static void handle_Sub(int *&stackTop, int locals[LocalStackSize], char *&ip) {
    stackTop[-2] -= stackTop[-1];
    --stackTop;
    ip += sizeof(SB_Instruction<SBC_Sub>);
}
FORCE_INLINE static void handle_Mul(int *&stackTop, int locals[LocalStackSize], char *&ip) {
    stackTop[-2] *= stackTop[-1];
    --stackTop;
    ip += sizeof(SB_Instruction<SBC_Mul>);
}
FORCE_INLINE static void handle_Div(int *&stackTop, int locals[LocalStackSize], char *&ip) {
    stackTop[-2] /= stackTop[-1];
    --stackTop;
    ip += sizeof(SB_Instruction<SBC_Div>);
}
FORCE_INLINE static void handle_EQ(int *&stackTop, int locals[LocalStackSize], char *&ip) {
    stackTop[-2] = stackTop[-2] == stackTop[-1];
    --stackTop;
    ip += sizeof(SB_Instruction<SBC_EQ>);
}
FORCE_INLINE static void handle_NE(int *&stackTop, int locals[LocalStackSize], char *&ip) {
    stackTop[-2] = stackTop[-2] != stackTop[-1];
    --stackTop;
    ip += sizeof(SB_Instruction<SBC_NE>);
}
FORCE_INLINE static void handle_PushLocal(int *&stackTop, int locals[LocalStackSize], char *&ip) {
    *stackTop++ = locals[((SB_Instruction<SBC_PushLocal>*)ip)->local];
    ip += sizeof(SB_Instruction<SBC_PushLocal>);
}
FORCE_INLINE static void handle_PopLocal(int *&stackTop, int locals[LocalStackSize], char *&ip) {
    locals[((SB_Instruction<SBC_PopLocal>*)ip)->local] = *--stackTop;
    ip += sizeof(SB_Instruction<SBC_PopLocal>);
}
FORCE_INLINE static void handle_PushInt(int *&stackTop, int locals[LocalStackSize], char *&ip) {
    *stackTop++ = ((SB_Instruction<SBC_PushInt>*)ip)->i;
    ip += sizeof(SB_Instruction<SBC_PushInt>);
}
FORCE_INLINE static void handle_Jmp(int *&stackTop, int locals[LocalStackSize], char *&ip) {
    ip += ((SB_Instruction<SBC_Jmp>*)ip)->jmpOff;
}
FORCE_INLINE static void handle_TJmp(int *&stackTop, int locals[LocalStackSize], char *&ip) {
    if (*--stackTop) ip += ((SB_Instruction<SBC_TJmp>*)ip)->jmpOff;
    else ip += sizeof(SB_Instruction<SBC_PushInt>);
}
FORCE_INLINE static void handle_Repeat(int *&stackTop, int locals[LocalStackSize], char *&ip) {
    SB_Instruction<SBC_Repeat>* p = (SB_Instruction<SBC_Repeat>*)ip;
    if (locals[p->loopCounter] > 0) {
        --locals[p->loopCounter];
        locals[p->iter] += locals[p->step];
        ip += sizeof(*p);
    } else {
        ip += p->jmpOff;
    }
}
FORCE_INLINE static void handle_Nop(int *&stackTop, int locals[LocalStackSize], char *&ip) {
    ip += sizeof(SB_Instruction<SBC_Nop>);
}

class SB_Interpreter_CallThreading: public Interpreter {
public:
    SB_Interpreter_CallThreading(InterpreterFactory* factory): Interpreter(factory){}
    virtual int interpret(InstructionList *insList) {
        static void(*handlers[])(int *&, int *, char*&) = {
            &handle_Add, 
            &handle_Sub, 
            &handle_Mul, 
            &handle_Div,
            &handle_EQ, 
            &handle_NE,
            &handle_PushLocal, 
            &handle_PopLocal,
            &handle_PushInt,
            &handle_Jmp, 
            &handle_TJmp,
            &handle_Repeat, 
            &handle_Nop,
        };
        char* ip = &insList->getBytes()[0];
        int locals[LocalStackSize] = {1};
        int stack[EvalStackSize] = {0};
        int *stackTop = stack;
        while ((CodeType&)*ip != SBC_EOF) {
            handlers[(CodeType&)*ip](stackTop, locals, ip);
        }
        return *--stackTop;
    }
    virtual bool isValid() { return true; }
private:
};

class SB_Interpreter_SwitchThreading: public Interpreter {
public:
    SB_Interpreter_SwitchThreading(InterpreterFactory* factory): Interpreter(factory){}
    virtual int interpret(InstructionList *insList) {
        char* ip = &insList->getBytes()[0];
        int locals[LocalStackSize] = {1};
        int stack[EvalStackSize] = {0};
        int *stackTop = stack;
        for (;;) {
            switch ((CodeType&)*ip) {
                case SBC_Add: handle_Add(stackTop, locals, ip); break;
                case SBC_Sub: handle_Sub(stackTop, locals, ip); break;
                case SBC_Mul: handle_Mul(stackTop, locals, ip); break;
                case SBC_Div: handle_Div(stackTop, locals, ip); break;
                case SBC_EQ: handle_EQ(stackTop, locals, ip); break;
                case SBC_NE: handle_NE(stackTop, locals, ip); break;
                case SBC_PushLocal: handle_PushLocal(stackTop, locals, ip); break;
                case SBC_PopLocal: handle_PopLocal(stackTop, locals, ip); break;
                case SBC_PushInt: handle_PushInt(stackTop, locals, ip); break;
                case SBC_Jmp: handle_Jmp(stackTop, locals, ip); break;
                case SBC_TJmp: handle_TJmp(stackTop, locals, ip); break;
                case SBC_Repeat: handle_Repeat(stackTop, locals, ip); break;
                case SBC_Nop: handle_Nop(stackTop, locals, ip); break;
                case SBC_EOF: return *--stackTop;
            }
        }
        return *--stackTop;
    }
    virtual bool isValid() { return true; }
private:
};

class SB_Interpreter_ReplicateSwitchThreading: public Interpreter {
public:
    SB_Interpreter_ReplicateSwitchThreading(InterpreterFactory* factory): Interpreter(factory){}
    virtual int interpret(InstructionList *insList) {
        char* ip = &insList->getBytes()[0];
        int locals[LocalStackSize] = {1};
        int stack[EvalStackSize] = {0};
        int *stackTop = stack;

#define NEXT() switch ((CodeType&)*ip) {\
                case SBC_Add: goto label_Add; \
                case SBC_Sub: goto label_Sub; \
                case SBC_Mul: goto label_Mul; \
                case SBC_Div: goto label_Div; \
                case SBC_EQ: goto label_EQ; \
                case SBC_NE: goto label_NE; \
                case SBC_PushLocal: goto label_PushLocal; \
                case SBC_PopLocal: goto label_PopLocal; \
                case SBC_PushInt: goto label_PushInt; \
                case SBC_Jmp: goto label_Jmp; \
                case SBC_TJmp: goto label_TJmp; \
                case SBC_Repeat: goto label_Repeat; \
                case SBC_Nop: goto label_Nop; \
                case SBC_EOF: goto label_EOF;\
            }

        NEXT();
label_Add: handle_Add(stackTop, locals, ip);  NEXT();
label_Sub: handle_Sub(stackTop, locals, ip);  NEXT();
label_Mul: handle_Mul(stackTop, locals, ip);  NEXT();
label_Div: handle_Div(stackTop, locals, ip);  NEXT();
label_EQ: handle_EQ(stackTop, locals, ip);  NEXT();
label_NE: handle_NE(stackTop, locals, ip);  NEXT();
label_PushLocal: handle_PushLocal(stackTop, locals, ip);  NEXT();
label_PopLocal: handle_PopLocal(stackTop, locals, ip);  NEXT();
label_PushInt: handle_PushInt(stackTop, locals, ip);  NEXT();
label_Jmp: handle_Jmp(stackTop, locals, ip);  NEXT();
label_TJmp: handle_TJmp(stackTop, locals, ip);  NEXT();
label_Repeat: handle_Repeat(stackTop, locals, ip);  NEXT();
label_Nop: handle_Nop(stackTop, locals, ip);  NEXT();
label_EOF: return *--stackTop;

#undef NEXT

        return *--stackTop;
    }
    virtual bool isValid() { return true; }
private:
};

class SB_Interpreter_TokenThreading: public Interpreter {
public:
    SB_Interpreter_TokenThreading(InterpreterFactory* factory): Interpreter(factory){}
    virtual int interpret(InstructionList *insList) {
#ifdef __GNUC__
        char* ip = &insList->getBytes()[0];
        int locals[LocalStackSize] = {1};
        int stack[EvalStackSize] = {0};
        int *stackTop = stack;
        void* label_table[] = {
            &&label_Add,
            &&label_Sub,
            &&label_Mul,
            &&label_Div,
            &&label_EQ,
            &&label_NE,
            &&label_PushLocal,
            &&label_PopLocal,
            &&label_PushInt,
            &&label_Jmp,
            &&label_TJmp,
            &&label_Repeat,
            &&label_Nop,
            &&label_EOF,
        };

#define NEXT() goto *label_table[(CodeType&)*ip]

        NEXT();
label_Add: handle_Add(stackTop, locals, ip);  NEXT();
label_Sub: handle_Sub(stackTop, locals, ip);  NEXT();
label_Mul: handle_Mul(stackTop, locals, ip);  NEXT();
label_Div: handle_Div(stackTop, locals, ip);  NEXT();
label_EQ: handle_EQ(stackTop, locals, ip);  NEXT();
label_NE: handle_NE(stackTop, locals, ip);  NEXT();
label_PushLocal: handle_PushLocal(stackTop, locals, ip);  NEXT();
label_PopLocal: handle_PopLocal(stackTop, locals, ip);  NEXT();
label_PushInt: handle_PushInt(stackTop, locals, ip);  NEXT();
label_Jmp: handle_Jmp(stackTop, locals, ip);  NEXT();
label_TJmp: handle_TJmp(stackTop, locals, ip);  NEXT();
label_Repeat: handle_Repeat(stackTop, locals, ip);  NEXT();
label_Nop: handle_Nop(stackTop, locals, ip);  NEXT();
label_EOF: return *--stackTop;

#undef NEXT

        return *--stackTop;
#else
        return 0;
#endif
    }
    virtual bool isValid() { 
#ifdef __GNUC__
        return true;
#else
        return false;
#endif
    }
private:
};

class SB_Interpreter_DirectThreading: public Interpreter {
public:
    SB_Interpreter_DirectThreading(InterpreterFactory* factory): Interpreter(factory){}
    virtual int interpret(InstructionList *insList) {
#if defined(__GNUC__) && ((!defined(__x86_64__) && (CodeSize >= 4)) || (defined(__x86_64__) && (CodeSize >= 8) ))
        void* label_table[] = {
            &&label_Add,
            &&label_Sub,
            &&label_Mul,
            &&label_Div,
            &&label_EQ,
            &&label_NE,
            &&label_PushLocal,
            &&label_PopLocal,
            &&label_PushInt,
            &&label_Jmp,
            &&label_TJmp,
            &&label_Repeat,
            &&label_Nop,
            &&label_EOF,
        };

        vector<char> bytes(insList->getBytes());
        for (int off = 0; off < (int)bytes.size(); ) {
            CodeType code = (CodeType&)bytes[off];
            (void*&)bytes[off] = label_table[code];
            off += m_factory->getInstructionSize(code);
        }

        char* ip = &bytes[0];
        int locals[LocalStackSize] = {1};
        int stack[EvalStackSize] = {0};
        int *stackTop = stack;

#define NEXT() goto **(void**)ip

        NEXT();
label_Add: handle_Add(stackTop, locals, ip);  NEXT();
label_Sub: handle_Sub(stackTop, locals, ip);  NEXT();
label_Mul: handle_Mul(stackTop, locals, ip);  NEXT();
label_Div: handle_Div(stackTop, locals, ip);  NEXT();
label_EQ: handle_EQ(stackTop, locals, ip);  NEXT();
label_NE: handle_NE(stackTop, locals, ip);  NEXT();
label_PushLocal: handle_PushLocal(stackTop, locals, ip);  NEXT();
label_PopLocal: handle_PopLocal(stackTop, locals, ip);  NEXT();
label_PushInt: handle_PushInt(stackTop, locals, ip);  NEXT();
label_Jmp: handle_Jmp(stackTop, locals, ip);  NEXT();
label_TJmp: handle_TJmp(stackTop, locals, ip);  NEXT();
label_Repeat: handle_Repeat(stackTop, locals, ip);  NEXT();
label_Nop: handle_Nop(stackTop, locals, ip);  NEXT();
label_EOF: return *--stackTop;

#undef NEXT

        return *--stackTop;
#else
        return 0;
#endif
    }
    virtual bool isValid() { 
#if defined(__GNUC__) && ((!defined(__x86_64__) && (CodeSize >= 4)) || (defined(__x86_64__) && (CodeSize >= 8) ))
        return true;
#else
        return false;
#endif
    }
private:
};

class SB_Interpreter_JIT: public Interpreter {
public:
    SB_Interpreter_JIT(InterpreterFactory* factory): Interpreter(factory){}
    virtual int interpret(InstructionList *insList) {
        return 0;
    }
    virtual bool isValid() { return false; }
private:
};
//==============================
class SB_InstructionList: public InstructionList {
public:
    SB_InstructionList(InterpreterFactory* factory): InstructionList(factory){}
    virtual void translateJmpIdx2Off() {
        vector<int> insOffs;
        for (int off = 0; off < (int)m_bytes.size(); ) {
            insOffs.push_back(off);
            off += m_factory->getInstructionSize((CodeType&)m_bytes[off]);
        }
        insOffs.push_back((int)m_bytes.size());

        for (int off = 0; off < (int)m_bytes.size(); ) {
            CodeType code = (CodeType&)m_bytes[off];
            JmpOffType *jmpOff = NULL;
            switch (code) {
                case SBC_Jmp: jmpOff = &((SB_Instruction<SBC_Jmp>&)m_bytes[off]).jmpOff; break;
                case SBC_TJmp: jmpOff = &((SB_Instruction<SBC_TJmp>&)m_bytes[off]).jmpOff; break;
                case SBC_Repeat: jmpOff = &((SB_Instruction<SBC_Repeat>&)m_bytes[off]).jmpOff; break;
                default: break;
            }
            if (jmpOff != NULL) *jmpOff = insOffs[*jmpOff] - off;
            off += m_factory->getInstructionSize(code);
        }
    }
    virtual void appendEOF() {
        appendValue(SBC_EOF);
    }
};
//==============================
SB_InterpreterFactory::SB_InterpreterFactory() {
    InstructionMetaManager *mgr = &m_insMetaMgr;
    mgr->add(new InstructionMeta(SBC_Add, "add"));
    mgr->add(new InstructionMeta(SBC_Sub, "sub"));
    mgr->add(new InstructionMeta(SBC_Mul, "mul"));
    mgr->add(new InstructionMeta(SBC_Div, "div"));
    mgr->add(new InstructionMeta(SBC_EQ, "eq"));
    mgr->add(new InstructionMeta(SBC_NE, "ne"));
    mgr->add(new InstructionMeta1<LocalIdxType>(SBC_PushLocal, "pushlocal"));
    mgr->add(new InstructionMeta1<LocalIdxType>(SBC_PopLocal, "poplocal"));
    mgr->add(new InstructionMeta1<int>(SBC_PushInt, "pushint"));
    mgr->add(new InstructionMeta1<JmpOffType>(SBC_Jmp, "jmp"));
    mgr->add(new InstructionMeta1<JmpOffType>(SBC_TJmp, "tjmp"));
    mgr->add(new InstructionMeta4<LocalIdxType, LocalIdxType, LocalIdxType, JmpOffType>(SBC_Repeat, "repeat"));
    mgr->add(new InstructionMeta(SBC_Nop, "nop"));
    mgr->add(new InstructionMeta(SBC_EOF, "eof"));
}
Interpreter* SB_InterpreterFactory::createInterpreter(const string &name) {
    if (name == "call") {
        return new SB_Interpreter_CallThreading(this);
    } else if (name == "switch") {
        return new SB_Interpreter_SwitchThreading(this);
    } else if (name == "repl_switch") {
        return new SB_Interpreter_ReplicateSwitchThreading(this);
    } else if (name == "token") {
        return new SB_Interpreter_TokenThreading(this);
    } else if (name == "direct") {
        return new SB_Interpreter_DirectThreading(this);
    } else if (name == "jit") {
        return new SB_Interpreter_JIT(this);
    } else {
        assert(0);
        return NULL;
    }
}
InstructionList* SB_InterpreterFactory::createInstructionList() {
    return new SB_InstructionList(this);
}

