
#include "pch.h"
#include "Utils.h"
#include "StackBasedInterpreter.h"
#include "StackBasedISA.h"

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
    ip += ((SB_Instruction<SBC_Jmp>*)ip)->off;
}
FORCE_INLINE static void handle_TJmp(int *&stackTop, int locals[LocalStackSize], char *&ip) {
    if (*--stackTop) ip += ((SB_Instruction<SBC_TJmp>*)ip)->off;
    else ip += sizeof(SB_Instruction<SBC_PushInt>);
}
FORCE_INLINE static void handle_Repeat(int *&stackTop, int locals[LocalStackSize], char *&ip) {
    SB_Instruction<SBC_Repeat>* p = (SB_Instruction<SBC_Repeat>*)ip;
    if (locals[p->loopCounter] > 0) {
        --locals[p->loopCounter];
        locals[p->iter] += locals[p->step];
        ip += sizeof(*p);
    } else {
        ip += p->off;
    }
}
FORCE_INLINE static void handle_Nop(int *&stackTop, int locals[LocalStackSize], char *&ip) {
    ip += sizeof(SB_Instruction<SBC_Nop>);
}

class SB_Interpreter_CallThreading: public SB_Interpreter {
public:
    virtual int interpret(SB_InstructionList *insList) {
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
        Byte* ip = &insList->getBytes()[0];
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

class SB_Interpreter_SwitchThreading: public SB_Interpreter {
public:
    virtual int interpret(SB_InstructionList *insList) {
        Byte* ip = &insList->getBytes()[0];
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

class SB_Interpreter_ReplicateSwitchThreading: public SB_Interpreter {
public:
    virtual int interpret(SB_InstructionList *insList) {
        Byte* ip = &insList->getBytes()[0];
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

class SB_Interpreter_TokenThreading: public SB_Interpreter {
public:
    virtual int interpret(SB_InstructionList *insList) {
#ifdef __GNUC__
        Byte* ip = &insList->getBytes()[0];
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

class SB_Interpreter_DirectThreading: public SB_Interpreter {
public:
    virtual int interpret(SB_InstructionList *insList) {
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

        vector<Byte> bytes(insList->getBytes());
        for (int off = 0; off < (int)bytes.size(); ) {
            CodeType code = (CodeType&)bytes[off];
            (void*&)bytes[off] = label_table[code];
            off += SB_getInsuctionSize(code);
        }

        Byte* ip = &bytes[0];
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

class SB_Interpreter_JIT: public SB_Interpreter {
public:
    virtual int interpret(SB_InstructionList *insList) {
        return 0;
    }
    virtual bool isValid() { return false; }
private:
};

SB_Interpreter* SB_Interpreter::getInstance(const string &name) {
    if (name == "call") {
        return new SB_Interpreter_CallThreading();
    } else if (name == "switch") {
        return new SB_Interpreter_SwitchThreading();
    } else if (name == "repl_switch") {
        return new SB_Interpreter_ReplicateSwitchThreading();
    } else if (name == "token") {
        return new SB_Interpreter_TokenThreading();
    } else if (name == "direct") {
        return new SB_Interpreter_DirectThreading();
    } else if (name == "jit") {
        return new SB_Interpreter_JIT();
    } else {
        assert(0);
        return NULL;
    }
}
