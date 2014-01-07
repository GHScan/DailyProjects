
#include "pch.h"
#include "Utils.h"
#include "RegisterBasedISA.h"
#include "RegisterBasedInterpreter.h"

FORCE_INLINE static void handle_Add(int locals[LocalStackSize], char *&ip) {
    RB_Instruction<RBC_Add>* p = (RB_Instruction<RBC_Add>*)ip;
    locals[p->dest] = locals[p->src1] + locals[p->src2];
    ip += sizeof(*p);
}
FORCE_INLINE static void handle_Sub(int locals[LocalStackSize], char *&ip) {
    RB_Instruction<RBC_Sub>* p = (RB_Instruction<RBC_Sub>*)ip;
    locals[p->dest] = locals[p->src1] - locals[p->src2];
    ip += sizeof(*p);
}
FORCE_INLINE static void handle_Mul(int locals[LocalStackSize], char *&ip) {
    RB_Instruction<RBC_Mul>* p = (RB_Instruction<RBC_Mul>*)ip;
    locals[p->dest] = locals[p->src1] * locals[p->src2];
    ip += sizeof(*p);
}
FORCE_INLINE static void handle_Div(int locals[LocalStackSize], char *&ip) {
    RB_Instruction<RBC_Div>* p = (RB_Instruction<RBC_Div>*)ip;
    locals[p->dest] = locals[p->src1] / locals[p->src2];
    ip += sizeof(*p);
}
FORCE_INLINE static void handle_EQ(int locals[LocalStackSize], char *&ip) {
    RB_Instruction<RBC_EQ>* p = (RB_Instruction<RBC_EQ>*)ip;
    locals[p->dest] = locals[p->src1] == locals[p->src2];
    ip += sizeof(*p);
}
FORCE_INLINE static void handle_NE(int locals[LocalStackSize], char *&ip) {
    RB_Instruction<RBC_NE>* p = (RB_Instruction<RBC_NE>*)ip;
    locals[p->dest] = locals[p->src1] != locals[p->src2];
    ip += sizeof(*p);
}
FORCE_INLINE static void handle_LoadInt(int locals[LocalStackSize], char *&ip) {
    RB_Instruction<RBC_LoadInt>* p = (RB_Instruction<RBC_LoadInt>*)ip;
    locals[p->dest] = p->i;
    ip += sizeof(*p);
}
FORCE_INLINE static void handle_Mov(int locals[LocalStackSize], char *&ip) {
    RB_Instruction<RBC_Mov>* p = (RB_Instruction<RBC_Mov>*)ip;
    locals[p->dest] = locals[p->src];
    ip += sizeof(*p);
}
FORCE_INLINE static void handle_Jmp(int locals[LocalStackSize], char *&ip) {
    ip += ((RB_Instruction<RBC_Jmp>*)ip)->jmpOff;
}
FORCE_INLINE static void handle_TJmp(int locals[LocalStackSize], char *&ip) {
    RB_Instruction<RBC_TJmp>* p = (RB_Instruction<RBC_TJmp>*)ip;
    if (locals[p->cond]) {
        ip += p->jmpOff;
    } else {
        ip += sizeof(*p);
    }
}
FORCE_INLINE static void handle_Repeat(int locals[LocalStackSize], char *&ip) {
    RB_Instruction<RBC_Repeat>* p = (RB_Instruction<RBC_Repeat>*)ip;
    if (locals[p->loopCounter] > 0) {
        --locals[p->loopCounter];
        locals[p->iter] += locals[p->step];
        ip += sizeof(*p);
    } else {
        ip += p->jmpOff;
    }
}
FORCE_INLINE static void handle_Nop(int locals[LocalStackSize], char *&ip) {
    ip += sizeof(RB_Instruction<RBC_Nop>);
}

class RB_Interpreter_CallThreading: public RB_Interpreter {
public:
    virtual int interpret(RB_InstructionList *insList) {
        static void(*handlers[])(int *, char*&) = {
            &handle_Add, 
            &handle_Sub, 
            &handle_Mul, 
            &handle_Div,
            &handle_EQ, 
            &handle_NE,
            &handle_LoadInt, 
            &handle_Mov,
            &handle_Jmp, 
            &handle_TJmp,
            &handle_Repeat, 
            &handle_Nop,
        };
        Byte* ip = &insList->getBytes()[0];
        int locals[LocalStackSize] = {1};
        while ((CodeType&)*ip != RBC_EOF) {
            handlers[(CodeType&)*ip](locals, ip);
        }
        return locals[0];
    }
    virtual bool isValid() { return true; }
private:
};

class RB_Interpreter_SwitchThreading: public RB_Interpreter {
public:
    virtual int interpret(RB_InstructionList *insList) {
        Byte* ip = &insList->getBytes()[0];
        int locals[LocalStackSize] = {1};
        for (;;) {
            switch ((CodeType&)*ip) {
                case RBC_Add: handle_Add(locals, ip); break;
                case RBC_Sub: handle_Sub(locals, ip); break;
                case RBC_Mul: handle_Mul(locals, ip); break;
                case RBC_Div: handle_Div(locals, ip); break;
                case RBC_EQ: handle_EQ(locals, ip); break;
                case RBC_NE: handle_NE(locals, ip); break;
                case RBC_LoadInt: handle_LoadInt(locals, ip); break;
                case RBC_Mov: handle_Mov(locals, ip); break;
                case RBC_Jmp: handle_Jmp(locals, ip); break;
                case RBC_TJmp: handle_TJmp(locals, ip); break;
                case RBC_Repeat: handle_Repeat(locals, ip); break;
                case RBC_Nop: handle_Nop(locals, ip); break;
                case RBC_EOF: return locals[0];
            }
        }
        return locals[0];
    }
    virtual bool isValid() { return true; }
private:
};

class RB_Interpreter_ReplicateSwitchThreading: public RB_Interpreter {
public:
    virtual int interpret(RB_InstructionList *insList) {
        Byte* ip = &insList->getBytes()[0];
        int locals[LocalStackSize] = {1};

#define NEXT() switch ((CodeType&)*ip) {\
                case RBC_Add: goto label_Add; \
                case RBC_Sub: goto label_Sub; \
                case RBC_Mul: goto label_Mul; \
                case RBC_Div: goto label_Div; \
                case RBC_EQ: goto label_EQ; \
                case RBC_NE: goto label_NE; \
                case RBC_LoadInt: goto label_LoadInt; \
                case RBC_Mov: goto label_Mov; \
                case RBC_Jmp: goto label_Jmp; \
                case RBC_TJmp: goto label_TJmp; \
                case RBC_Repeat: goto label_Repeat; \
                case RBC_Nop: goto label_Nop; \
                case RBC_EOF: goto label_EOF;\
            }

        NEXT();
label_Add: handle_Add(locals, ip);  NEXT();
label_Sub: handle_Sub(locals, ip);  NEXT();
label_Mul: handle_Mul(locals, ip);  NEXT();
label_Div: handle_Div(locals, ip);  NEXT();
label_EQ: handle_EQ(locals, ip);  NEXT();
label_NE: handle_NE(locals, ip);  NEXT();
label_LoadInt: handle_LoadInt(locals, ip);  NEXT();
label_Mov: handle_Mov(locals, ip);  NEXT();
label_Jmp: handle_Jmp(locals, ip);  NEXT();
label_TJmp: handle_TJmp(locals, ip);  NEXT();
label_Repeat: handle_Repeat(locals, ip);  NEXT();
label_Nop: handle_Nop(locals, ip);  NEXT();
label_EOF: return locals[0];

#undef NEXT

        return locals[0];
    }
    virtual bool isValid() { return true; }
private:
};

class RB_Interpreter_TokenThreading: public RB_Interpreter {
public:
    virtual int interpret(RB_InstructionList *insList) {
#ifdef __GNUC__
        Byte* ip = &insList->getBytes()[0];
        int locals[LocalStackSize] = {1};
        void* label_table[] = {
            &&label_Add,
            &&label_Sub,
            &&label_Mul,
            &&label_Div,
            &&label_EQ,
            &&label_NE,
            &&label_LoadInt,
            &&label_Mov,
            &&label_Jmp,
            &&label_TJmp,
            &&label_Repeat,
            &&label_Nop,
            &&label_EOF,
        };

#define NEXT() goto *label_table[(CodeType&)*ip]

        NEXT();
label_Add: handle_Add(locals, ip);  NEXT();
label_Sub: handle_Sub(locals, ip);  NEXT();
label_Mul: handle_Mul(locals, ip);  NEXT();
label_Div: handle_Div(locals, ip);  NEXT();
label_EQ: handle_EQ(locals, ip);  NEXT();
label_NE: handle_NE(locals, ip);  NEXT();
label_LoadInt: handle_LoadInt(locals, ip);  NEXT();
label_Mov: handle_Mov(locals, ip);  NEXT();
label_Jmp: handle_Jmp(locals, ip);  NEXT();
label_TJmp: handle_TJmp(locals, ip);  NEXT();
label_Repeat: handle_Repeat(locals, ip);  NEXT();
label_Nop: handle_Nop(locals, ip);  NEXT();
label_EOF: return locals[0];

#undef NEXT

           return locals[0];
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

class RB_Interpreter_DirectThreading: public RB_Interpreter {
public:
    virtual int interpret(RB_InstructionList *insList) {
#if defined(__GNUC__) && ((!defined(__x86_64__) && (CodeSize >= 4)) || (defined(__x86_64__) && (CodeSize >= 8) ))
        void* label_table[] = {
            &&label_Add,
            &&label_Sub,
            &&label_Mul,
            &&label_Div,
            &&label_EQ,
            &&label_NE,
            &&label_LoadInt,
            &&label_Mov,
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
            off += RB_getInsuctionSize(code);
        }

        Byte* ip = &bytes[0];
        int locals[LocalStackSize] = {1};

#define NEXT() goto **(void**)ip

        NEXT();
label_Add: handle_Add(locals, ip);  NEXT();
label_Sub: handle_Sub(locals, ip);  NEXT();
label_Mul: handle_Mul(locals, ip);  NEXT();
label_Div: handle_Div(locals, ip);  NEXT();
label_EQ: handle_EQ(locals, ip);  NEXT();
label_NE: handle_NE(locals, ip);  NEXT();
label_LoadInt: handle_LoadInt(locals, ip);  NEXT();
label_Mov: handle_Mov(locals, ip);  NEXT();
label_Jmp: handle_Jmp(locals, ip);  NEXT();
label_TJmp: handle_TJmp(locals, ip);  NEXT();
label_Repeat: handle_Repeat(locals, ip);  NEXT();
label_Nop: handle_Nop(locals, ip);  NEXT();
label_EOF: return locals[0];

#undef NEXT

        return locals[0];
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

class RB_Interpreter_JIT: public RB_Interpreter {
public:
    virtual int interpret(RB_InstructionList *insList) {
        return 0;
    }
    virtual bool isValid() { return false; }
private:
};

RB_Interpreter* RB_Interpreter::getInstance(const string &name) {
    if (name == "call") {
        return new RB_Interpreter_CallThreading();
    } else if (name == "switch") {
        return new RB_Interpreter_SwitchThreading();
    } else if (name == "repl_switch") {
        return new RB_Interpreter_ReplicateSwitchThreading();
    } else if (name == "token") {
        return new RB_Interpreter_TokenThreading();
    } else if (name == "direct") {
        return new RB_Interpreter_DirectThreading();
    } else if (name == "jit") {
        return new RB_Interpreter_JIT();
    } else {
        assert(0);
        return NULL;
    }
}
