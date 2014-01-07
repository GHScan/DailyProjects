
#include "pch.h"
#include "Utils.h"
#include "RegisterBasedInterpreter.h"
//==============================
enum RB_Code {
    RBC_Add, 
    RBC_Sub, 
    RBC_Mul, 
    RBC_Div,

    RBC_EQ, 
    RBC_NE,

    RBC_LoadInt,
    RBC_Mov,

    RBC_Jmp, 
    RBC_TJmp,

    RBC_Repeat, 

    RBC_Nop,
    RBC_EOF,

    RBC_COUNT,
};

#pragma pack(push, 1)
struct RB_Instruction_Add {
    CodeType code;
    LocalIdxType dest, src1, src2;
};
struct RB_Instruction_Sub {
    CodeType code;
    LocalIdxType dest, src1, src2;
};
struct RB_Instruction_Mul {
    CodeType code;
    LocalIdxType dest, src1, src2;
};
struct RB_Instruction_Div {
    CodeType code;
    LocalIdxType dest, src1, src2;
};
struct RB_Instruction_EQ {
    CodeType code;
    LocalIdxType dest, src1, src2;
};
struct RB_Instruction_NE {
    CodeType code;
    LocalIdxType dest, src1, src2;
};
struct RB_Instruction_LoadInt {
    CodeType code;
    LocalIdxType dest;
    int i;
};
struct RB_Instruction_Mov {
    CodeType code;
    LocalIdxType dest, src;
};
struct RB_Instruction_Jmp {
    CodeType code;
    JmpOffType jmpOff;
};
struct RB_Instruction_TJmp {
    CodeType code;
    LocalIdxType cond;
    JmpOffType jmpOff;
};
struct RB_Instruction_Repeat {
    CodeType code;
    LocalIdxType loopCounter, iter, step;
    JmpOffType jmpOff;
};
struct RB_Instruction_Nop {
    CodeType code;
};
struct RB_Instruction_EOF {
    CodeType code;
};
#pragma pack(pop)
//==============================
FORCE_INLINE static void handle_Add(int locals[LocalStackSize], char *&ip) {
    RB_Instruction_Add* p = (RB_Instruction_Add*)ip;
    locals[p->dest] = locals[p->src1] + locals[p->src2];
    ip += sizeof(*p);
}
FORCE_INLINE static void handle_Sub(int locals[LocalStackSize], char *&ip) {
    RB_Instruction_Sub* p = (RB_Instruction_Sub*)ip;
    locals[p->dest] = locals[p->src1] - locals[p->src2];
    ip += sizeof(*p);
}
FORCE_INLINE static void handle_Mul(int locals[LocalStackSize], char *&ip) {
    RB_Instruction_Mul* p = (RB_Instruction_Mul*)ip;
    locals[p->dest] = locals[p->src1] * locals[p->src2];
    ip += sizeof(*p);
}
FORCE_INLINE static void handle_Div(int locals[LocalStackSize], char *&ip) {
    RB_Instruction_Div* p = (RB_Instruction_Div*)ip;
    locals[p->dest] = locals[p->src1] / locals[p->src2];
    ip += sizeof(*p);
}
FORCE_INLINE static void handle_EQ(int locals[LocalStackSize], char *&ip) {
    RB_Instruction_EQ* p = (RB_Instruction_EQ*)ip;
    locals[p->dest] = locals[p->src1] == locals[p->src2];
    ip += sizeof(*p);
}
FORCE_INLINE static void handle_NE(int locals[LocalStackSize], char *&ip) {
    RB_Instruction_NE* p = (RB_Instruction_NE*)ip;
    locals[p->dest] = locals[p->src1] != locals[p->src2];
    ip += sizeof(*p);
}
FORCE_INLINE static void handle_LoadInt(int locals[LocalStackSize], char *&ip) {
    RB_Instruction_LoadInt* p = (RB_Instruction_LoadInt*)ip;
    locals[p->dest] = p->i;
    ip += sizeof(*p);
}
FORCE_INLINE static void handle_Mov(int locals[LocalStackSize], char *&ip) {
    RB_Instruction_Mov* p = (RB_Instruction_Mov*)ip;
    locals[p->dest] = locals[p->src];
    ip += sizeof(*p);
}
FORCE_INLINE static void handle_Jmp(int locals[LocalStackSize], char *&ip) {
    ip += ((RB_Instruction_Jmp*)ip)->jmpOff;
}
FORCE_INLINE static void handle_TJmp(int locals[LocalStackSize], char *&ip) {
    RB_Instruction_TJmp* p = (RB_Instruction_TJmp*)ip;
    if (locals[p->cond]) {
        ip += p->jmpOff;
    } else {
        ip += sizeof(*p);
    }
}
FORCE_INLINE static void handle_Repeat(int locals[LocalStackSize], char *&ip) {
    RB_Instruction_Repeat* p = (RB_Instruction_Repeat*)ip;
    if (locals[p->loopCounter] > 0) {
        --locals[p->loopCounter];
        locals[p->iter] += locals[p->step];
        ip += sizeof(*p);
    } else {
        ip += p->jmpOff;
    }
}
FORCE_INLINE static void handle_Nop(int locals[LocalStackSize], char *&ip) {
    ip += sizeof(RB_Instruction_Nop);
}
//==============================
class RB_Interpreter_CallThreading: public Interpreter {
public:
    RB_Interpreter_CallThreading(InterpreterFactory *factory): Interpreter(factory){}
    virtual int interpret(InstructionList *insList) {
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
        char* ip = &insList->getBytes()[0];
        int locals[LocalStackSize] = {1};
        while ((CodeType&)*ip != RBC_EOF) {
            handlers[(CodeType&)*ip](locals, ip);
        }
        return locals[0];
    }
    virtual bool isValid() { return true; }
private:
};

class RB_Interpreter_SwitchThreading: public Interpreter {
public:
    RB_Interpreter_SwitchThreading(InterpreterFactory *factory): Interpreter(factory){}
    virtual int interpret(InstructionList *insList) {
        char* ip = &insList->getBytes()[0];
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

class RB_Interpreter_ReplicateSwitchThreading: public Interpreter {
public:
    RB_Interpreter_ReplicateSwitchThreading(InterpreterFactory *factory): Interpreter(factory){}
    virtual int interpret(InstructionList *insList) {
        char* ip = &insList->getBytes()[0];
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

class RB_Interpreter_TokenThreading: public Interpreter {
public:
    RB_Interpreter_TokenThreading(InterpreterFactory *factory): Interpreter(factory){}
    virtual int interpret(InstructionList *insList) {
#ifdef __GNUC__
        char* ip = &insList->getBytes()[0];
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

class RB_Interpreter_DirectThreading: public Interpreter {
public:
    RB_Interpreter_DirectThreading(InterpreterFactory *factory): Interpreter(factory){}
    virtual int interpret(InstructionList *insList) {
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

        vector<char> bytes(insList->getBytes());
        for (int off = 0; off < (int)bytes.size(); ) {
            CodeType code = (CodeType&)bytes[off];
            (void*&)bytes[off] = label_table[code];
            off += m_factory->getInstructionSize(code);
        }

        char* ip = &bytes[0];
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

class RB_Interpreter_JIT: public Interpreter {
public:
    RB_Interpreter_JIT(InterpreterFactory *factory): Interpreter(factory){}
    virtual int interpret(InstructionList *insList) {
        return 0;
    }
    virtual bool isValid() { return false; }
private:
};
//==============================
class RB_InstructionList: public InstructionList {
public:
    RB_InstructionList(InterpreterFactory* factory): InstructionList(factory){}
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
                case RBC_Jmp: jmpOff = &((RB_Instruction_Jmp&)m_bytes[off]).jmpOff; break;
                case RBC_TJmp: jmpOff = &((RB_Instruction_TJmp&)m_bytes[off]).jmpOff; break;
                case RBC_Repeat: jmpOff = &((RB_Instruction_Repeat&)m_bytes[off]).jmpOff; break;
                default: break;
            }
            if (jmpOff != NULL) *jmpOff = insOffs[*jmpOff] - off;
            off += m_factory->getInstructionSize(code);
        }
    }
    virtual void appendEOF() {
        appendValue(RBC_EOF);
    }
};

//==============================
RB_InterpreterFactory::RB_InterpreterFactory() {
    InstructionMetaManager *mgr = &m_insMetaMgr;
    mgr->add(new InstructionMeta3<LocalIdxType, LocalIdxType, LocalIdxType>(RBC_Add, "add"));
    mgr->add(new InstructionMeta3<LocalIdxType, LocalIdxType, LocalIdxType>(RBC_Sub, "sub"));
    mgr->add(new InstructionMeta3<LocalIdxType, LocalIdxType, LocalIdxType>(RBC_Mul, "mul"));
    mgr->add(new InstructionMeta3<LocalIdxType, LocalIdxType, LocalIdxType>(RBC_Div, "div"));
    mgr->add(new InstructionMeta3<LocalIdxType, LocalIdxType, LocalIdxType>(RBC_EQ, "eq"));
    mgr->add(new InstructionMeta3<LocalIdxType, LocalIdxType, LocalIdxType>(RBC_NE, "ne"));
    mgr->add(new InstructionMeta2<LocalIdxType, int>(RBC_LoadInt, "loadint"));
    mgr->add(new InstructionMeta2<LocalIdxType, LocalIdxType>(RBC_Mov, "mov"));
    mgr->add(new InstructionMeta1<JmpOffType>(RBC_Jmp, "jmp"));
    mgr->add(new InstructionMeta2<LocalIdxType, JmpOffType>(RBC_TJmp, "tjmp"));
    mgr->add(new InstructionMeta4<LocalIdxType, LocalIdxType, LocalIdxType, JmpOffType>(RBC_Repeat, "repeat"));
    mgr->add(new InstructionMeta(RBC_Nop, "nop"));
    mgr->add(new InstructionMeta(RBC_EOF, "eof"));
}
Interpreter* RB_InterpreterFactory::createInterpreter(const string &name) {
    if (name == "call") {
        return new RB_Interpreter_CallThreading(this);
    } else if (name == "switch") {
        return new RB_Interpreter_SwitchThreading(this);
    } else if (name == "repl_switch") {
        return new RB_Interpreter_ReplicateSwitchThreading(this);
    } else if (name == "token") {
        return new RB_Interpreter_TokenThreading(this);
    } else if (name == "direct") {
        return new RB_Interpreter_DirectThreading(this);
    } else if (name == "jit") {
        return new RB_Interpreter_JIT(this);
    } else {
        assert(0);
        return NULL;
    }
}
InstructionList* RB_InterpreterFactory::createInstructionList() {
    return new RB_InstructionList(this);
}
