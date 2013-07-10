
#include "pch.h"

#include <limits.h>

#include <functional>

#include "BEx86JITEngine.h"
#include "BEx86FileBuilder.h"
#include "BEx86FunctionBuilder.h"
#include "BEConstant.h"
#include "BESymbolTable.h"
#include "BEType.h"
#include "BEStorage.h"

//==============================
static void* os_mallocExec(int size);
static void  os_freeExec(void *p);
static void* os_getFuncAddress(const char *funcName);
//==============================
#ifdef _MSC_VER
#pragma warning(disable : 4312)
#pragma warning(disable : 4311)
#include <Windows.h>
#include <Psapi.h>
#pragma warning(default : 4311)
#pragma warning(default : 4312)

static void* os_mallocExec(int size) {
    void *p = ::VirtualAlloc(NULL, size, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    ASSERT(p != NULL);
    return p;
}
static void  os_freeExec(void *p) {
    ASSERT(p != NULL);
	::VirtualFree(p, 0, MEM_RELEASE);
}
static void* os_getFuncAddress(const char *funcName) {
    HANDLE process = ::OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, GetCurrentProcessId());
    ASSERT(process != NULL);

    vector<HMODULE> modules;
    {
        DWORD bytes;
        ::EnumProcessModules(process, NULL, 0, &bytes);
        modules.resize(bytes / sizeof(modules[0]));
        ::EnumProcessModules(process, &modules[0], bytes, &bytes);
    }
    ASSERT(modules.size() > 0);

    void *func = NULL;
    for (auto module : modules) {
        if (func = ::GetProcAddress(module, funcName)) {
            break;
        }
    }

    CloseHandle(process);

    return func;
}
#endif
//==============================
#ifdef __linux__ 
#include <dlfcn.h>
#include <unistd.h>
#include <sys/mman.h>

static void* os_mallocExec(int size) {
    void *p = NULL;
    ::posix_memalign(&p, ::getpagesize(), size);
    ASSERT(p != NULL);
    int erro = ::mprotect(p, size, PROT_READ | PROT_WRITE | PROT_EXEC);
    ASSERT(erro == 0);
    return p;
}
static void  os_freeExec(void *p) {
    ASSERT(p != NULL);
    ::free(p);
}
static void* os_getFuncAddress(const char *funcName) {
    void *m = ::dlopen(NULL, RTLD_NOW);
    void *r = ::dlsym(m, funcName);
    ::dlclose(m);
    return r;
}
#endif
//==============================
class ExecMemoryStream : public Noncopyable {
public:
    ExecMemoryStream(int size): m_size(size), m_off(0) { m_base = (char*)os_mallocExec(size); }
    ~ExecMemoryStream() { os_freeExec(m_base); }
    char* getBasePtr() { return m_base; }
    char* getCurrentPtr() { return m_base + m_off; }
    int getCurrentOff() const { return m_off; }
    int getSize() const { return m_size; }
    void pushInt8(char c) { /*printf("0x%0x,", (unsigned char)c);*/ m_base[m_off] = c; m_off += 1; }
    void pushInt32(int i) { (int&)m_base[m_off] = i; /*for(int j=0; j < 4; ++j)printf("0x%0x,", ((unsigned char*)&i)[j]);; */m_off += 4; }
    void setListener(function<void(int)> listener) { m_listener = listener; }
    void notifyListener(int event) { if (m_listener != NULL) m_listener(event); }
private:
    char* m_base;
    int m_size, m_off;
    function<void(int)> m_listener;
}; 
static_assert(sizeof(int) == 4, "");
//==============================
class BitOutStream : public Noncopyable {
public:
    BitOutStream(ExecMemoryStream *out): m_out(out) {}
    ~BitOutStream() { flush(); }
    BitOutStream& push(const char *bitStrLiteral) {
        m_literals.push_back(bitStrLiteral);
        return *this;
    }
    void flush() {
        if (m_literals.empty()) return;

        int i = 0;
        char c = 0;
        for (auto str : m_literals) {
            for (const char *end = str + strlen(str); str < end; ++str) { 
                if (*str == '0' || *str == '1') {
                    c |= (*str == '1') << (7 - i);
                    i = (i + 1) & 7;
                    if (i == 0) {
                        m_out->pushInt8(c);
                        c = 0;
                    }
                }
            }
        }
        ASSERT(i == 0);

        m_literals.clear();
    }
private:
    ExecMemoryStream *m_out;
    vector<const char*> m_literals;
};
//==============================
enum ia32_AddressType {
    ia32_AT_Null,
    ia32_AT_Register,
    //ia32_AT_Immediate8,
    ia32_AT_Immediate32,
    ia32_AT_Addr,
    ia32_AT_RegisterAsAddr,
    ia32_AT_RegisterDisp8AsAddr,
    ia32_AT_RegisterDisp32AsAddr,
};
struct ia32_InstructionOperand {
    ia32_AddressType type;
    BEx86RegisterType reg;
    int displacement;
};
//==============================
static const char* reg2BitString(BEx86RegisterType reg) {
    switch (reg) {
        case x86RT_EAX: return "000";
        case x86RT_EBX: return "011";
        case x86RT_ECX: return "001";
        case x86RT_EDX: return "010";
        case x86RT_ESI: return "110";
        case x86RT_EDI: return "111";
        case x86RT_ESP: return "100";
        case x86RT_EBP: return "101";
        default: ASSERT(0);
    }
    return "";
}
static const char* condTest2BitString(BEx86InstructionType type) {
    switch (type) {
        case x86IT_JE: return "0100";
        case x86IT_JNE: return "0101";
        case x86IT_JG: return "1111";
        case x86IT_JGE: return "1101";
        case x86IT_JL: return "1100";
        case x86IT_JLE: return "1110";
        default: ASSERT(0);
    }
    return "";
}
static bool isMemoryAddressType(ia32_AddressType type) {
    return type == ia32_AT_Addr || type == ia32_AT_RegisterAsAddr || type == ia32_AT_RegisterDisp8AsAddr || type == ia32_AT_RegisterDisp32AsAddr;
}
static void encodeMemoryWithModRM(ExecMemoryStream *out, const char *regOpStr, ia32_InstructionOperand mem) {
    ASSERT(isMemoryAddressType(mem.type));
    if (mem.type == ia32_AT_Addr) {
        BitOutStream(out).push("00").push(regOpStr).push("101");
        out->pushInt32(mem.displacement);
    } else if (mem.type == ia32_AT_RegisterDisp8AsAddr) {
        ASSERT(mem.reg == x86RT_EBP); // support [ebp+n] only
        BitOutStream(out).push("01").push(regOpStr).push("101");
        out->pushInt8((char)mem.displacement);
    } else if (mem.type == ia32_AT_RegisterDisp32AsAddr) {
        ASSERT(mem.reg == x86RT_EBP); // support [ebp+n] only
        BitOutStream(out).push("10").push(regOpStr).push("101");
        out->pushInt32(mem.displacement);
    } else {
        ASSERT(0); // currently we do not support other case
    }
}
//==============================
static const int EVENT_JMP_TARGET = 0;
//==============================
static void encodeInstruction_MOV(ExecMemoryStream *out, ia32_InstructionOperand to, ia32_InstructionOperand from) {
    if (from.type == ia32_AT_Register && to.type == ia32_AT_Register) {
        BitOutStream(out).push("1000 1001 11").push(reg2BitString(from.reg)).push(reg2BitString(to.reg));
    } else if (from.type == ia32_AT_Addr && to.type == ia32_AT_Register && to.reg == x86RT_EAX) {
        BitOutStream(out).push("1010 0001");
        out->pushInt32(from.displacement);
    } else if (isMemoryAddressType(from.type) && to.type == ia32_AT_Register) {
        BitOutStream(out).push("1000 1011");
        encodeMemoryWithModRM(out, reg2BitString(to.reg), from);
    } else if (from.type == ia32_AT_Register && to.type == ia32_AT_Addr && from.reg == x86RT_EAX) {
        BitOutStream(out).push("1010 0011");
        out->pushInt32(to.displacement);
    } else if (from.type == ia32_AT_Register && isMemoryAddressType(to.type)) {
        BitOutStream(out).push("1000 1001");
        encodeMemoryWithModRM(out, reg2BitString(from.reg), to);
    } else if (from.type == ia32_AT_Immediate32 && to.type == ia32_AT_Register) {
        BitOutStream(out).push("1011 1").push(reg2BitString(to.reg));
        out->pushInt32(from.displacement);
    } else if (from.type == ia32_AT_Immediate32 && isMemoryAddressType(to.type)) {
        BitOutStream(out).push("1100 0111");
        encodeMemoryWithModRM(out, "000", to);
        out->pushInt32(from.displacement);
    }
}
static void encodeInstruction_LEA(ExecMemoryStream *out, ia32_InstructionOperand a, ia32_InstructionOperand b) {
    ASSERT(0);
}
static void encodeInstruction_AND(ExecMemoryStream *out, ia32_InstructionOperand to, ia32_InstructionOperand from) {
    if (from.type == ia32_AT_Register && to.type == ia32_AT_Register) {
        BitOutStream(out).push("0010 0001 11").push(reg2BitString(from.reg)).push(reg2BitString(to.reg));
    } else if (isMemoryAddressType(from.type) && to.type == ia32_AT_Register) {
        BitOutStream(out).push("0010 0011");
        encodeMemoryWithModRM(out, reg2BitString(to.reg), from);
    } else if (from.type == ia32_AT_Register && isMemoryAddressType(to.type)) {
        BitOutStream(out).push("0010 0001");
        encodeMemoryWithModRM(out, reg2BitString(from.reg), to);
    } else if (from.type == ia32_AT_Immediate32 && to.type == ia32_AT_Register && to.reg == x86RT_EAX) {
        BitOutStream(out).push("0010 0101");
        out->pushInt32(from.displacement);
    } else if (from.type == ia32_AT_Immediate32 && to.type == ia32_AT_Register) {
        BitOutStream(out).push("1000 0001 1110 0").push(reg2BitString(to.reg));
        out->pushInt32(from.displacement);
    } else if (from.type == ia32_AT_Immediate32 && isMemoryAddressType(to.type)) {
        BitOutStream(out).push("1000 0001");
        encodeMemoryWithModRM(out, "100", to);
        out->pushInt32(from.displacement);
    } else {
        ASSERT(0);
    }
}
static void encodeInstruction_OR(ExecMemoryStream *out, ia32_InstructionOperand a, ia32_InstructionOperand b) {
    ASSERT(0);
}
static void encodeInstruction_NOT(ExecMemoryStream *out, ia32_InstructionOperand a, ia32_InstructionOperand b) {
    ASSERT(0);
}
static void encodeInstruction_INC(ExecMemoryStream *out, ia32_InstructionOperand to, ia32_InstructionOperand from) {
    if (to.type == ia32_AT_Register) {
        BitOutStream(out).push("0100 0").push(reg2BitString(to.reg));
    } else if (isMemoryAddressType(to.type)) {
        BitOutStream(out).push("1111 1111");
        encodeMemoryWithModRM(out, "000", to);
    } else {
        ASSERT(0);
    }
}
static void encodeInstruction_DEC(ExecMemoryStream *out, ia32_InstructionOperand to, ia32_InstructionOperand from) {
    if (to.type == ia32_AT_Register) {
        BitOutStream(out).push("0100 1").push(reg2BitString(to.reg));
    } else if (isMemoryAddressType(to.type)) {
        BitOutStream(out).push("1111 1111");
        encodeMemoryWithModRM(out, "001", to);
    } else {
        ASSERT(0);
    }
}
static void encodeInstruction_ADD(ExecMemoryStream *out, ia32_InstructionOperand to, ia32_InstructionOperand from) {
    if (from.type == ia32_AT_Register && to.type == ia32_AT_Register) {
        BitOutStream(out).push("0000 0001 11").push(reg2BitString(from.reg)).push(reg2BitString(to.reg));
    } else if (isMemoryAddressType(from.type) && to.type == ia32_AT_Register) {
        BitOutStream(out).push("0000 0011");
        encodeMemoryWithModRM(out, reg2BitString(to.reg), from);
    } else if (from.type == ia32_AT_Register && isMemoryAddressType(to.type)) {
        BitOutStream(out).push("0000 0001");
        encodeMemoryWithModRM(out, reg2BitString(from.reg), to);
    } else if (from.type == ia32_AT_Immediate32 && to.type == ia32_AT_Register && to.reg == x86RT_EAX) {
        BitOutStream(out).push("0000 0101");
        out->pushInt32(from.displacement);
    } else if (from.type == ia32_AT_Immediate32 && to.type == ia32_AT_Register) {
        BitOutStream(out).push("1000 0001 1100 0").push(reg2BitString(to.reg));
        out->pushInt32(from.displacement);
    } else if (from.type == ia32_AT_Immediate32 && isMemoryAddressType(to.type)) {
        BitOutStream(out).push("1000 0001");
        encodeMemoryWithModRM(out, "000", to);
        out->pushInt32(from.displacement);
    } else {
        ASSERT(0);
    }
}
static void encodeInstruction_SUB(ExecMemoryStream *out, ia32_InstructionOperand to, ia32_InstructionOperand from) {
    if (from.type == ia32_AT_Register && to.type == ia32_AT_Register) {
        BitOutStream(out).push("0010 1001 11").push(reg2BitString(from.reg)).push(reg2BitString(to.reg));
    } else if (isMemoryAddressType(from.type) && to.type == ia32_AT_Register) {
        BitOutStream(out).push("0010 1011");
        encodeMemoryWithModRM(out, reg2BitString(to.reg), from);
    } else if (from.type == ia32_AT_Register && isMemoryAddressType(to.type)) {
        BitOutStream(out).push("0010 1001");
        encodeMemoryWithModRM(out, reg2BitString(from.reg), to);
    } else if (from.type == ia32_AT_Immediate32 && to.type == ia32_AT_Register && to.reg == x86RT_EAX) {
        BitOutStream(out).push("0010 1101");
        out->pushInt32(from.displacement);
    } else if (from.type == ia32_AT_Immediate32 && to.type == ia32_AT_Register) {
        BitOutStream(out).push("1000 0001 1110 1").push(reg2BitString(to.reg));
        out->pushInt32(from.displacement);
    } else if (from.type == ia32_AT_Immediate32 && isMemoryAddressType(to.type)) {
        BitOutStream(out).push("1000 0001");
        encodeMemoryWithModRM(out, "101", to);
        out->pushInt32(from.displacement);
    } else {
        ASSERT(0);
    }
}
static void encodeInstruction_MUL(ExecMemoryStream *out, ia32_InstructionOperand to, ia32_InstructionOperand from) {
    if (from.type == ia32_AT_Register && to.type == ia32_AT_Register && from.reg == x86RT_EAX) {
        BitOutStream(out).push("1111 0111 1110 1").push(reg2BitString(to.reg));
    } else if (from.type == ia32_AT_Register && isMemoryAddressType(to.type) && from.reg == x86RT_EAX) {
        BitOutStream(out).push("1111 0111");
        encodeMemoryWithModRM(out, "101", to);
    } else if (from.type == ia32_AT_Register && to.type == ia32_AT_Register) {
        BitOutStream(out).push("0000 1111 1010 1111 11").push(reg2BitString(from.reg)).push(reg2BitString(to.reg));
    } else if (from.type == ia32_AT_Register && isMemoryAddressType(to.type)) {
        BitOutStream(out).push("0000 1111 1010 1111");
        encodeMemoryWithModRM(out, reg2BitString(from.reg), to);
    } else if (from.type == ia32_AT_Immediate32 && to.type == ia32_AT_Register) {
        BitOutStream(out).push("0110 1001 11").push(reg2BitString(to.reg)).push(reg2BitString(to.reg));
        out->pushInt32(from.displacement);
    } else if (isMemoryAddressType(from.type) && to.type == ia32_AT_Register) {
        BitOutStream(out).push("0000 1111 1010 1111");
        encodeMemoryWithModRM(out, reg2BitString(to.reg), from);
    } else {
        ASSERT(0);
    }
}
static void encodeInstruction_DIV(ExecMemoryStream *out, ia32_InstructionOperand to, ia32_InstructionOperand from) {
    if (to.type == ia32_AT_Register) {
        BitOutStream(out).push("1111 0111 1111 0").push(reg2BitString(to.reg));
    } else if (isMemoryAddressType(to.type)) {
        BitOutStream(out).push("1111 0111");
        encodeMemoryWithModRM(out, "110", to);
    } else {
        ASSERT(0);
    }
}
static void encodeInstruction_SAL(ExecMemoryStream *out, ia32_InstructionOperand to, ia32_InstructionOperand from) {
    if (to.type == ia32_AT_Register && from.type == ia32_AT_Immediate32) {
        if (from.displacement == 1) {
            BitOutStream(out).push("1101 0001 1110 0").push(reg2BitString(to.reg));
        } else {
            ASSERT(from.displacement >= CHAR_MIN && from.displacement <= CHAR_MAX);
            BitOutStream(out).push("1100 0001 1110 0").push(reg2BitString(to.reg));
            out->pushInt8((char)from.displacement);
        }
    } else if (isMemoryAddressType(to.type) && from.type == ia32_AT_Immediate32) {
        if (from.displacement == 1) {
            BitOutStream(out).push("1101 0001");
            encodeMemoryWithModRM(out, "100", to);
        } else {
            ASSERT(from.displacement >= CHAR_MIN && from.displacement <= CHAR_MAX);
            BitOutStream(out).push("1100 0001");
            encodeMemoryWithModRM(out, "100", to);
            out->pushInt8((char)from.displacement);
        }
    } else {
        ASSERT(0);
    }
}
static void encodeInstruction_SAR(ExecMemoryStream *out, ia32_InstructionOperand to, ia32_InstructionOperand from) {
    if (to.type == ia32_AT_Register && from.type == ia32_AT_Immediate32) {
        if (from.displacement == 1) {
            BitOutStream(out).push("1101 0001 1111 1").push(reg2BitString(to.reg));
        } else {
            ASSERT(from.displacement >= CHAR_MIN && from.displacement <= CHAR_MAX);
            BitOutStream(out).push("1100 0001 1111 1").push(reg2BitString(to.reg));
            out->pushInt8((char)from.displacement);
        }
    } else if (isMemoryAddressType(to.type) && from.type == ia32_AT_Immediate32) { 
        if (from.displacement == 1)  {
            BitOutStream(out).push("1101 0001");
            encodeMemoryWithModRM(out, "111", to);
        } else {
            ASSERT(from.displacement >= CHAR_MIN && from.displacement <= CHAR_MAX);
            BitOutStream(out).push("1100 0001");
            encodeMemoryWithModRM(out, "111", to);
            out->pushInt8((char)from.displacement);
        }
    } else {
        ASSERT(0);
    }
}
static void encodeInstruction_XOR(ExecMemoryStream *out, ia32_InstructionOperand to, ia32_InstructionOperand from) {
    if (from.type == ia32_AT_Register && to.type == ia32_AT_Register) {
        BitOutStream(out).push("0011 0001 11").push(reg2BitString(from.reg)).push(reg2BitString(to.reg));
    } else if (isMemoryAddressType(from.type) && to.type == ia32_AT_Register) {
        BitOutStream(out).push("0011 0011");
        encodeMemoryWithModRM(out, reg2BitString(to.reg), from);
    } else if (from.type == ia32_AT_Register && isMemoryAddressType(to.type)) {
        BitOutStream(out).push("0011 0001");
        encodeMemoryWithModRM(out, reg2BitString(from.reg), to);
    } else if (from.type == ia32_AT_Immediate32 && to.type == ia32_AT_Register && to.reg == x86RT_EAX) {
        BitOutStream(out).push("0011 0101");
        out->pushInt32(from.displacement);
    } else if (from.type == ia32_AT_Immediate32 && to.type == ia32_AT_Register) {
        BitOutStream(out).push("1000 0001 1111 0").push(reg2BitString(to.reg));
        out->pushInt32(from.displacement);
    } else if (from.type == ia32_AT_Immediate32 && isMemoryAddressType(to.type)) {
        BitOutStream(out).push("1000 0001");
        encodeMemoryWithModRM(out, "110", to);
        out->pushInt32(from.displacement);
    } else {
        ASSERT(0);
    }
}
static void encodeInstruction_CMP(ExecMemoryStream *out, ia32_InstructionOperand to, ia32_InstructionOperand from) {
    if (from.type == ia32_AT_Register && to.type == ia32_AT_Register) {
        BitOutStream(out).push("0011 1001 11").push(reg2BitString(from.reg)).push(reg2BitString(to.reg));
    } else if (isMemoryAddressType(from.type) && to.type == ia32_AT_Register) {
        BitOutStream(out).push("0011 1011");
        encodeMemoryWithModRM(out, reg2BitString(to.reg), from);
    } else if (from.type == ia32_AT_Register && isMemoryAddressType(to.type)) {
        BitOutStream(out).push("0011 1001");
        encodeMemoryWithModRM(out, reg2BitString(from.reg), to);
    } else if (from.type == ia32_AT_Immediate32 && to.type == ia32_AT_Register && to.reg == x86RT_EAX) {
        BitOutStream(out).push("0011 1101");
        out->pushInt32(from.displacement);
    } else if (from.type == ia32_AT_Immediate32 && to.type == ia32_AT_Register) {
        BitOutStream(out).push("1000 0001 1111 1").push(reg2BitString(to.reg));
        out->pushInt32(from.displacement);
    } else if (from.type == ia32_AT_Immediate32 && isMemoryAddressType(to.type)) {
        BitOutStream(out).push("1000 0001");
        encodeMemoryWithModRM(out, "111", to);
        out->pushInt32(from.displacement);
    } else {
        ASSERT(0);
    }
}
static void encodeInstruction_NOP(ExecMemoryStream *out, ia32_InstructionOperand a, ia32_InstructionOperand b) {
    ASSERT(0);
}
static void encodeInstruction_PUSH(ExecMemoryStream *out, ia32_InstructionOperand to, ia32_InstructionOperand from) {
    if (to.type == ia32_AT_Register) {
        BitOutStream(out).push("0101 0").push(reg2BitString(to.reg));
    } else if (isMemoryAddressType(to.type)) {
        BitOutStream(out).push("1111 1111");
        encodeMemoryWithModRM(out, "110", to);
    } else if (to.type == ia32_AT_Immediate32) {
        BitOutStream(out).push("0110 1000");
        out->pushInt32(to.displacement);
    } else {
        ASSERT(0);
    }
}
static void encodeInstruction_POP(ExecMemoryStream *out, ia32_InstructionOperand to, ia32_InstructionOperand from) {
    if (to.type == ia32_AT_Register) {
        BitOutStream(out).push("0101 1").push(reg2BitString(to.reg));
    } else if (isMemoryAddressType(to.type)) {
        BitOutStream(out).push("1000 1111");
        encodeMemoryWithModRM(out, "000", to);
    } else {
        ASSERT(0);
    }
}
static void encodeInstruction_RET(ExecMemoryStream *out, ia32_InstructionOperand to, ia32_InstructionOperand from) {
    BitOutStream(out).push("1100 0011");
}
static void encodeInstruction_CALL(ExecMemoryStream *out, ia32_InstructionOperand to, ia32_InstructionOperand from) {
    if (isMemoryAddressType(to.type)) {
        BitOutStream(out).push("1111 1111");
        encodeMemoryWithModRM(out, "010", to);
    } else {
        ASSERT(0);
    }
}

static void encodeInstruction_JMP(ExecMemoryStream *out, ia32_InstructionOperand to, ia32_InstructionOperand from) {
    ASSERT(to.type == ia32_AT_Immediate32);
    BitOutStream(out).push("1110 1001");
    out->pushInt32(to.displacement);
    out->notifyListener(EVENT_JMP_TARGET);
}
static void encodeInstruction_CondJMP(BEx86InstructionType insType, ExecMemoryStream *out, ia32_InstructionOperand to, ia32_InstructionOperand from) {
    ASSERT(to.type == ia32_AT_Immediate32);
    BitOutStream(out).push("0000 1111 1000").push(condTest2BitString(insType));
    out->pushInt32(to.displacement);
    out->notifyListener(EVENT_JMP_TARGET);
}
//==============================
class BEx86JITEngineImpl: public Noncopyable {
public:
    BEx86JITEngineImpl(BEx86FileBuilder* fileBuilder);
    ~BEx86JITEngineImpl();
    void* getSymbol(const string &name);
    void dumpCode(ostream &so);
private:
    ia32_InstructionOperand toIA32InstructionOperand(BEx86FunctionBuilder *funcBuilder, const BEx86Operand &operand);
    void lookupExternSymbols(BEx86FileBuilder *fileBuilder);
    void buildDataSection(BEx86FileBuilder *fileBuilder);
    void buildTextSection(BEx86FileBuilder *fileBuilder);
    int calcDataSectionSize(BEx86FileBuilder *fileBuilder);
    int calcTextSectionSize(BEx86FileBuilder *fileBuilder);
private:
    map<string, void*> m_globals;
    map<const BEConstant*, void*> m_constStrs;
    ExecMemoryStream *m_textSection;
    vector<char> m_dataSection;
    map<string, void*> m_funcEntries;
};

ia32_InstructionOperand BEx86JITEngineImpl::toIA32InstructionOperand(BEx86FunctionBuilder *funcBuilder, const BEx86Operand &operand) {
    if (operand.type == x86OT_Null || operand.type == x86OT_BasicBlock) {
        ia32_InstructionOperand r = {ia32_AT_Null};
        return r;
    } else if (operand.type == x86OT_Register) {
        ia32_InstructionOperand r = {ia32_AT_Register, (BEx86RegisterType)operand.reg->regType, 0};
        return r;
    } else if (operand.type == x86OT_Memory) {
        BESymbol *symbol = operand.symbol;
        if (symbol->parent == funcBuilder->getArgSymbolTable()) {
            int regInUse = 0; // this calculation can be do once only
            for (int i = 1; i < x86RT_GRCount; ++i) {
                if (funcBuilder->getRegister(i)->isWritten) ++regInUse;
            }
            int off = (regInUse + 1 + 1) * 4 + symbol->off;
            if (off >= CHAR_MIN && off <= CHAR_MAX) {
                ia32_InstructionOperand r = {ia32_AT_RegisterDisp8AsAddr, x86RT_EBP, (char)off};
                return r;
            } else {
                ia32_InstructionOperand r = {ia32_AT_RegisterDisp32AsAddr, x86RT_EBP, off};
                return r;
            }
        } else if (symbol->parent == funcBuilder->getParent()->getGlobalSymbolTable()) { 
            if (symbol->type == BETypeManager::instance()->getFunc()) {
                ia32_InstructionOperand r = {ia32_AT_Addr, (BEx86RegisterType)0, force_cast<int>(m_funcEntries[symbol->name])};
                return r;
            } else if (dynamic_cast<const BEType_Array*>(symbol->type)) {
                ia32_InstructionOperand r = {ia32_AT_Immediate32, (BEx86RegisterType)0, force_cast<int>(m_globals[symbol->name])};
                return r;
            } else {
                ia32_InstructionOperand r = {ia32_AT_Addr, (BEx86RegisterType)0, force_cast<int>(m_globals[symbol->name])};
                return r;
            }
        } else {
            int off = -(symbol->off - funcBuilder->getMaxArgOff() + 4);
            if (off >= CHAR_MIN && off <= CHAR_MAX) {
                ia32_InstructionOperand r = {ia32_AT_RegisterDisp8AsAddr, x86RT_EBP, (char)off};
                return r;
            } else {
                ia32_InstructionOperand r = {ia32_AT_RegisterDisp32AsAddr, x86RT_EBP, off};
                return r;
            }
        }
    } else if (operand.type == x86OT_Constant) {
        if (auto p = dynamic_cast<const BEConstantInt*>(operand.constant)) {
            if (p->num >= CHAR_MIN && p->num <= CHAR_MAX) {
                // ia32_InstructionOperand r = {ia32_AT_Immediate8, (BEx86RegisterType)0, (char)p->num};
                ia32_InstructionOperand r = {ia32_AT_Immediate32, (BEx86RegisterType)0, p->num};
                return r;
            } else {
                ia32_InstructionOperand r = {ia32_AT_Immediate32, (BEx86RegisterType)0, p->num};
                return r;
            }
        } else if (auto p = dynamic_cast<const BEConstantString*>(operand.constant)) {
            ia32_InstructionOperand r = {ia32_AT_Immediate32, (BEx86RegisterType)0, force_cast<int>(m_constStrs[p])};
            return r;
        } else ASSERT(0);
    } else {
        ASSERT(0);
    }
}
void BEx86JITEngineImpl::lookupExternSymbols(BEx86FileBuilder *fileBuilder) {
    for (auto symbol : fileBuilder->getGlobalSymbolTable()->getSymbols()) {
        if (!fileBuilder->isExternSymbol(symbol->name)) continue;
        if (symbol->type == BETypeManager::instance()->getFunc()) {
            void *func = os_getFuncAddress(symbol->name.c_str());
            ASSERT(func != NULL);
            m_globals[symbol->name] = func;
            *(void**)m_funcEntries[symbol->name] = func;
        } else {
            ASSERT(0);
        }
    }
}
void BEx86JITEngineImpl::buildDataSection(BEx86FileBuilder *fileBuilder) {
    m_dataSection.resize(calcDataSectionSize(fileBuilder));
    char *p = &m_dataSection[0];
    for (auto &strPair : fileBuilder->getConstantPool()->getStrings()) {
        m_constStrs[&strPair.second] = p;
        strcpy(p, strPair.first.c_str());
        p += strPair.first.size() + 1;
    }
    for (auto symbol : fileBuilder->getGlobalSymbolTable()->getSymbols()) {
        if (fileBuilder->isExternSymbol(symbol->name)) continue;
        if (fileBuilder->getFunctionBuilder(symbol->name)) continue;
        m_globals[symbol->name] = p;
        if (auto array = dynamic_cast<const BEType_Array*>(symbol->type)) {
            p += array->elemType->size * array->count;
        } else {
            p += symbol->type->size;
        }
    }
    for (auto symbol : fileBuilder->getGlobalSymbolTable()->getSymbols()) {
        if (symbol->type != BETypeManager::instance()->getFunc()) continue;
        m_funcEntries[symbol->name] = p;
        p += 4;
    }
    ASSERT(p - &m_dataSection[0] < (int)m_dataSection.size());
}
void BEx86JITEngineImpl::buildTextSection(BEx86FileBuilder *fileBuilder) {
    m_textSection = new ExecMemoryStream(calcTextSectionSize(fileBuilder));

    for (auto symbol : fileBuilder->getGlobalSymbolTable()->getSymbols()) {
        BEx86FunctionBuilder *funcBuilder = fileBuilder->getFunctionBuilder(symbol->name);
        if (funcBuilder == NULL) continue;

        m_globals[symbol->name] = m_textSection->getCurrentPtr();
        *(void**)m_funcEntries[symbol->name] = m_textSection->getCurrentPtr();

        map<BEx86BasicBlock*, vector<char*> > jmpSource;
        map<BEx86BasicBlock*, char*> basicBlock2Ptr;

        for (auto basicBlock : funcBuilder->getBasicBlocks()) {
            basicBlock2Ptr[basicBlock] = m_textSection->getCurrentPtr();

            for (auto &ins : basicBlock->instructions) {
                ia32_InstructionOperand operands[] = {
                    toIA32InstructionOperand(funcBuilder, ins.operands[0]), 
                    toIA32InstructionOperand(funcBuilder, ins.operands[1])};
                switch (ins.type) {
                    case x86IT_MOV: encodeInstruction_MOV(m_textSection, operands[0], operands[1]); break;
                    case x86IT_LEA: encodeInstruction_LEA(m_textSection, operands[0], operands[1]); break;
                    case x86IT_AND: encodeInstruction_AND(m_textSection, operands[0], operands[1]); break;
                    case x86IT_OR:  encodeInstruction_OR(m_textSection, operands[0], operands[1]); break;
                    case x86IT_NOT:  encodeInstruction_NOT(m_textSection, operands[0], operands[1]); break;
                    case x86IT_INC:  encodeInstruction_INC(m_textSection, operands[0], operands[1]); break;
                    case x86IT_DEC:  encodeInstruction_DEC(m_textSection, operands[0], operands[1]); break;
                    case x86IT_ADD:  encodeInstruction_ADD(m_textSection, operands[0], operands[1]); break;
                    case x86IT_SUB:  encodeInstruction_SUB(m_textSection, operands[0], operands[1]); break;
                    case x86IT_MUL:  encodeInstruction_MUL(m_textSection, operands[0], operands[1]); break;
                    case x86IT_DIV:  encodeInstruction_DIV(m_textSection, operands[0], operands[1]); break;
                    case x86IT_SAL:  encodeInstruction_SAL(m_textSection, operands[0], operands[1]); break;
                    case x86IT_SAR:  encodeInstruction_SAR(m_textSection, operands[0], operands[1]); break;
                    case x86IT_XOR:  encodeInstruction_XOR(m_textSection, operands[0], operands[1]); break;
                    case x86IT_CMP:  encodeInstruction_CMP(m_textSection, operands[0], operands[1]); break;
                    case x86IT_NOP:  encodeInstruction_NOP(m_textSection, operands[0], operands[1]); break;
                    case x86IT_PUSH:  encodeInstruction_PUSH(m_textSection, operands[0], operands[1]); break;
                    case x86IT_POP:  encodeInstruction_POP(m_textSection, operands[0], operands[1]); break;
                    case x86IT_RET:  encodeInstruction_RET(m_textSection, operands[0], operands[1]); break;
                    case x86IT_CALL:  encodeInstruction_CALL(m_textSection, operands[0], operands[1]); break;
                    
                    case x86IT_JMP: 
                    case x86IT_JE: 
                    case x86IT_JNE: 
                    case x86IT_JG: 
                    case x86IT_JGE: 
                    case x86IT_JL: 
                    case x86IT_JLE: {
                        m_textSection->setListener([this, &ins, &jmpSource](int event){
                            if (event == EVENT_JMP_TARGET) {
                                jmpSource[ins.operands[0].basicBlock].push_back(m_textSection->getCurrentPtr() - 4);
                            }
                        });
                        operands[0].type = ia32_AT_Immediate32;
                        operands[0].displacement = 0;
                        if (ins.type == x86IT_JMP) encodeInstruction_JMP(m_textSection, operands[0], operands[1]);
                        else encodeInstruction_CondJMP(ins.type, m_textSection, operands[0], operands[1]);
                     }
                    break;

                    default: ASSERT(0);
                }
            }
        }

        for (auto blockPtr : basicBlock2Ptr) {
            for (auto src : jmpSource[blockPtr.first]) {
                *(int*)src = int(blockPtr.second - (src + 4));
            }
        }
    }
}
int BEx86JITEngineImpl::calcDataSectionSize(BEx86FileBuilder *fileBuilder) {
    return 1024 * 16; // TODO: 
}
int BEx86JITEngineImpl::calcTextSectionSize(BEx86FileBuilder *fileBuilder) {
    return 1024 * 128; // TODO:
}

BEx86JITEngineImpl::BEx86JITEngineImpl(BEx86FileBuilder* fileBuilder): 
    m_textSection(NULL) {
    buildDataSection(fileBuilder);
    lookupExternSymbols(fileBuilder);
    buildTextSection(fileBuilder);
}
BEx86JITEngineImpl::~BEx86JITEngineImpl() {
    delete m_textSection;
}
void* BEx86JITEngineImpl::getSymbol(const string &name) {
    auto iter = m_globals.find(name);
    return iter == m_globals.end() ? os_getFuncAddress(name.c_str()) : iter->second;
}
void BEx86JITEngineImpl::dumpCode(ostream &so) {
    int size = m_textSection->getCurrentOff();
    for (int i = 0; i < size; ++i) {
        so << format("0x%02x,", (unsigned char)m_textSection->getBasePtr()[i]);
    }
}
//==============================
BEx86JITEngine::BEx86JITEngine(BEx86FileBuilder* fileBuilder):
    m_impl(new BEx86JITEngineImpl(fileBuilder)) {
}
BEx86JITEngine::~BEx86JITEngine() {
    delete m_impl;
}
void* BEx86JITEngine::getSymbol(const string &name) {
    return m_impl->getSymbol(name);
}
void BEx86JITEngine::dumpCode(ostream &so) {
    m_impl->dumpCode(so);
}
