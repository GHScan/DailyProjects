#ifndef STACKBASED_ISA_H
#define STACKBASED_ISA_H
//==============================
#include "ISA.h"

//==============================
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
    LocalIdxType loopCounter;
    LocalIdxType iter;
    LocalIdxType step;
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

class SB_InstructionList {
public:
    void fromStream(istream &si);
    void toStream(ostream &so) const;

    void append(CodeType c) {
        appendValue(c);
    }

    void translateJmpIdx2Off();

    vector<Byte>& getBytes() { return m_bytes; }
private:
    template<typename T>
    void appendValue(const T& v) {
        int off = (int)m_bytes.size();
        m_bytes.resize(off + sizeof(v));
        memcpy(&m_bytes[off], &v, sizeof(v));
    }

private:
    vector<Byte> m_bytes;
};

inline istream& operator >> (istream &si, SB_InstructionList &list) {
    list.fromStream(si);
    return si;
}
inline ostream& operator << (ostream& so, const SB_InstructionList &list) {
    list.toStream(so);
    return so;
}

void SB_setupISA();
int SB_getInsuctionSize(CodeType code);

//==============================
#endif // STACKBASED_ISA_H
