#ifndef REGISTER_BASED_ISA_H
#define REGISTER_BASED_ISA_H
//==============================
#include "ISA.h"

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
template<RB_Code>
struct RB_Instruction;
template<>
struct RB_Instruction<RBC_Add> {
    CodeType code;
    LocalIdxType dest, src1, src2;
};
template<>
struct RB_Instruction<RBC_Sub> {
    CodeType code;
    LocalIdxType dest, src1, src2;
};
template<>
struct RB_Instruction<RBC_Mul> {
    CodeType code;
    LocalIdxType dest, src1, src2;
};
template<>
struct RB_Instruction<RBC_Div> {
    CodeType code;
    LocalIdxType dest, src1, src2;
};
template<>
struct RB_Instruction<RBC_EQ> {
    CodeType code;
    LocalIdxType dest, src1, src2;
};
template<>
struct RB_Instruction<RBC_NE> {
    CodeType code;
    LocalIdxType dest, src1, src2;
};
template<>
struct RB_Instruction<RBC_LoadInt> {
    CodeType code;
    LocalIdxType dest;
    int i;
};
template<>
struct RB_Instruction<RBC_Mov> {
    CodeType code;
    LocalIdxType dest, src;
};
template<>
struct RB_Instruction<RBC_Jmp> {
    CodeType code;
    JmpOffType jmpOff;
};
template<>
struct RB_Instruction<RBC_TJmp> {
    CodeType code;
    LocalIdxType cond;
    JmpOffType jmpOff;
};
template<>
struct RB_Instruction<RBC_Repeat> {
    CodeType code;
    LocalIdxType loopCounter, iter, step;
    JmpOffType jmpOff;
};
template<>
struct RB_Instruction<RBC_Nop> {
    CodeType code;
};
template<>
struct RB_Instruction<RBC_EOF> {
    CodeType code;
};
#pragma pack(pop)

class RB_InstructionList {
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

inline istream& operator >> (istream &si, RB_InstructionList &list) {
    list.fromStream(si);
    return si;
}
inline ostream& operator << (ostream& so, const RB_InstructionList &list) {
    list.toStream(so);
    return so;
}

void RB_setupISA();
// TODO
int RB_getInsuctionSize(CodeType code);
const char* RB_getInstructionName(CodeType code);

#endif // REGISTER_BASED_ISA_H
