#include "pch.h"
#include "RegisterBasedISA.h"

void RB_InstructionList::fromStream(istream &si) {
    m_bytes.clear();

    string insName;
    while (si >> insName) {
        InstructionMeta *meta = InstructionMetaManager<RB_Code>::instance()->get(insName);
        int off = (int)m_bytes.size();
        m_bytes.resize(off + meta->size);
        meta->fromStream(si, &m_bytes[off]);
    }
    append(RBC_EOF);
}

void RB_InstructionList::toStream(ostream &so) const {
    if (m_bytes.empty()) return;

    for (int off = 0; off < (int)m_bytes.size(); ) {
        InstructionMeta *meta = InstructionMetaManager<RB_Code>::instance()->get((CodeType&)m_bytes[off]);
        meta->toStream(so, &m_bytes[off]);
        so << '\n';
        off += meta->size;
    }
}

void RB_InstructionList::translateJmpIdx2Off() {
    vector<int> insOffs;
    for (int off = 0; off < (int)m_bytes.size(); ) {
        insOffs.push_back(off);
        off += RB_getInsuctionSize((CodeType&)m_bytes[off]);
    }
    insOffs.push_back((int)m_bytes.size());

    for (int off = 0; off < (int)m_bytes.size(); ) {
        CodeType code = (CodeType&)m_bytes[off];
        JmpOffType *jmpOff = NULL;
        switch (code) {
            case RBC_Jmp: jmpOff = &((RB_Instruction<RBC_Jmp>&)m_bytes[off]).jmpOff; break;
            case RBC_TJmp: jmpOff = &((RB_Instruction<RBC_TJmp>&)m_bytes[off]).jmpOff; break;
            case RBC_Repeat: jmpOff = &((RB_Instruction<RBC_Repeat>&)m_bytes[off]).jmpOff; break;
            default: break;
        }
        if (jmpOff != NULL) *jmpOff = insOffs[*jmpOff] - off;
        off += RB_getInsuctionSize(code);
    }
}

int RB_getInsuctionSize(CodeType code) {
    return InstructionMetaManager<RB_Code>::instance()->get(code)->size;
}
const char* RB_getInstructionName(CodeType code) {
    return InstructionMetaManager<RB_Code>::instance()->get(code)->name.c_str();
}

void RB_setupISA() {
    InstructionMetaManager<RB_Code>::instance()->add(new InstructionMeta3<LocalIdxType, LocalIdxType, LocalIdxType>(RBC_Add, "add"));
    InstructionMetaManager<RB_Code>::instance()->add(new InstructionMeta3<LocalIdxType, LocalIdxType, LocalIdxType>(RBC_Sub, "sub"));
    InstructionMetaManager<RB_Code>::instance()->add(new InstructionMeta3<LocalIdxType, LocalIdxType, LocalIdxType>(RBC_Mul, "mul"));
    InstructionMetaManager<RB_Code>::instance()->add(new InstructionMeta3<LocalIdxType, LocalIdxType, LocalIdxType>(RBC_Div, "div"));
    InstructionMetaManager<RB_Code>::instance()->add(new InstructionMeta3<LocalIdxType, LocalIdxType, LocalIdxType>(RBC_EQ, "eq"));
    InstructionMetaManager<RB_Code>::instance()->add(new InstructionMeta3<LocalIdxType, LocalIdxType, LocalIdxType>(RBC_NE, "ne"));
    InstructionMetaManager<RB_Code>::instance()->add(new InstructionMeta2<LocalIdxType, int>(RBC_LoadInt, "loadint"));
    InstructionMetaManager<RB_Code>::instance()->add(new InstructionMeta2<LocalIdxType, LocalIdxType>(RBC_Mov, "mov"));
    InstructionMetaManager<RB_Code>::instance()->add(new InstructionMeta1<JmpOffType>(RBC_Jmp, "jmp"));
    InstructionMetaManager<RB_Code>::instance()->add(new InstructionMeta2<LocalIdxType, JmpOffType>(RBC_TJmp, "tjmp"));
    InstructionMetaManager<RB_Code>::instance()->add(new InstructionMeta4<LocalIdxType, LocalIdxType, LocalIdxType, JmpOffType>(RBC_Repeat, "repeat"));
    InstructionMetaManager<RB_Code>::instance()->add(new InstructionMeta(RBC_Nop, "nop"));
    InstructionMetaManager<RB_Code>::instance()->add(new InstructionMeta(RBC_EOF, "eof"));
}
