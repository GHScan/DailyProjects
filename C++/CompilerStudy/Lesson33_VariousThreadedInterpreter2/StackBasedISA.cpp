
#include "pch.h"
#include "StackBasedISA.h"

void SB_InstructionList::fromStream(istream &si) {
    m_bytes.clear();

    string insName;
    while (si >> insName) {
        InstructionMeta *meta = InstructionMetaManager<SB_Code>::instance()->get(insName);
        int off = m_bytes.size();
        m_bytes.resize(off + meta->size);
        meta->fromStream(si, &m_bytes[off]);
    }
    append(SBC_EOF);
}

void SB_InstructionList::toStream(ostream &so) const {
    if (m_bytes.empty()) return;

    for (int off = 0; off < (int)m_bytes.size(); ) {
        InstructionMeta *meta = InstructionMetaManager<SB_Code>::instance()->get((CodeType&)m_bytes[off]);
        meta->toStream(so, &m_bytes[off]);
        so << '\n';
        off += meta->size;
    }
}

void SB_InstructionList::convertJmpOff() {
    vector<int> insOffs;
    for (int off = 0; off < (int)m_bytes.size(); ) {
        insOffs.push_back(off);
        off += SB_getInsuctionSize((CodeType&)m_bytes[off]);
    }
    insOffs.push_back((int)m_bytes.size());

    for (int off = 0; off < (int)m_bytes.size(); ) {
        CodeType code = (CodeType&)m_bytes[off];
        JmpOffType *poff = NULL;
        switch (code) {
            case SBC_Jmp: 
                poff = &((SB_Instruction<SBC_Jmp>&)m_bytes[off]).off;
                break;
            case SBC_TJmp:
                poff = &((SB_Instruction<SBC_TJmp>&)m_bytes[off]).off;
                break;
            case SBC_Repeat:
                poff = &((SB_Instruction<SBC_Repeat>&)m_bytes[off]).off;
                break;
            default: break;
        }
        if (poff != NULL) *poff = insOffs[*poff] - off;
        off += SB_getInsuctionSize(code);
    }
}

int SB_getInsuctionSize(CodeType code) {
    return InstructionMetaManager<SB_Code>::instance()->get(code)->size;
}

void SB_setupISA() {
    InstructionMetaManager<SB_Code>::instance()->add(new InstructionMeta(SBC_Add, "add"));
    InstructionMetaManager<SB_Code>::instance()->add(new InstructionMeta(SBC_Sub, "sub"));
    InstructionMetaManager<SB_Code>::instance()->add(new InstructionMeta(SBC_Mul, "mul"));
    InstructionMetaManager<SB_Code>::instance()->add(new InstructionMeta(SBC_Div, "div"));
    InstructionMetaManager<SB_Code>::instance()->add(new InstructionMeta(SBC_EQ, "eq"));
    InstructionMetaManager<SB_Code>::instance()->add(new InstructionMeta(SBC_NE, "ne"));
    InstructionMetaManager<SB_Code>::instance()->add(new InstructionMeta1<LocalIdxType>(SBC_PushLocal, "pushlocal"));
    InstructionMetaManager<SB_Code>::instance()->add(new InstructionMeta1<LocalIdxType>(SBC_PopLocal, "poplocal"));
    InstructionMetaManager<SB_Code>::instance()->add(new InstructionMeta1<int>(SBC_PushInt, "pushint"));
    InstructionMetaManager<SB_Code>::instance()->add(new InstructionMeta1<JmpOffType>(SBC_Jmp, "jmp"));
    InstructionMetaManager<SB_Code>::instance()->add(new InstructionMeta1<JmpOffType>(SBC_TJmp, "tjmp"));
    InstructionMetaManager<SB_Code>::instance()->add(new InstructionMeta4<LocalIdxType, LocalIdxType, LocalIdxType, JmpOffType>(SBC_Repeat, "repeat"));
    InstructionMetaManager<SB_Code>::instance()->add(new InstructionMeta(SBC_Nop, "nop"));
    InstructionMetaManager<SB_Code>::instance()->add(new InstructionMeta(SBC_EOF, "eof"));
}

