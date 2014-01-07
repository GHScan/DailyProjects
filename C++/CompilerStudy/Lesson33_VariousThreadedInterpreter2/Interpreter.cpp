
#include "pch.h"
#include "Interpreter.h"

void InstructionList::fromStream(istream &si) {
    m_bytes.clear();

    string insName;
    while (si >> insName) {
        InstructionMeta *meta = InstructionMetaManager<SB_Code>::instance()->get(insName);
        int off = (int)m_bytes.size();
        m_bytes.resize(off + meta->size);
        meta->fromStream(si, &m_bytes[off]);
    }
    append(SBC_EOF);
}

void InstructionList::toStream(ostream &so) const {
    if (m_bytes.empty()) return;

    for (int off = 0; off < (int)m_bytes.size(); ) {
        InstructionMeta *meta = InstructionMetaManager<SB_Code>::instance()->get((CodeType&)m_bytes[off]);
        meta->toStream(so, &m_bytes[off]);
        so << '\n';
        off += meta->size;
    }
}

void SB_InstructionList::translateJmpIdx2Off() {
    vector<int> insOffs;
    for (int off = 0; off < (int)m_bytes.size(); ) {
        insOffs.push_back(off);
        off += SB_getInsuctionSize((CodeType&)m_bytes[off]);
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
        off += SB_getInsuctionSize(code);
    }
}

int SB_getInsuctionSize(CodeType code) {
    return InstructionMetaManager<SB_Code>::instance()->get(code)->size;
}

