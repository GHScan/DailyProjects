
#include "pch.h"
#include "Interpreter.h"
#include "StackBasedInterpreter.h"
#include "RegisterBasedInterpreter.h"

void InstructionList::fromStream(istream &si) {
    m_bytes.clear();

    string insName;
    while (si >> insName) {
        InstructionMeta *meta = m_factory->getInstructionMetaManager()->get(insName);
        int off = (int)m_bytes.size();
        m_bytes.resize(off + meta->size);
        meta->fromStream(si, &m_bytes[off]);
    }
    appendEOF();
}

void InstructionList::toStream(ostream &so) const {
    if (m_bytes.empty()) return;

    for (int off = 0; off < (int)m_bytes.size(); ) {
        InstructionMeta *meta = m_factory->getInstructionMetaManager()->get((CodeType&)m_bytes[off]);
        meta->toStream(so, &m_bytes[off]);
        so << '\n';
        off += meta->size;
    }
}

InterpreterFactory* InterpreterFactory::getFactory(const string &name) {
    if (name == "<sb>") {
        static SB_InterpreterFactory s_ins;
        return &s_ins;
    } else if (name == "<rb>") {
        static RB_InterpreterFactory s_ins;
        return &s_ins;
    } else {
        assert(0);
    }
}
