
#ifndef BYTECODE_H
#define BYTECODE_H

#include "AST.h"
class StackFrame;

class ByteCodeSeq
{
public:
    ByteCodeSeq(StmtNodePtr stmt);
    void disassembly(ostream& so);
    void run(StackFrame* frame);
private:
    vector<int> m_codes;
};

#endif
