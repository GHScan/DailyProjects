
#ifndef BYTE_CODE_H
#define BYTE_CODE_H

struct StackFrame;
struct FuncMeta;

void genCode(FuncMeta *meta);
void execute(StackFrame *frame);
void disassemble(FuncMeta *meta, int depth);

#endif
