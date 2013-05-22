
#ifndef BYTE_CODE_H
#define BYTE_CODE_H

struct StackFrame;
struct FuncMeta;

void emitCode(FuncMeta *meta);
void execute(StackFrame *stopFrame);
void disassemble(FuncMeta *meta, int depth);

#endif
