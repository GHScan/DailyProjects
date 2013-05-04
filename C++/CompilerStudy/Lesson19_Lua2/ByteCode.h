#ifndef BYTE_CODE_H
#define BYTE_CODE_H

struct LuaStackFrame;
struct IStmtNode;
struct LuaFunctionMeta;

void execute(LuaStackFrame* stopFrame);
void emitCode(LuaFunctionMeta* meta);
void disassemble(ostream& so, LuaFunctionMeta* meta);

#endif
