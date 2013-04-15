#ifndef BYTE_CODE_H
#define BYTE_CODE_H

struct LuaStackFrame;
struct IStmtNode;

void execute(LuaStackFrame* stopFrame);
void genCode(vector<int>& codes, IStmtNode* ast);

#endif
