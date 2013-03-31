
#ifndef LUA_LIBS_H
#define LUA_LIBS_H

#include "LuaValue.h"
#include "LuaTable.h"
#include "Runtime.h"
#include "Function.h"

struct IFunction;
typedef shared_ptr<IFunction> FunctionPtr;

extern FunctionPtr loadFile(const char *fname);
extern FunctionPtr loadFile(FILE *f);
extern void dofile(const char *fname);

extern void openLib_buildin();
extern void openLib_string();
extern void openLib_table();
extern void openLib_math();
extern void openLib_os();
extern void openLib_io();
extern void openLib_all();

#endif
