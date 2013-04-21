
#ifndef LUA_LIBS_H
#define LUA_LIBS_H

#include "LuaVM.h"
#include "LuaValue.h"
#include "LuaTable.h"
#include "LuaString.h"
#include "LuaFunction.h"

extern LuaValue loadFile(const char *fname);
extern LuaValue loadFile(FILE *f);
extern void runfile(int argc, char *argv[]);

extern void openLib_buildin();
extern void openLib_string();
extern void openLib_table();
extern void openLib_math();
extern void openLib_os();
extern void openLib_io();

#endif
