
#ifndef LUA_LIBS_H
#define LUA_LIBS_H

struct IFunction;
typedef shared_ptr<IFunction> FunctionPtr;

extern FunctionPtr loadFile(const char *fname);
FunctionPtr loadFile(FILE *f);

extern void openLib_buildin();
extern void openLib_string();
extern void openLib_table();
extern void openLib_math();
extern void openLib_os();
extern void openLib_io();

#endif
