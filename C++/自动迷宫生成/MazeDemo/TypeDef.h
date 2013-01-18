#pragma once 

/*
    约定: 内置类型小写开头, 扩展类型大写开头
*/

typedef unsigned char   uChar;
typedef unsigned short  uShort;
typedef unsigned int    uInt;
typedef unsigned long   uLong;

typedef char *          cStr;
typedef const char *    c_cStr;

typedef void *          handle;

#if defined(__cplusplus)
#include <string>
#include <sstream>
#include <fstream>
#endif 

#if defined(__cplusplus)
typedef std::string             StdStr;
typedef std::ifstream           StdIFile;
typedef std::ofstream           StdOFile;
typedef std::istringstream      StdIStream;
typedef std::ostringstream      StdOStream;
#endif
