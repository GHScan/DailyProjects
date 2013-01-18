#pragma once 

/*
    Լ��: ��������Сд��ͷ, ��չ���ʹ�д��ͷ
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
