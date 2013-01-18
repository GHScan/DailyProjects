// stdafx.h : 标准系统包含文件的包含文件，
// 或是经常使用但不常更改的
// 特定于项目的包含文件
//

#pragma once


#define WIN32_LEAN_AND_MEAN		// 从 Windows 头中排除极少使用的资料
#include <stdio.h>
#include <tchar.h>

#include <windows.h>

// TODO: 在此处引用程序需要的其他头文件
#pragma warning(disable : 4311 4312)
#include <QtGui/QtGui>
#include <QtCore/QtCore>
#pragma warning(default : 4311 4312)

#ifdef __cplusplus
#include <iostream>
using std::cin;
using std::cout;
using std::endl;
#endif

#define _STRING_MACRO(a) #a
#define STRING_MACRO(a) _STRING_MACRO(a)

#define FILE_LINE  __FILE__ "(" STRING_MACRO(__LINE__) ")"

#define verify(exp)\
    if (exp){}\
    else\
{\
    const char *msg = FILE_LINE " : \n" STRING_MACRO(exp) "\n";\
    ::OutputDebugString(msg);\
    ::MessageBox(NULL, msg, "断言失败", MB_OK);\
    ::DebugBreak();\
}

#define ASSERT_INSIDE_N(a, n)   verify(a >= 0 && a < n)