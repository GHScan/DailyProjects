// stdafx.cpp : 只包括标准包含文件的源文件
// Console.pch 将作为预编译头
// stdafx.obj 将包含预编译类型信息

#include "stdafx.h"

// TODO: 在 STDAFX.H 中
// 引用任何所需的附加头文件，而不是在此文件中引用

#ifdef _DEBUG
#define LIB_TYPE    "d"
#else
#define LIB_TYPE
#endif

#pragma comment(lib, "qtgui" LIB_TYPE "4")
#pragma comment(lib, "qtcore" LIB_TYPE "4")