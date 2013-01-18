// stdafx.cpp : source file that includes just the standard includes
// Console.pch will be the pre-compiled header
// stdafx.obj will contain the pre-compiled type information

#include "stdafx.h"

// TODO: reference any additional headers you need in STDAFX.H
// and not in this file
#ifdef _DEBUG
#pragma comment(lib, "qtguid4")
#pragma comment(lib, "qtcored4")
#else
#pragma comment(lib, "qtgui4")
#pragma comment(lib, "qtcore4")
#endif