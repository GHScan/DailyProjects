#include <stdio.h>
#include <assert.h>

#include <iostream>
using std::cin;
using std::cout;
using std::endl;

#ifdef _MSC_VER
#pragma warning(disable : 4018)
#endif

#ifdef _DEBUG
#else
#undef assert
#define assert(b) if (b); else *(int*)0 = 1;
#endif

#define _TO_STRING(s) #s
#define TO_STRING(s) _TO_STRING(s)
