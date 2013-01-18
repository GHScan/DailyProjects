// test03.cpp : 定义控制台应用程序的入口点。
//

#include "stdafx.h"

#include "..\hookDll03\hookDll03.h"
#pragma comment(lib, "hookdll03")

using namespace std;

int main(int argc, _TCHAR* argv[])
{
    new int;

    dumpMemoryLeaksToDebug();

	return 0;
}