// test03.cpp : �������̨Ӧ�ó������ڵ㡣
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