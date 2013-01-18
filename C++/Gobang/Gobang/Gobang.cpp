// Gobang.cpp : 定义 DLL 应用程序的入口点。
//

#include "stdafx.h"


#ifdef _MANAGED
#pragma managed(push, off)
#endif

extern void registerLevel0Creator();
extern void registerLevel1Creator();
extern void registerLevel0ExCreator();
extern void registerLevel1ExCreator();

BOOL APIENTRY DllMain( HMODULE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved
					 )
{
    if (ul_reason_for_call == DLL_PROCESS_ATTACH)
    {
        registerLevel0Creator();
        registerLevel1Creator();
        registerLevel0ExCreator();
        registerLevel1ExCreator();
    }

    return TRUE;
}

#ifdef _MANAGED
#pragma managed(pop)
#endif

