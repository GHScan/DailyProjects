// Test05.cpp : 定义控制台应用程序的入口点。
//

#include "stdafx.h"

#include <cassert>

#include <memory>
#include <vector>
#include <string>

#include <windows.h>

#include <Psapi.h>
#pragma comment(lib, "Psapi")
#include <TlHelp32.h>

typedef std::auto_ptr<std::vector<MODULEENTRY32>> ModuleVec;

ModuleVec getMoudleList(DWORD pid)
{
    ModuleVec vec(new ModuleVec::element_type());

    HANDLE h = CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, pid);
    if (h == INVALID_HANDLE_VALUE) return vec;

    MODULEENTRY32 entry = {sizeof(entry)};
    if (Module32First(h, &entry))
    {
        do
        {
            vec->push_back(entry);
        } while(Module32Next(h, &entry));
    }

    CloseHandle(h);
    return vec;
}

bool dumpMemoryUsage(DWORD pid)
{
    HANDLE h = OpenProcess(PROCESS_QUERY_INFORMATION, FALSE, pid);
    if (h == NULL) return false;

    {
        cout << "VirtualQuery :" << endl;
        cout << "**************" << endl;

        ModuleVec modVec(getMoudleList(pid));

        size_t commitedBytes = 0;
        size_t readAllowedBytes = 0;
        size_t imageBytes = 0;
        size_t mappedBytes = 0;
        size_t privateBytes = 0;
        size_t namedImageBytes = 0;
        size_t namedMappedBytes = 0;

        char *p = NULL;
        MEMORY_BASIC_INFORMATION blkInfo = {0};
        while (VirtualQueryEx(h, p, &blkInfo, sizeof(blkInfo)) > 0)
        {
            if (blkInfo.State == MEM_COMMIT)
            {
                // 在使用了
                commitedBytes += blkInfo.RegionSize;

                if ((blkInfo.Protect & (PAGE_EXECUTE | PAGE_NOACCESS)) == 0)
                {
                    // 具有读权限
                    readAllowedBytes += blkInfo.RegionSize;
                }

                if (blkInfo.Type == MEM_IMAGE) imageBytes += blkInfo.RegionSize;
                if (blkInfo.Type == MEM_MAPPED) mappedBytes += blkInfo.RegionSize;
                if (blkInfo.Type == MEM_PRIVATE) privateBytes += blkInfo.RegionSize;

                if (blkInfo.Type == MEM_IMAGE)
                {
                    size_t i = 0;
                    for (; i < modVec->size(); ++i)
                    {
                        if (blkInfo.BaseAddress == (*modVec)[i].modBaseAddr) break;
                    }
                    if (i < modVec->size())
                    {                        
                        cout << "image : " << (*modVec)[i].modBaseSize / 1000.0f << " " << (*modVec)[i].szExePath << endl;
                        assert((*modVec)[i].modBaseSize % 4096 == 0);
                        namedImageBytes += (*modVec)[i].modBaseSize;
                    }
                }
                if (blkInfo.Type == MEM_MAPPED)
                {
                    char fileName[MAX_PATH] = "";
                    if (GetMappedFileName(h, blkInfo.BaseAddress, fileName, sizeof(fileName)) > 0)
                    {
                        cout << "mapped file : " << blkInfo.RegionSize / 1000.0f << " " << fileName << endl;
                        namedMappedBytes += blkInfo.RegionSize;
                    }
                }
            }

            if (p > (char*)-1 - blkInfo.RegionSize) break;
            p += blkInfo.RegionSize;
        }

        cout << "**************" << endl;
        cout << "commitedBytes = " << commitedBytes / 1000.f << endl;
        cout << "readAllowedBytes = " << readAllowedBytes / 1000.f << endl;
        cout << "imageBytes = " << imageBytes / 1000.f << endl;
        cout << "mappedBytes = " << mappedBytes / 1000.f << endl;
        cout << "privateBytes = " << privateBytes / 1000.f << endl;
        cout << "namedImageBytes = " << namedImageBytes / 1000.f << endl;
        cout << "namedMappedBytes = " << namedMappedBytes / 1000.f << endl;
    }

    {
        cout << "GetProcessMemoryInfo :" << endl;

        PROCESS_MEMORY_COUNTERS_EX memCounter = {0};
        if (!GetProcessMemoryInfo(h, (PROCESS_MEMORY_COUNTERS *)&memCounter, sizeof(memCounter))) assert(0);
        cout << "WorkingSetSize = " << memCounter.WorkingSetSize / 1000.0f << endl;
        cout << "PagefileUsage = " << memCounter.PagefileUsage / 1000.0f << endl;
        cout << "PrivateUsage = " << memCounter.PrivateUsage / 1000.0f << endl;
    }

    {
        cout << "GlobalMemoryStatus :" << endl;

        MEMORYSTATUS memStatus = {sizeof(memStatus)};
        GlobalMemoryStatus(&memStatus);
        cout << "dwAvailVirtual = " << memStatus.dwAvailVirtual / 1000.0f << endl;
    }

    CloseHandle(h);
    return true;
}

int main()
{
    DWORD pid = 0;
    cin >> pid;
    if (pid == 0) pid = GetCurrentProcessId();
    dumpMemoryUsage(pid);

    system("pause");
}