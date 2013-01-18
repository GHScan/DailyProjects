// Test05.cpp : 定义控制台应用程序的入口点。
//

#include "stdafx.h"

#include <cassert>

#include <vector>

#include <boost/shared_ptr.hpp>

#include <windows.h>

typedef boost::shared_ptr<std::vector<void*>>   PtrVec;

const void* qsearch(const void* _src, int srcLen, const void *_sub, int subLen)
{
    // sunday
    assert(_src != NULL && _sub != NULL);

    typedef unsigned char byte;
    const byte* src = (const byte*)_src;
    const byte* sub = (const byte*)_sub;
    const byte* srcEnd = src + srcLen - subLen;

    int jumps[256] = {0};
    for (int i = 0; i < _countof(jumps); ++i) jumps[i] = subLen + 1;
    for (int i = 0; i < subLen; ++i) jumps[sub[i]] = subLen - i;

    while (src <= srcEnd)
    {
        if (memcmp(src, sub, subLen) == 0) return src;
        int jump = jumps[src[subLen]];
        src += jump;
    }

    return NULL;
}

PtrVec searchProcessMemory(DWORD pid, const void* patt, size_t pattLen)
{
    assert(patt != NULL && pattLen > 0);
    assert(pid != 0);

    PtrVec ret(new PtrVec::element_type());

    HANDLE h = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, pid);
    if (h == NULL) return ret;

    std::vector<char> buf;

    char *p = NULL;
    MEMORY_BASIC_INFORMATION memInfo = {0};
    while (VirtualQueryEx(h, p, &memInfo, sizeof(memInfo)) > 0)
    {
        if (memInfo.State == MEM_COMMIT &&
            (memInfo.Protect & (PAGE_EXECUTE | PAGE_NOACCESS)) == 0)
        {
            buf.resize(memInfo.RegionSize);
            SIZE_T readedSize = 0;
            if (ReadProcessMemory(h, memInfo.BaseAddress, &buf[0], buf.size(), &readedSize))
            {
                buf.resize(readedSize);

                size_t off = 0;
                const void* matched = NULL;
                while (off < buf.size() &&
                     (matched = qsearch(&buf[0] + off, buf.size() - off, patt, pattLen)) != NULL)
                {
                    size_t pos = (char*)matched - &buf[0];
                    ret->push_back(p + pos);
                    off = pos + 1;
                }
            }
        }

        if (p > (char*)-1 - memInfo.RegionSize) break;
        p += memInfo.RegionSize;
    }

    CloseHandle(h);

    return ret;
}

void dumpPtrVec(PtrVec vec)
{
    for (size_t i = 0; i < vec->size(); ++i) 
    {
        if (i % 5 == 0 && i > 0) puts("");
        printf("%08x\t", (*vec)[i]);
    }
    puts("");
}

bool filterProcessDWORD(DWORD pid, PtrVec& vec, DWORD newVal)
{
    assert(vec.get() != NULL && !vec->empty());
    assert(pid != 0);

    HANDLE h = OpenProcess(PROCESS_VM_READ, FALSE, pid);
    if (h == NULL) return false;

    PtrVec tempVec(new PtrVec::element_type());

    for (size_t i = 0; i < vec->size(); ++i)
    {
        DWORD val = 0;
        SIZE_T readedSize = 0;
        if (ReadProcessMemory(h, (*vec)[i], &val, sizeof(val), &readedSize) && 
            readedSize == sizeof(val) &&
            val == newVal)
        {
            tempVec->push_back((*vec)[i]);
        }
    }

    CloseHandle(h);
    tempVec.swap(vec);
    return vec->size() < tempVec->size();
}

bool writeProcessMemory(DWORD pid, void *p, const void *src, size_t srcLen)
{
    assert(p != NULL && src != NULL && srcLen > 0);
    assert(pid != 0);

    bool ret = false;

    HANDLE h = OpenProcess(PROCESS_VM_WRITE | PROCESS_VM_OPERATION, FALSE, pid);
    if (h == NULL) return ret;

    SIZE_T writedSize = 0;
    ret = WriteProcessMemory(h, p, src, srcLen, &writedSize) == TRUE && 
        writedSize == srcLen;

    CloseHandle(h);
    return ret;
}

int main()
{
    int pid = 0;
    puts("输入进程ID:"); cin >> pid;
    if (pid == 0) pid = GetCurrentProcessId();

    DWORD val = 0;
    puts("输入要查找的DWORD:"); cin >> val;
    PtrVec vec = searchProcessMemory(pid, &val, sizeof(val));
    dumpPtrVec(vec);

    int filterFailed = 0; // 过滤失败次数
    while (vec->size() > 1 && filterFailed < 5)
    {
        puts("输入新的DWORD:"); cin >> val;

        if (filterProcessDWORD(pid, vec, val)) filterFailed = 0;
        else ++filterFailed;

        system("cls");
        dumpPtrVec(vec);
    }

    if (!vec->empty())
    {
        puts("输入要修改成的DWORD");
        cin >> val;
        for (size_t i = 0; i < vec->size(); ++i)
        {
            printf("写入 %08x %s\n", (*vec)[i], 
                writeProcessMemory(pid, (*vec)[i], &val, sizeof(val)) ? "成功" : "失败");
        }
    }

    system("pause");
}