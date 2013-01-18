#include "StdAfx.h"

#include <cassert>

#include "PlatformDepends.h"

#if defined(_DEBUG) || defined(DEBUG)

namespace Scan
{
    namespace
    {
        class VcCrtDebugFlagSeter
        {
        public:
            VcCrtDebugFlagSeter()
            {
                _CrtSetDbgFlag(
                    _CRTDBG_ALLOC_MEM_DF | 
                    _CRTDBG_LEAK_CHECK_DF |
                    _CrtSetDbgFlag(_CRTDBG_REPORT_FLAG));    
            }
        };

        VcCrtDebugFlagSeter g_setter;
    }

    static ISyncObject* getMemoryCheckSyncObject()
    {
        static Windows::CriticalSection ls_cs;
        return &ls_cs;
    }

    static const int SCAN_MEMORY_BLOCK = (23 << 16) | _CLIENT_BLOCK;

    void* allocMemoryCheck(size_t memSize, const char *fileName, unsigned int line)
    {
        SingleLocker locker(getMemoryCheckSyncObject());

        void* p = _malloc_dbg(memSize, SCAN_MEMORY_BLOCK, fileName, line);
        assert(p != NULL);
        return p;
    }

    void freeMemoryCheck(void* memChunk, const char *fileName, unsigned int line)
    {
        SingleLocker locker(getMemoryCheckSyncObject());

        _free_dbg(memChunk,  SCAN_MEMORY_BLOCK);
    }
}

void* operator new (size_t memSize, const char *fileName, unsigned int line)
{
    return Scan::allocMemoryCheck(memSize, fileName, line);
}
void* operator new[] (size_t memSize, const char *fileName, unsigned int line)
{
    return Scan::allocMemoryCheck(memSize, fileName, line);
}
void operator delete (void* memChunk, const char *fileName, unsigned int line)
{
    Scan::freeMemoryCheck(memChunk, fileName, line);
}
void operator delete[] (void* memChunk, const char *fileName, unsigned int line)
{
    Scan::freeMemoryCheck(memChunk, fileName, line);
}

#endif