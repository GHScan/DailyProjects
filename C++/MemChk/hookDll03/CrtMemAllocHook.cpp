#include "stdafx.h"

#include <ctime>

#include <map>
#include <vector>
#include <string>

#include "FuncHook.h"
#include "Win32APIWrapper.h"
#include "DllComponent.h"
#include "ScanUtil.h"

#include "hookDll03.h"

#ifndef __in
#define __in
#endif

void __cdecl fake_free_dbg(void * p, int blockType)
{
}

#ifndef _DEBUG
#define _free_dbg fake_free_dbg
#endif

typedef std::vector<void*>      PtrList;

class StackContex
{
public:
    StackContex(size_t skip = -1);

    const PtrList& getCallSeq() const
    {
        return m_eips;
    }

    bool operator < (const StackContex& o) const
    {
        if (m_eips.size() == o.m_eips.size())
        {
            return memcmp(&m_eips[0], &o.m_eips[0], m_eips.size() << 2) < 0;
        }
        return m_eips.size() < o.m_eips.size();
    }

private:
    PtrList m_eips;
};

StackContex::StackContex(size_t skip)
{
#pragma warning(disable : 4312)

    if (skip == -1)
    {
        return;
    }

    int* _ebp = NULL;

    __asm mov _ebp, ebp

    while (_ebp != NULL)
    {   
        if (skip == 0)
        {
            m_eips.push_back((void*)_ebp[1]);
        }
        else
        {
            --skip;
        }
        _ebp = (int*)_ebp[0];
    }

#pragma warning(default : 4312)
}

struct SingleAlloc
{
    size_t  size;
    time_t  tm;
};

struct StackContexAllocation
{
    typedef std::map<void*, SingleAlloc>    MemBlocks;

    StackContexAllocation(): usingSize(0){}
    
    MemBlocks   blocks;
    size_t      usingSize;
};

class MemAllocMonitor:
    public IDllComponent
{
public:
    MemAllocMonitor();

    void dumpToFile(FILE *f, size_t maxDumpCnt) const;
    void dumpToDebug(size_t maxDumpCnt) const;

    virtual bool isEnable() const;
    virtual bool setup();
    virtual bool hasSetuped() const;
    virtual void cleanup();

    static void * __cdecl _onMalloc(size_t size);
    static void  __cdecl _onFree(void * p);
    static void __cdecl _onFreeDbg(void * p, int blockType);
    void * onMalloc(size_t size);
    void  onFree(void * p);
    void  onFreeDbg(void * p, int blockType);

private:
    void recordFree(void *p);
    void recordMalloc(void *p, size_t size);

private:
    typedef std::map<StackContex, StackContexAllocation>   UsingMemories;
    typedef std::map<void*, StackContex>                   PtrAllocMap;

private:
    FuncHook       *m_hook_malloc;
    FuncHook       *m_hook_free;
    FuncHook       *m_hook_freeDbg;

    CriticalSection m_cs;

    UsingMemories   m_mem;
    PtrAllocMap     m_ptr2Alloc;

    size_t          m_totalAlloc;
    size_t          m_totalFree;

    mutable bool    m_isInsideAlloc;
};

MemAllocMonitor::MemAllocMonitor():
m_totalFree(0), m_totalAlloc(0), m_isInsideAlloc(false)
{
}

bool MemAllocMonitor::isEnable() const
{
#if ENABLE_MEMORY_CHECK
    return true;
#else
    return false;
#endif
}

bool MemAllocMonitor::setup()
{
    if (hasSetuped())
    {
        return false;
    }

    size_t mallocAlign = 8;
    size_t freeAlign = 7;
    size_t freeDbgAlign = 10;

#ifdef _DEBUG
    freeAlign = 9;
#endif
    
    m_hook_freeDbg = new FuncHook(_free_dbg, _onFreeDbg, freeDbgAlign);
    m_hook_free = new FuncHook(free, _onFree, freeAlign);
    m_hook_malloc = new FuncHook(malloc, _onMalloc, mallocAlign);

    return m_hook_malloc->isValid() 
        && m_hook_free->isValid() 
       && m_hook_freeDbg->isValid();
}

void MemAllocMonitor::cleanup()
{
    if (hasSetuped())
    {
        Scan::safe_delete(m_hook_malloc);
        Scan::safe_delete(m_hook_free);
        Scan::safe_delete(m_hook_freeDbg);
    }
}

bool MemAllocMonitor::hasSetuped() const
{
    return m_hook_malloc != NULL;
}

void MemAllocMonitor::recordFree(void *p)
{
    PtrAllocMap::iterator iter = m_ptr2Alloc.find(p);
    if (iter != m_ptr2Alloc.end())
    {
        UsingMemories::iterator iter2 = m_mem.find(iter->second);

        StackContexAllocation& allocation = iter2->second;
        size_t blockSize = allocation.blocks[p].size;
        allocation.usingSize -= blockSize;

        if (allocation.usingSize == 0)
        {
            m_mem.erase(iter2);
        }

        m_ptr2Alloc.erase(iter);

        m_totalFree += blockSize;
    }
}

void MemAllocMonitor::recordMalloc(void *p, size_t size)
{
    StackContex callSeq(3);

    StackContexAllocation& allocation = m_mem[callSeq];
    allocation.usingSize += size;

    SingleAlloc& singleAlloc = allocation.blocks[p];
    singleAlloc.size = size;
    singleAlloc.tm = time(NULL);

    m_ptr2Alloc[p] = callSeq;

    m_totalAlloc += size;
}

void * MemAllocMonitor::onMalloc(size_t size)
{
    SingleLocker locker(&m_cs);

    void *ret = m_hook_malloc->getOldFunc(_onMalloc)(size);

    if (!m_isInsideAlloc)
    {
        m_isInsideAlloc = true;

        recordMalloc(ret, size);

        m_isInsideAlloc = false;
    }

    return ret;
}

void MemAllocMonitor::onFree(void * p)
{
    SingleLocker locker(&m_cs);

    if (!m_isInsideAlloc)
    {
        m_isInsideAlloc = true;

        recordFree(p);

        m_isInsideAlloc = false;
    }

    m_hook_free->getOldFunc(_onFree)(p);
}

void MemAllocMonitor::onFreeDbg(void * p, int blockType)
{
    SingleLocker locker(&m_cs);

    if (!m_isInsideAlloc)
    {
        m_isInsideAlloc = true;

        recordFree(p);

        m_isInsideAlloc = false;
    }

    m_hook_freeDbg->getOldFunc(_onFreeDbg)(p, blockType);
}

static MemAllocMonitor g_memMonitor;

void * __cdecl MemAllocMonitor::_onMalloc(size_t size)
{
    return g_memMonitor.onMalloc(size);
}

void  __cdecl MemAllocMonitor::_onFree(void * p)
{
    g_memMonitor.onFree(p);
}

void __cdecl MemAllocMonitor::_onFreeDbg(void * p, int blockType)
{
    g_memMonitor.onFreeDbg(p, blockType);
}

void MemAllocMonitor::dumpToFile(FILE *f, size_t maxDumpCnt) const
{

}

void MemAllocMonitor::dumpToDebug(size_t maxDumpCnt) const
{
    if (m_mem.empty())
    {
        OutputDebugString("没有检测到内存泄漏\n");
        return;
    }

    m_isInsideAlloc = true;

    OutputDebugString(
        ("检测到内存泄漏 : 总计 " + 
        Scan::toString(m_mem.size()) + " 处 " + 
        Scan::toString(m_totalAlloc - m_totalFree) + " 字节\n").c_str());

    DbgHelpWrapper dlgHelper;

    typedef std::multimap<size_t, UsingMemories::const_iterator>  UsingSizeOrderedMap;

    UsingSizeOrderedMap usingMp;

    for (UsingMemories::const_iterator iter = m_mem.begin();
        iter != m_mem.end();
        ++iter)
    {
        usingMp.insert(UsingSizeOrderedMap::value_type(iter->second.usingSize, iter));
    }

    size_t index = 0;
    for (UsingSizeOrderedMap::iterator iter = usingMp.begin();
        index < maxDumpCnt && iter != usingMp.end();
        ++iter)
    {
        UsingMemories::const_iterator iter2 = iter->second;

        std::string s = "@位置 " + Scan::toString(++index) + " :\n";

        const PtrList& ptrs = iter2->first.getCallSeq();
        for (PtrList::const_iterator iter3 = ptrs.begin();
            iter3 != ptrs.end();
            ++iter3)
        {
            std::string file;
            int line;
            char buf[256] = "";

            if (dlgHelper.addressToFileLine(*iter3, file, line))
            {
                sprintf(buf, "%s(%d) : 0x%x\n", file.c_str(), line, *iter3);
                s += buf;
            }
            else if (dlgHelper.addressToModuleName(*iter3, file))
            {
                sprintf(buf, "%s: 0x%x\n", file.c_str(), *iter3);
                s += buf;
            }
            else
            {
                sprintf(buf, "%0x%x\n", file.c_str(), *iter3);
                s += buf;
            }
        }

        s += "共计 : " + Scan::toString(iter2->second.usingSize) + "字节\n";
        
        s += "分配点 : \n";

        for (StackContexAllocation::MemBlocks::const_iterator iter3 = iter2->second.blocks.begin();
            iter3 != iter2->second.blocks.end();
            ++iter3)
        {
            tm *t = localtime(&iter3->second.tm);
            char sz[256] = "";
            sprintf(sz, "(%d:%d:%d) : %d字节 -> ", t->tm_hour, t->tm_min, t->tm_sec, iter3->second.size);
            memcpy(sz + strlen(sz), iter3->first, min(iter3->second.size, 200));
            s += sz; s += " \n";
        }

        s += "\n";

        OutputDebugString(s.c_str());
    }

    m_isInsideAlloc = false;
}

HOOKDLL_API void dumpMemoryLeaksToFile(FILE *f, size_t maxDumpCnt)
{
    g_memMonitor.dumpToFile(f, maxDumpCnt);
}

HOOKDLL_API void dumpMemoryLeaksToDebug(size_t maxDumpCnt)
{
    g_memMonitor.dumpToDebug(maxDumpCnt);
}