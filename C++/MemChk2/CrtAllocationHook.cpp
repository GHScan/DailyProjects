#include "StdAfx.h"

#include <crtdbg.h>

#include <exception>
#include <vector>
#include <algorithm>

#include "FuncHook.h"
#include "ObjTrace.h"
#include "Win32APIWrapper.h"
#include "APIHook.h"

#include "CrtAllocationHook.h"

#undef max
#undef min

namespace 
{
    typedef std::vector<std::string>    StringVec;

    struct StackNode;
    StackNode* stackNodePool_alloc(StackNode *parent, void *retAddress);
    void stackNodePool_free(StackNode *node);
    void stackNodePool_clear();

    struct StackNode
    {
        StackNode(void *_retAddress, StackNode* _parent): 
        parent(_parent), nodeBytes(0), retAddress(_retAddress), recordCount(0){}

        StackNode *parent;
        size_t nodeBytes;
        size_t recordCount;
        void *retAddress;
        std::vector<StackNode*> children;

        StackNode* addRecord(const VoidPtrVec& callStack, size_t allocBytes, size_t depth = 0)
        {   
            assert(!callStack.empty() && allocBytes > 0);

            if (callStack.size() - 1 >= depth)
            {
                size_t idx = callStack.size() - 1 - depth;
                void *retAddr = callStack[idx];

                StackNode* child = NULL;

                size_t childCnt = children.size();
                for (size_t i = 0; i < childCnt; ++i)
                {
                    if (children[i]->retAddress == retAddr)
                    {
                        child = children[i];
                        break;
                    }
                }
                if (child == NULL)
                {
                    children.push_back(stackNodePool_alloc(this, retAddr));
                    child = children.back();
                }

                return child->addRecord(callStack, allocBytes, depth + 1);
            }
            else
            {
                nodeBytes += allocBytes;
                ++recordCount;
                return this;
            }
        }

        void removeRecord(size_t allocBytes, StackNode* end)
        {
            if (this == end) return;

            // 这个是真正的移除记录操作
            if (allocBytes > 0)
            {
                nodeBytes -= allocBytes;
                --recordCount;
            }

            if (recordCount == 0 && children.empty())
            {
                StackNode *_parent = parent;

                _parent->children.erase(std::remove(_parent->children.begin(), _parent->children.end(), this), _parent->children.end());
                stackNodePool_free(this);
                _parent->removeRecord(0, end);
            }
        }

        void dumpRecord(std::multimap<float, std::string>& out, size_t minDumpBytes, StringVec& stackDescrib, Win32::DbgHelpWrapper *helper, bool skipThis = true)
        {
            if (!skipThis)
            {
                std::string describ;
                helper->addressToString(retAddress, describ);
                stackDescrib.push_back(describ);

                if (nodeBytes >= minDumpBytes)
                {
                    std::string outS;

                    char buf[128] = "";
                    sprintf(buf, "%d 次分配 :\n", recordCount);
                    outS = buf;
                    // 反向打印
                    for (int i = (int)stackDescrib.size() - 1; i >= 0; --i)
                    {
                        outS += stackDescrib[i] + '\n';
                    }
                    outS += '\n';

                    out.insert(std::multimap<float, std::string>::value_type(nodeBytes / float(1000 * 1000), outS));
                }
            }

            for (size_t i = 0; i < children.size(); ++i)
            {
                children[i]->dumpRecord(out, minDumpBytes, stackDescrib, helper, false);
            }
            
            if (!skipThis)
            {
                stackDescrib.pop_back();
            }
        }

        void deleteChildren()
        {
            while (!children.empty())
            {
                children.back()->deleteChildren();
                stackNodePool_free(children.back());
                children.pop_back();
            }
        }
    };

    // 不用boost::pool而用这种方式, 是因为, 比起StackNode结构自己的分配和释放, 
    // StackNode的成员vector的分配和释放开销也不小, 这种方式缓存了new和construction,
    // 所以避免了成员vector频繁构造和析构
    std::vector<StackNode*> g_stackNodePool;
    StackNode* stackNodePool_alloc(StackNode *parent, void *retAddress)
    {
        StackNode *node = NULL;
        if (!g_stackNodePool.empty())
        {
            node = g_stackNodePool.back();
            g_stackNodePool.pop_back();
            node->retAddress = retAddress;
            node->parent = parent;
        }
        else
        {
            node = new StackNode(retAddress, parent);
        }
        return node;
    }

    void stackNodePool_free(StackNode *node)
    {
        node->children.clear();
        g_stackNodePool.push_back(node);

        // 如果空闲的节点超过1M个, 清理
        if (g_stackNodePool.size() > 1 << 20)
        {
            stackNodePool_clear();
        }
    }

    void stackNodePool_clear()
    {
        while (!g_stackNodePool.empty())
        {
            delete g_stackNodePool.back();
            g_stackNodePool.pop_back();
        }
        g_stackNodePool.swap(std::vector<StackNode*>());
    }

    class MemoryTracer
    {
    public:
        MemoryTracer();
        ~MemoryTracer();
        void recordMalloc(void *ptr, size_t sz);
        void recordFree(void *ptr);
        void dumpResults(const char *file, size_t minBytes);
        void clearResults();

    private:
        struct MemoryInfo
        {
            MemoryInfo(StackNode* _node = 0, size_t _bytes = 0): node(_node), bytes(_bytes){}
            StackNode* node;
            size_t     bytes;
        };

    private:
        typedef std::map<void*, MemoryInfo> StackNodeMap;

    private:
        StackNode       m_root;
        StackNodeMap    m_mp;
    };

    MemoryTracer::MemoryTracer():
    m_root(NULL, NULL)
    {
    }

    MemoryTracer::~MemoryTracer()
    {
        clearResults();
    }

    void MemoryTracer::clearResults()
    {
        m_mp.clear();
        m_root.deleteChildren();
        stackNodePool_clear();
    }

    void MemoryTracer::recordMalloc(void *ptr, size_t sz)
    {
        assert(ptr != NULL && sz > 0);

        static VoidPtrVec callStack;
        callStack.clear();
        captureStacks(callStack, 3);
        if (callStack.empty()) return;

        assert(m_mp.count(ptr) == 0);
        m_mp[ptr] = MemoryInfo(m_root.addRecord(callStack, sz), sz);
    }

    void MemoryTracer::recordFree(void *ptr)
    {
        assert(ptr != NULL);

        StackNodeMap::iterator iter = m_mp.find(ptr);
        if (iter != m_mp.end())
        {
            iter->second.node->removeRecord(iter->second.bytes, &m_root);
            m_mp.erase(iter);
        }
    }

    void MemoryTracer::dumpResults(const char *fileName, size_t minDumpBytes)
    {
        StringVec stackDescrib;
        Win32::DbgHelpWrapper helper;
        std::multimap<float, std::string> out;
        m_root.dumpRecord(out, minDumpBytes, stackDescrib, &helper);

        if (!out.empty())
        {
            FILE *f = fileName != NULL ? fopen(fileName, "w") : NULL;

            float totalBytes = 0;

            char buf[256] = "";
            size_t idx = 0;
            for (std::multimap<float, std::string>::reverse_iterator iter = out.rbegin();
                iter != out.rend();
                ++iter)
            {
                totalBytes += iter->first;

                sprintf(buf, "(%d) 检测到泄漏%.6f MB, ", idx++, iter->first);
                std::string outS = buf + iter->second;

                if (f == NULL) OutputDebugString(outS.c_str());
                else fprintf(f, "%s", outS.c_str());
            }

            size_t realTotalBytes = 0;
            {
                std::vector<StackNode*> vec;
                vec.push_back(&m_root);
                while (!vec.empty())
                {
                    StackNode *node = vec.back();
                    vec.pop_back();
                    if (!node->children.empty())
                    {
                        vec.insert(vec.end(), node->children.begin(), node->children.end());
                    }

                    realTotalBytes += node->nodeBytes;
                }
            }

            sprintf(buf, "总计泄漏%.6f (%.6f) MB\n", totalBytes, realTotalBytes / float(1000 * 1000));
            if (f == NULL) OutputDebugString(buf);
            else fprintf(f, "%s", buf);

            if (f != NULL) fclose(f);
        }
    }

    class Hook_MSVC;
    class CrtAllocationHooker
    {
    public:
        void init(size_t minHookBytes);
        void uninit();

        void dumpResults(size_t minDumpBytes, const char *file);
        void clearResults();

        static CrtAllocationHooker* getSingletonPtr();

    public:
        void _recordMalloc(void* ptr, size_t sz);
        void _recordRealloc(void *newPtr, void* oldPtr, size_t newSize);
        void _recordFree(void *ptr);

    private:
        CrtAllocationHooker();
        ~CrtAllocationHooker();

        static void _onLoadLibrary(HMODULE h) { getSingletonPtr()->onLoadLibrary(h); }
        void onLoadLibrary(HMODULE h);

    private:
        Hook_MSVC   *m_hooker;
        MemoryTracer m_tracker;
        bool         m_enableHook;
        size_t       m_minHookBytes;
        Win32::CriticalSection m_cs;
    };

    class Hook_MSVC
    {
    public:
        Hook_MSVC();
        ~Hook_MSVC();

        static void * __cdecl on_nh_malloc_dbg (size_t nSize, int nhFlag, int nBlockUse, const char * szFileName, int nLine);
        static void * __cdecl on_nh_malloc_base(size_t size, int nhFlag);
        static void * __cdecl on_realloc_dbg(void * pUserData, size_t nNewSize, int nBlockUse, const char * szFileName,int nLine);
        static void * __cdecl on_realloc_base (void * pBlock, size_t newsize);
        static void __cdecl on_free_dbg(void * pUserData, int nBlockUse);
        static void __cdecl on_free_base (void * pBlock);

    private:
        static FuncHook    *cs_mallocHook;
        static FuncHook    *cs_freeHook;
        static FuncHook    *cs_reallocHook;
    };

    FuncHook    *Hook_MSVC::cs_mallocHook = NULL;
    FuncHook    *Hook_MSVC::cs_freeHook = NULL;
    FuncHook    *Hook_MSVC::cs_reallocHook = NULL;

    Hook_MSVC::Hook_MSVC()
    {        
#ifdef _DEBUG
        size_t asmAlign[3] =
        {
#if _MSC_VER == 1310 || _MSC_VER == 1300 
            10, 10, 10,
#elif _MSC_VER == 1400
            10, 10, 7,
#elif _MSC_VER == 1500
			7, 7, 8,
#endif
        };

        void *free_addr = *(int*)((char*)free + 10) + (char*)free + 14;
        void *realloc_addr = *(int*)((char*)realloc + 19) + (char*)realloc + 23;
        void *malloc_addr = *(int*)((char*)malloc + 21) + (char*)malloc + 25;
#if _MSC_VER == 1500
		malloc_addr = *(int*)((char*)malloc + 23) + (char*)malloc + 27;
		realloc_addr = *(int*)((char*)realloc + 21) + (char*)realloc + 25;
		free_addr = *(int*)((char*)free + 12) + (char*)free + 16;
#endif
        cs_freeHook = new FuncHook(free_addr, on_free_dbg, asmAlign[0]);
        cs_reallocHook = new FuncHook(realloc_addr, on_realloc_dbg, asmAlign[1]);
        cs_mallocHook = new FuncHook(malloc_addr, on_nh_malloc_dbg, asmAlign[2]);
#else
        size_t asmAlign[3] =
        {
#if _MSC_VER == 1310 || _MSC_VER == 1300 
            7, 7, 7,
#elif _MSC_VER == 1400
            7, 7, 8,
#elif _MSC_VER == 1500
			7, 7, 9,
#endif
        };

        void *free_addr = free;
        void *realloc_addr = realloc;
        void *malloc_addr = *(int*)((char*)malloc + 11) + (char*)malloc + 15;
#if _MSC_VER == 1400
		malloc_addr = malloc;
#elif _MSC_VER == 1500
		malloc_addr = malloc;
#endif
        cs_freeHook = new FuncHook(free_addr, on_free_base, asmAlign[0]);
        cs_reallocHook = new FuncHook(realloc_addr, on_realloc_base, asmAlign[1]);
        cs_mallocHook = new FuncHook(malloc_addr, on_nh_malloc_base, asmAlign[2]);
#endif
    }

    Hook_MSVC::~Hook_MSVC()
    {
        delete cs_freeHook;
        cs_freeHook = NULL;
        delete cs_reallocHook;
        cs_reallocHook = NULL;
        delete cs_mallocHook;
        cs_mallocHook = NULL;
    }

    void * __cdecl Hook_MSVC::on_nh_malloc_dbg (size_t nSize, int nhFlag, int nBlockUse, const char * szFileName, int nLine)
    {
        void *ret = cs_mallocHook->getOldFunc(on_nh_malloc_dbg)(nSize, nhFlag, nBlockUse, szFileName, nLine);
        CrtAllocationHooker::getSingletonPtr()->_recordMalloc(ret, nSize);
        return ret;
    }

    void * __cdecl Hook_MSVC::on_nh_malloc_base(size_t size, int nhFlag)
    {
        void *ret = cs_mallocHook->getOldFunc(on_nh_malloc_base)(size, nhFlag);
        CrtAllocationHooker::getSingletonPtr()->_recordMalloc(ret, size);
        return ret;
    }

    void * __cdecl Hook_MSVC::on_realloc_dbg(void * pUserData, size_t nNewSize, int nBlockUse, const char * szFileName,int nLine)
    {
        void *ret = cs_reallocHook->getOldFunc(on_realloc_dbg)(pUserData, nNewSize, nBlockUse, szFileName, nLine);
        // 如果pUserData为空, 上面的调用会触发malloc; 如果nNewSize为0, 会触发free
        if (pUserData != NULL && nNewSize > 0)
        {
            CrtAllocationHooker::getSingletonPtr()->_recordRealloc(ret, pUserData, nNewSize);
        }
        return ret;
    }

    void * __cdecl Hook_MSVC::on_realloc_base (void * pBlock, size_t newsize)
    {
        void *ret = cs_reallocHook->getOldFunc(on_realloc_base)(pBlock, newsize);
        // 如果pBlock为空, 上面的调用会触发malloc; 如果newsize为0, 会触发free
        if (pBlock != NULL && newsize > 0)
        {
            CrtAllocationHooker::getSingletonPtr()->_recordRealloc(ret, pBlock, newsize);
        }
        return ret;
    }

    void __cdecl Hook_MSVC::on_free_dbg(void * pUserData, int nBlockUse)
    {
        cs_freeHook->getOldFunc(on_free_dbg)(pUserData, nBlockUse);
        CrtAllocationHooker::getSingletonPtr()->_recordFree(pUserData);
    }

    void __cdecl Hook_MSVC::on_free_base(void * pBlock)
    {
        cs_freeHook->getOldFunc(on_free_base)(pBlock);
        CrtAllocationHooker::getSingletonPtr()->_recordFree(pBlock);
    }


    CrtAllocationHooker::CrtAllocationHooker():
    m_enableHook(true), m_minHookBytes(1), m_hooker(NULL)
    {
        addLoadLibraryListener(_onLoadLibrary);
    }

    CrtAllocationHooker::~CrtAllocationHooker()
    {
        uninit();

        removeLoadLibraryListener(_onLoadLibrary);
    }

    inline CrtAllocationHooker* CrtAllocationHooker::getSingletonPtr()
    {
        static CrtAllocationHooker ls_ins;
        return &ls_ins;
    }   

    void CrtAllocationHooker::init(size_t minHookBytes)
    {
        m_hooker = new Hook_MSVC;
        m_minHookBytes = std::max<size_t>(minHookBytes, 1);
    }

    void CrtAllocationHooker::uninit()
    {
        m_minHookBytes = 1;

        delete m_hooker;
        m_hooker = NULL;
    }

    void CrtAllocationHooker::_recordMalloc(void* ptr, size_t sz)
    {
        Win32::SingleLocker locker(&m_cs);

        if (ptr != NULL && sz >= m_minHookBytes && m_enableHook)
        {
            try
            {
                m_enableHook = false;
                m_tracker.recordMalloc(ptr, sz);
                m_enableHook = true;
            }
            catch(...)
            {
                m_enableHook = true;
            }
        }
    }

    void CrtAllocationHooker::_recordRealloc(void *newPtr, void* oldPtr, size_t newSize)
    {
        Win32::SingleLocker locker(&m_cs);

        try
        {
            m_enableHook = false;
            if (oldPtr != NULL)
            {
                m_tracker.recordFree(oldPtr);
            }
            if (newPtr != NULL && newSize > m_minHookBytes)
            {
                m_tracker.recordMalloc(newPtr, newSize);
            }
            m_enableHook = true;
        }
        catch(...)
        {
            m_enableHook = true;
        }
    }

    void CrtAllocationHooker::_recordFree(void *ptr)
    {
        Win32::SingleLocker locker(&m_cs);

        if (ptr != NULL && m_enableHook)
        {
            try
            {
                m_enableHook = false;
                m_tracker.recordFree(ptr);
                m_enableHook = true;
            }
            catch(...)
            {
                m_enableHook = true;
            }
        }
    }

    void CrtAllocationHooker::dumpResults(size_t minDumpBytes, const char *file)
    {
        Win32::SingleLocker locker(&m_cs);

        try
        {
            m_enableHook = false;
            m_tracker.dumpResults(file, std::max<size_t>(minDumpBytes, 1));
            m_enableHook = true;
        }
        catch(...)
        {
            m_enableHook = true;
        }
    }

    void CrtAllocationHooker::onLoadLibrary(HMODULE h)
    {
        Win32::SingleLocker locker(&m_cs);

        try
        {
            m_enableHook = false;
            // 保证捕捉的栈信息准确
            resetCodeSegRangeForCaptureStack();
            m_enableHook = true;
        }
        catch(...)
        {
            m_enableHook = true;
        }
    }

    void CrtAllocationHooker::clearResults()
    {
        assert(m_hooker == NULL);

        m_tracker.clearResults();
    }
}
void crtAllocation_beginHook(size_t minHookBytes)
{
    CrtAllocationHooker::getSingletonPtr()->init(minHookBytes);
}

void crtAllocation_endHook()
{
    CrtAllocationHooker::getSingletonPtr()->uninit();
}

void crtAllocation_dumpResults(size_t minDumpBytes, const char *file)
{
    CrtAllocationHooker::getSingletonPtr()->dumpResults(minDumpBytes, file);
}

void crtAllocation_clearResults()
{
    CrtAllocationHooker::getSingletonPtr()->clearResults();
}