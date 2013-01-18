#pragma once

#include <boost/pool/pool.hpp>

#include "nedmalloc/nedmalloc.h"

#include "PlatformDepends.h"
#include "Types.h"

namespace Scan
{
    namespace MemoryAllocation
    {
        #if defined(_DEBUG) || defined(DEBUG)
    void* allocMemoryCheck(size_t memSize, const char *fileName, unsigned int line);
    void freeMemoryCheck(void* memChunk, const char *fileName, unsigned int line);
#endif

        /**
            @brief 高效的内存分配
            在windows上比malloc性能提升大概50%
        */
        inline void* mallocFast(uint32 iSize)
        {
            return nedalloc::nedmalloc(iSize);
        }

        /**
            @brief 高效的内存重分配
        */
        inline void* reallocFast(void *p, uint32 iNewSize)
        {
            return nedalloc::nedrealloc(p, iNewSize);
        }

        /**
            @brief 高效内存释放
        */
        inline void  freeFast(void *p)
        {
            nedalloc::nedfree(p);
        }

        /**
            @brief 作为基类, 提供内存分配选择方案
        */
        template<typename MemoryAllocatorType>
        struct CustomMemAllocObject
        {
        public:

            void operator delete(void* p)
            {
#if defined(_DEBUG) || defined(DEBUG)
                free(p);
#else       
                MemoryAllocatorType::free(p);
#endif
            }

            void operator delete(void* p, const char* szFileName, int iLine)
            {
#if defined(_DEBUG) || defined(DEBUG)
                freeMemoryCheck(p, szFileName, iLine);
#else
                delete p;
#endif
            }

            void* operator new(uint32 iSize)
            {
#if defined(_DEBUG) || defined(DEBUG)
                return malloc(iSize);
#else
                return MemoryAllocatorType::malloc(iSize);
#endif
            }

            void* operator new(uint32 iSize, const char* szFileName, int iLine)
            {
#if defined(_DEBUG) || defined(DEBUG)
                return allocMemoryCheck(iSize, szFileName, iLine);
#else
                return operator new (iSize);
#endif
            }

            void operator delete[](void* p)
            {
#if defined(_DEBUG) || defined(DEBUG)
                free(p);
#else       
                MemoryAllocatorType::free(p);
#endif
            }

            void operator delete[](void* p, const char* szFileName, int iLine)
            {
#if defined(_DEBUG) || defined(DEBUG)
                freeMemoryCheck(p, szFileName, iLine);
#else
                delete[] p;
#endif
            }

            void* operator new[](uint32 iSize)
            {
#if defined(_DEBUG) || defined(DEBUG)
                return malloc(iSize);
#else
                return MemoryAllocatorType::malloc(iSize);
#endif
            }

            void* (operator new[])(uint32 iSize, const char* szFileName, int iLine)
            {
#if defined(_DEBUG) || defined(DEBUG)
                return allocMemoryCheck(iSize, szFileName, iLine);
#else
                return operator new[] (iSize);
#endif
            }

        protected:
            CustomMemAllocObject(){}
            ~CustomMemAllocObject() {}
        };

        /**
            @brief nedmalloc的高效内存分配方案
        */
        struct NemallocMemoryAllocator
        {
            static void* malloc(uint32 bytes)
            {
                return mallocFast(bytes);
            }
            static void free(void *p)
            {
                freeFast(p);
            }
        };

        /**
            @brief 栈内存分配方案
            @remarks 栈对象不能太大, 否则可能溢出
        */
        struct StackMemoryAllocator
        {
            static void* malloc(uint32 bytes)
            {
                return _malloca(bytes);
            }
            static void free(void *p)
            {
                _freea(p);
            }
        };

        typedef CustomMemAllocObject<NemallocMemoryAllocator>  FastMemAllocObject;
        typedef CustomMemAllocObject<StackMemoryAllocator>     StackMemAllocObject;

        /**
            @brief 内存池, boost的实现
        */
        template<typename T, typename MutexType>
        class MemoryPool
        {
        public:
            MemoryPool(): m_pool(sizeof(T)){}

            void* malloc()
            {
                SingleLocker guard(m_mutex);
                return m_pool.malloc();
            }

            void free(void* p)
            {
                SingleLocker guard(m_mutex);
                m_pool.free(p);
            }

        private:
            // MemoryPool(const MemoryPool&);
            // MemoryPool& operator = (const MemoryPool&);

        private:
            boost::pool<>   m_pool;
            MutexType       m_mutex;
        };

        /**
            @brief 大小固定的频繁堆分配对象基类
            速度比默认提高10倍; new[]和delete[]无效
        */
        template<typename T, typename PoolMutexType = EmptyMutex>
        struct SizefixedMemAllocObject
        {
            void operator delete(void* p)
            {
#if defined(_DEBUG) || defined(DEBUG)
                free(p);
#else
                getPool().free(p);
#endif
            }

            void operator delete(void* p, const char* szFileName, int iLine)
            {
#if defined(_DEBUG) || defined(DEBUG)
                freeMemoryCheck(p, szFileName, iLine);
#else
                delete p;
#endif
            }

            void* operator new(uint32 iSize)
            {
#if defined(_DEBUG) || defined(DEBUG)
                return malloc(iSize);
#else
                return getPool().malloc();
#endif
            }

            void* operator new(uint32 iSize, const char* szFileName, int iLine)
            {
#if defined(_DEBUG) || defined(DEBUG)
                return allocMemoryCheck(iSize, szFileName, iLine);
#else
                return operator new (iSize);
#endif
            }

            void operator delete[](void* p, const char* szFileName, int iLine)
            {
#if defined(_DEBUG) || defined(DEBUG)
                freeMemoryCheck(p, szFileName, iLine);
#else
                delete[] p;
#endif
            }

            void* (operator new[])(uint32 iSize, const char* szFileName, int iLine)
            {
#if defined(_DEBUG) || defined(DEBUG)
                return allocMemoryCheck(iSize, szFileName, iLine);
#else
                return operator new[] (iSize);
#endif
            }

        protected:
            SizefixedMemAllocObject() {}
            ~SizefixedMemAllocObject() {}

        private:
            static MemoryPool<T, PoolMutexType>& getPool()
            {
                static MemoryPool<T, PoolMutexType>  ls_pool;
                return ls_pool;
            }
        };
    }
}