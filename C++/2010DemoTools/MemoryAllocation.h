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
            @brief ��Ч���ڴ����
            ��windows�ϱ�malloc�����������50%
        */
        inline void* mallocFast(uint32 iSize)
        {
            return nedalloc::nedmalloc(iSize);
        }

        /**
            @brief ��Ч���ڴ��ط���
        */
        inline void* reallocFast(void *p, uint32 iNewSize)
        {
            return nedalloc::nedrealloc(p, iNewSize);
        }

        /**
            @brief ��Ч�ڴ��ͷ�
        */
        inline void  freeFast(void *p)
        {
            nedalloc::nedfree(p);
        }

        /**
            @brief ��Ϊ����, �ṩ�ڴ����ѡ�񷽰�
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
            @brief nedmalloc�ĸ�Ч�ڴ���䷽��
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
            @brief ջ�ڴ���䷽��
            @remarks ջ������̫��, ����������
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
            @brief �ڴ��, boost��ʵ��
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
            @brief ��С�̶���Ƶ���ѷ���������
            �ٶȱ�Ĭ�����10��; new[]��delete[]��Ч
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