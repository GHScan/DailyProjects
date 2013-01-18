#pragma once

#include <Windows.h>

#include "Types.h"
#include "Utility.h"

namespace Scan
{
    uint64  getCpuFrequency();
    uint64  getInstructionCount();

    /**
            @brief 返回处理器核心个数
    */
    uint32 getProcessorCount();

    uint32  getCurrentTID();

    inline double calcElapseTime(uint64 beginCounter)
    {
        double d = double(getInstructionCount());
        d -= double(beginCounter);
        return d / getCpuFrequency();
    }

    void printDebug(const char* msg);
    void printVCDebug(const char *msg, const char *file, int line, const char *func);

    /**
        @brief 同步对象
    */
    class ISyncObject
    {
    public:
        virtual ~ISyncObject() = 0{}

        /**
            @brief 加锁
            @param waitTime 锁的时间
            @return 同步对象有信号时返回true
        */
        virtual bool lock(uint32 waitTime = -1) = 0;

        /**
            @brief 解锁
        */
        virtual void unlock() = 0;
    };

    /**
        @brief 空实现
    */ 
    class EmptyMutex:
        public ISyncObject
    {
    public:
        EmptyMutex(){}

        bool    lock(uint32 waitTime = -1)  { return true; }
        void    unlock(){}

    private:
        EmptyMutex(const EmptyMutex&);
        EmptyMutex& operator = (const EmptyMutex&);
    };

    inline ISyncObject* getEmptyMutex()
    {
        static EmptyMutex ls_mutex;
        return &ls_mutex;
    }

    /**
        @brief 同步对象的raii
    */
    class SingleLocker
    {
    public:
        SingleLocker(ISyncObject *o):
          m_obj(o)
          {
              m_obj->lock();
          }
        SingleLocker(ISyncObject &o):
          m_obj(&o)
          {
              m_obj->lock();
          }
        
        ~SingleLocker()
        {
            m_obj->unlock();
        }

    private:
        SingleLocker(const SingleLocker&);
        SingleLocker& operator = (const SingleLocker&);

    private:
        ISyncObject *m_obj;
    };

    namespace Windows
    {
        /**
            @brief 返回windows api调用错误描述字符串
    */
        String getWindowsErrorString();

    /**
            @brief 将指定windows api错误码转化为描述字符串
            @param error 要描述的错误码. 来自GetLastError()
    */
        String getWindowsErrorString(uint32 error);

        /**
            @brief 互斥对象
            同一线程不会在同一对象上递归锁住; 即, 互斥对象内对每个线程有计数
        */
        class Mutex: 
            public ISyncObject
        {
        public:
            /**
                @brief 构造
                @param name 命名内核对象的名称
                @param isInitiaOwner 是否创建后该线程即持有信号;true的话相当于立刻lock
            */
            Mutex(const String& name = "", bool isInitiaOwner = false);
            ~Mutex();

            bool    lock(uint32 waitTime = -1);
            void    unlock();

            /**
                @brief 是否该命名对象已经存在
            */
            bool    isAlreadyExist() const  { return m_isExist; }

            /**
                @brief 获得原始句柄
            */
            void*   getHandle() const       { return m_h; }

            /**
                @brief 将指定句柄至于该对象的管理之下
            */
            void    attach(void* h);

            /**
                @brief 放弃内部句柄的管理权
                @return 曾经持有的句柄
            */
            void*   detach();
            bool    isValid() const         { return m_h != NULL; }
            const String& getName() const   { return m_name; }

        private:
            /**
                @brief 禁止拷贝
            */
            Mutex(const Mutex&);
            Mutex& operator = (const Mutex&);

        private:
            void*           m_h;
            bool            m_isExist;
            String          m_name;
        };

        /**
            @brief 临界区对象
            同一线程不会在同一对象上递归锁住; 即, 临界区对象对每个线程有计数
        */
        class CriticalSection: 
            public ISyncObject
        {
        public:
            CriticalSection();
            ~CriticalSection();

            bool lock(uint32 waitTime = -1);
            void unlock();

        private:
            /**
                @brief 禁止拷贝
            */
            CriticalSection(const CriticalSection&);
            CriticalSection& operator = (const CriticalSection&);

        private:
            CRITICAL_SECTION m_cs;
        };

        /**
            @brief 信号
            同一线程会在同一对象上递归锁住; 即, 信号对象内没有线程计数
        */
        class Event: 
            public ISyncObject
        {
        public:
            /**
                @brief 构造
                @param name 内核对象名
                @param isManuReset 在每次lock后, 是否会自动变为无信号
                @param isInitSignaled 初始的时候有无信号
            */
            Event(
                const String& name = "",
                bool isManuReset = false, 
                bool isInitSignaled = true);

            ~Event();

            bool    lock(uint32 waitTime = -1);
            void    unlock();

            /**
                @brief 命名对象已经存在
            */
            bool    isAlreadyExist() const      { return m_isExist; }

            /**
                @brief 原始句柄
            */
            void*   getHandle() const           { return m_h; }

            /**
                @brief 将指定句柄置于该对象的管理之下
            */
            void    attach(void* h);

            /**
                @brief 放弃内部句柄的管理权
                @return 曾经持有的句柄
            */
            void*   detach();
            bool    isValid() const             { return m_h != NULL; }
            const String& getName() const       { return m_name; }

            /**
                @brief 强制将对象置为有信号
            */
            void    setEvent();

            /**
                @brief 强制将对象置为无信号
            */
            void    resetEvent();

        private:
            /**
                @brief 禁止拷贝
            */
            Event(const Event&);
            Event& operator = (const Event&);

        private:
            void*       m_h;
            bool        m_isExist;
            String      m_name;
        };

    /**
            @brief 四字节tls
            用于实现错误码等
        */
        template<typename T>
        class TlsDWORD
        {
        public:
            TlsDWORD():
              m_tlsID(TlsAlloc())
              {
                  SCAN_STATIC_ASSERT(sizeof(T) == sizeof(DWORD));
              }

              ~TlsDWORD()
              {
                  TlsFree(m_tlsID);
              }

              T getValue() const
              {
                  return force_cast<T>(TlsGetValue(m_tlsID));
              }

              void setValue(T v)
              {
                  TlsSetValue(m_tlsID, force_cast<void*>(v));
              }

              operator T() const
              {
                  return getValue();
              }

        private:
            TlsDWORD(const TlsDWORD&);
            TlsDWORD& operator = (const TlsDWORD&);

        private:
            uint32  m_tlsID;
        };

        /**
            @brief 指针的tls
            在某线程上该tls为空时, 为自动分配
            @remarks 在使用过tls的线程上要调用threadFree, 否则内存泄漏
        */
        template<typename T>
        class TlsPtr:
            protected TlsDWORD<T*>
        {
        public:
            TlsPtr()
              {
              }

              operator T* () const
              {
                  return getPtr();
              }

              T* getPtr() const
              {
                  T *p = getValue();
                  if (p == NULL)
                  {
                      setValue(p = ClassAllocator<T>::alloc());
                  }
                  return p;
              }

              void resetPtr(T* newPtr) const
              {
                  T* p = getValue();
                  if (p != newPtr)
                  {
                      ClassAllocator<T>::free(p);
                      setValue(newPtr);
                  }
              }

              void onThreadEnd() const
              {
                  ClassAllocator<T>::free(getValue());
              }
        };
    }
}