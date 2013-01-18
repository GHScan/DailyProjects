#pragma once

#include <Windows.h>

#include "Types.h"
#include "Utility.h"

namespace Scan
{
    uint64  getCpuFrequency();
    uint64  getInstructionCount();

    /**
            @brief ���ش��������ĸ���
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
        @brief ͬ������
    */
    class ISyncObject
    {
    public:
        virtual ~ISyncObject() = 0{}

        /**
            @brief ����
            @param waitTime ����ʱ��
            @return ͬ���������ź�ʱ����true
        */
        virtual bool lock(uint32 waitTime = -1) = 0;

        /**
            @brief ����
        */
        virtual void unlock() = 0;
    };

    /**
        @brief ��ʵ��
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
        @brief ͬ�������raii
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
            @brief ����windows api���ô��������ַ���
    */
        String getWindowsErrorString();

    /**
            @brief ��ָ��windows api������ת��Ϊ�����ַ���
            @param error Ҫ�����Ĵ�����. ����GetLastError()
    */
        String getWindowsErrorString(uint32 error);

        /**
            @brief �������
            ͬһ�̲߳�����ͬһ�����ϵݹ���ס; ��, ��������ڶ�ÿ���߳��м���
        */
        class Mutex: 
            public ISyncObject
        {
        public:
            /**
                @brief ����
                @param name �����ں˶��������
                @param isInitiaOwner �Ƿ񴴽�����̼߳������ź�;true�Ļ��൱������lock
            */
            Mutex(const String& name = "", bool isInitiaOwner = false);
            ~Mutex();

            bool    lock(uint32 waitTime = -1);
            void    unlock();

            /**
                @brief �Ƿ�����������Ѿ�����
            */
            bool    isAlreadyExist() const  { return m_isExist; }

            /**
                @brief ���ԭʼ���
            */
            void*   getHandle() const       { return m_h; }

            /**
                @brief ��ָ��������ڸö���Ĺ���֮��
            */
            void    attach(void* h);

            /**
                @brief �����ڲ�����Ĺ���Ȩ
                @return �������еľ��
            */
            void*   detach();
            bool    isValid() const         { return m_h != NULL; }
            const String& getName() const   { return m_name; }

        private:
            /**
                @brief ��ֹ����
            */
            Mutex(const Mutex&);
            Mutex& operator = (const Mutex&);

        private:
            void*           m_h;
            bool            m_isExist;
            String          m_name;
        };

        /**
            @brief �ٽ�������
            ͬһ�̲߳�����ͬһ�����ϵݹ���ס; ��, �ٽ��������ÿ���߳��м���
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
                @brief ��ֹ����
            */
            CriticalSection(const CriticalSection&);
            CriticalSection& operator = (const CriticalSection&);

        private:
            CRITICAL_SECTION m_cs;
        };

        /**
            @brief �ź�
            ͬһ�̻߳���ͬһ�����ϵݹ���ס; ��, �źŶ�����û���̼߳���
        */
        class Event: 
            public ISyncObject
        {
        public:
            /**
                @brief ����
                @param name �ں˶�����
                @param isManuReset ��ÿ��lock��, �Ƿ���Զ���Ϊ���ź�
                @param isInitSignaled ��ʼ��ʱ�������ź�
            */
            Event(
                const String& name = "",
                bool isManuReset = false, 
                bool isInitSignaled = true);

            ~Event();

            bool    lock(uint32 waitTime = -1);
            void    unlock();

            /**
                @brief ���������Ѿ�����
            */
            bool    isAlreadyExist() const      { return m_isExist; }

            /**
                @brief ԭʼ���
            */
            void*   getHandle() const           { return m_h; }

            /**
                @brief ��ָ��������ڸö���Ĺ���֮��
            */
            void    attach(void* h);

            /**
                @brief �����ڲ�����Ĺ���Ȩ
                @return �������еľ��
            */
            void*   detach();
            bool    isValid() const             { return m_h != NULL; }
            const String& getName() const       { return m_name; }

            /**
                @brief ǿ�ƽ�������Ϊ���ź�
            */
            void    setEvent();

            /**
                @brief ǿ�ƽ�������Ϊ���ź�
            */
            void    resetEvent();

        private:
            /**
                @brief ��ֹ����
            */
            Event(const Event&);
            Event& operator = (const Event&);

        private:
            void*       m_h;
            bool        m_isExist;
            String      m_name;
        };

    /**
            @brief ���ֽ�tls
            ����ʵ�ִ������
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
            @brief ָ���tls
            ��ĳ�߳��ϸ�tlsΪ��ʱ, Ϊ�Զ�����
            @remarks ��ʹ�ù�tls���߳���Ҫ����threadFree, �����ڴ�й©
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