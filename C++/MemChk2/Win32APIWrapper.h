#pragma once

#include <string>
#include <map>

#include <Windows.h>
#include <TlHelp32.h>

namespace Win32
{
    typedef unsigned uint32;

    class DbgHelpWrapper
    {
    public:
        DbgHelpWrapper(void);
        ~DbgHelpWrapper(void);

        bool addressToFileLine(void* ptr, std::string& file, int &line);
        bool addressToFuncName(void *ptr, std::string& funcName);
        bool addressToModuleName(void *ptr, std::string& moduleName);
        bool addressToString(void *ptr, std::string& ret);

    private:
        typedef std::map<HMODULE, std::string>  ModuleNameMap;

    private:
        HANDLE          m_process;
    };

    class ToolHelp32Iterator
    {
    public:
        enum HelpType
        {
            HT_Process = TH32CS_SNAPPROCESS,
            HT_Module = TH32CS_SNAPMODULE,
        };

    public:
        ToolHelp32Iterator(HelpType ht, DWORD processID = 0);
        ~ToolHelp32Iterator(void);

        bool hasMore() const;

        const MODULEENTRY32& peekNextModule() const;
        const MODULEENTRY32  getNextModule();

        const PROCESSENTRY32& peekNextProcess() const;
        const PROCESSENTRY32  getNextProcess();

        void moveNext();

    private:
        ToolHelp32Iterator(const ToolHelp32Iterator&);
        ToolHelp32Iterator& operator = (const ToolHelp32Iterator&);

    private:
        HelpType    m_ht;
        HANDLE      m_snapShot;
        union
        {
            MODULEENTRY32   module;
            PROCESSENTRY32  process;
        }   m_data;
        bool        m_hasMore;
    };

    struct ISyncObject
    {
    public:
        virtual ~ISyncObject() = 0{}
        virtual bool lock(uint32 waitTime = -1) = 0;
        virtual void unlock() = 0;
    };

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

    class CriticalSection: 
        public ISyncObject
    {
    public:
        CriticalSection();
        ~CriticalSection();

        bool lock(uint32 waitTime = -1);
        void unlock();

    private:
        CriticalSection(const CriticalSection&);
        CriticalSection& operator = (const CriticalSection&);

    private:
        CRITICAL_SECTION m_cs;
    };
}