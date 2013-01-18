#include "StdAfx.h"

#include <windows.h>

#include "PlatformDepends.h"
#include "Utility.h"

namespace Scan
{
    uint64 _getCpuFrequency()
    {
        LARGE_INTEGER li = {0};
        QueryPerformanceFrequency(&li);
        return uint64(li.QuadPart);
    }

    uint64  getCpuFrequency()
    {
        static uint64 ls_frequncy = _getCpuFrequency();
        return ls_frequncy;
    }

    uint32 getProcessorCount()
    {
        SYSTEM_INFO info;
        GetSystemInfo(&info);
        return info.dwNumberOfProcessors;
    }

    uint64  getInstructionCount()
    {
        LARGE_INTEGER li = {0};
        QueryPerformanceCounter(&li);
        return uint64(li.QuadPart);
    }

    uint32  getCurrentTID()
    {
        return GetCurrentThreadId();
    }

    void printDebug(const char* msg)
    {   
        OutputDebugString(msg);
    }

    void printVCDebug(const char *msg, const char *file, int line, const char *func)
    {
        printDebug(OutStream().format("%s(%d):(%s): %s", file, line, func, msg).getStream().str().c_str());
    }

    namespace Windows
    {
        Mutex::Mutex(const String& name/* = ""*/, bool isInitiaOwner/* = false*/)
            : m_name(name)
        {
            m_h = CreateMutex(
                NULL, 
                isInitiaOwner ? TRUE : FALSE, 
                m_name.c_str());

            m_isExist = GetLastError() == ERROR_ALREADY_EXISTS;
        }

        Mutex::~Mutex()
        {
            if (isValid())
            {
                CloseHandle(m_h);
            }
        }

        bool Mutex::lock(uint32 waitTime/* = INFINITE*/) 
        {
            if (!isValid())
            {
                return false;
            }

            return WaitForSingleObject(m_h, waitTime) == WAIT_OBJECT_0;
        }

        void Mutex::unlock()
        {
            if (isValid())
            {
                ReleaseMutex(m_h);
            }
        }

        void Mutex::attach(HANDLE h)
        {
            if (isValid())
            {
                CloseHandle(m_h);
            }
            m_h = h;
        }

        void* Mutex::detach()
        {
            void* hRet = m_h;
            m_h = NULL;
            return hRet;
        }

        CriticalSection::CriticalSection()
        {
            InitializeCriticalSection(&m_cs);
        }

        CriticalSection::~CriticalSection()
        {
            DeleteCriticalSection(&m_cs);
        }

        bool CriticalSection::lock(uint32 waitTime/* = -1*/)
        {
            EnterCriticalSection(&m_cs);
            return true;
        }

        void CriticalSection::unlock()
        {
            LeaveCriticalSection(&m_cs);
        }

        Event::Event(
            const String& name/* = ""*/,
            bool isManuReset/* = false*/, 
            bool isInitSignaled/* = true*/)
            : m_name(name)
        {
            m_h = CreateEvent(
                NULL, 
                isManuReset ? TRUE : FALSE, 
                isInitSignaled ? TRUE : FALSE, 
                m_name.c_str());

            m_isExist = GetLastError() == ERROR_ALREADY_EXISTS;
        }

        Event::~Event()
        {
            if (isValid())
            {
                CloseHandle(m_h);
            }
        }

        bool Event::lock(uint32 waitTime/* = -1*/)
        {
            if (!isValid())
            {
                return false;
            }

            return WaitForSingleObject(m_h, waitTime) == WAIT_OBJECT_0;
        }

        void Event::unlock()
        {
            if (isValid())
            {
                SetEvent(m_h);
            }
        }

        void Event::attach(void* h)
        {
            if (isValid())
            {
                CloseHandle(m_h);
            }
            m_h = h;
        }

        void* Event::detach()
        {
            void* hRet = m_h;
            m_h = NULL;
            return hRet;
        }

        void Event::setEvent()
        {
            SetEvent(m_h);
        }

        void Event::resetEvent()
        {
            ResetEvent(m_h);
        }

        String getWindowsErrorString()
        {
            return getWindowsErrorString(GetLastError());
        }

        String getWindowsErrorString(uint32 error)
        {
            char* lpMsgBuf;

            FormatMessage(
                FORMAT_MESSAGE_ALLOCATE_BUFFER | 
                FORMAT_MESSAGE_FROM_SYSTEM,
                NULL,
                error,
                MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                reinterpret_cast<char*>(&lpMsgBuf),
                0, NULL );

            String szTmp = lpMsgBuf;

            LocalFree(lpMsgBuf);

            return szTmp;
        }
    }
}