#include "StdAfx.h"

#include "Win32APIWrapper.h"

#include <windows.h>
#include <DbgHelp.h>

#pragma comment(lib, "dbghelp")

DbgHelpWrapper::DbgHelpWrapper(void)
{
    SymSetOptions(SYMOPT_UNDNAME | SYMOPT_DEFERRED_LOADS);

    m_process = OpenProcess(PROCESS_ALL_ACCESS, FALSE, GetCurrentProcessId());

    if (!SymInitialize(m_process, NULL, TRUE))
    {
        CloseHandle(m_process);
        m_process = NULL;
    }
}

DbgHelpWrapper::~DbgHelpWrapper(void)
{
    if (m_process)
    {
        SymCleanup(m_process);

        CloseHandle(m_process);
        m_process = NULL;
    }
}

bool DbgHelpWrapper::addressToFileLine(void* ptr, std::string& file, int &line)
{
    DWORD  displacement = 0;
    IMAGEHLP_LINE64 imgLine;
    imgLine.SizeOfStruct = sizeof(imgLine);

    if (SymGetLineFromAddr64(m_process, (DWORD64)ptr, &displacement, &imgLine))
    {
        file = imgLine.FileName;
        line = imgLine.LineNumber;
        return true;
    }
    return false;
}

bool DbgHelpWrapper::addressToModuleName(void *ptr, std::string& moduleName)
{
    MEMORY_BASIC_INFORMATION info = {0};
    if (VirtualQuery(ptr, &info, sizeof(info)) != 0)
    {
        HMODULE module = (HMODULE)info.AllocationBase;

        ModuleNameMap::iterator iter = m_moduleNames.lower_bound(module);
        if (iter == m_moduleNames.end() || iter->first != module)
        {
            char buf[128] = "";
            if (GetModuleFileName(module, buf, sizeof(buf)) != 0)
            {
                moduleName = buf;
                m_moduleNames.insert(iter, ModuleNameMap::value_type(module, buf));
                return true;
            }
        }
        else
        {
            moduleName = iter->second;
            return true;
        }
    }
    return false;
}


ToolHelp32Iterator::ToolHelp32Iterator(HelpType ht, DWORD processID)
{
    m_snapShot = CreateToolhelp32Snapshot(ht, processID);
    m_ht = ht;

    if (m_snapShot != NULL)
    {
        switch (m_ht)
        {
        case HT_Process:
            m_data.process.dwSize = sizeof(m_data.process);
            m_hasMore = Process32First(m_snapShot, &m_data.process) == TRUE;
            break;
        case HT_Module:
            m_data.module.dwSize = sizeof(m_data.module);
            m_hasMore = Module32First(m_snapShot, &m_data.module) == TRUE;
            break;
        default:
            break;
        }
    }
}

ToolHelp32Iterator::~ToolHelp32Iterator(void)
{
    if (m_snapShot != NULL)
    {
        CloseHandle(m_snapShot);
        m_snapShot = NULL;
    }
}

bool ToolHelp32Iterator::hasMore() const
{
    return m_hasMore;
}

const MODULEENTRY32& ToolHelp32Iterator::peekNextModule() const
{
    return m_data.module;
}   

const MODULEENTRY32 ToolHelp32Iterator::getNextModule()
{
    MODULEENTRY32 module = m_data.module;
    m_hasMore = Module32Next(m_snapShot, &m_data.module) == TRUE;
    return module;
}

const PROCESSENTRY32& ToolHelp32Iterator::peekNextProcess() const
{
    return m_data.process;   
}

const PROCESSENTRY32 ToolHelp32Iterator::getNextProcess()
{
    PROCESSENTRY32 process = m_data.process;
    m_hasMore = Process32Next(m_snapShot, &m_data.process) == TRUE;
    return process;
}

void ToolHelp32Iterator::moveNext()
{   
    switch (m_ht)
    {
    case HT_Process:
        m_hasMore = Process32Next(m_snapShot, &m_data.process) == TRUE;
        break;
    case HT_Module:
        m_hasMore = Module32Next(m_snapShot, &m_data.module) == TRUE;
        break;
    default:
        break;
    }
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