#include "StdAfx.h"

#include <cassert>

#include <map>
#include <vector>
#include <string>

#include <algorithm>

#include <Windows.h>
#include <Dbghelp.h>

#include "Win32APIWrapper.h"
#include "APIHook.h"

static void* _getMaxAppAddress()
{
    SYSTEM_INFO info = {0};
    GetSystemInfo(&info);
    return info.lpMaximumApplicationAddress;
}

static void* getMaxAppAddress()
{
    static void * ls_maxAddress = _getMaxAppAddress();
    return ls_maxAddress;
}

inline static FARPROC fixupProcAddress(FARPROC proc)
{
    if (proc > getMaxAppAddress())
    {
        struct FixupProc
        {
            char push_asmCode;
            FARPROC proc;
        };

         FixupProc *p = (FixupProc*)proc;
         // x86汇编push指令
         if (p->push_asmCode == 0x6a)
         {
            proc = p->proc;
         }
    }

    return proc;
}

APIHook::APIHook(const char *moduleName, const char* origin, void *fake):
m_fake((FARPROC)fake), m_moduleName(moduleName)
{
    m_origin = fixupProcAddress(GetProcAddress(GetModuleHandle(m_moduleName.c_str()), origin));
}   

APIHook::~APIHook(void)
{
}

static void hookModuleProc(HMODULE hookDest, const char *moduleName, FARPROC oldProc, FARPROC newProc)
{
    ULONG size;
    PIMAGE_IMPORT_DESCRIPTOR importDesrc = 
        (PIMAGE_IMPORT_DESCRIPTOR)ImageDirectoryEntryToData(hookDest, TRUE, IMAGE_DIRECTORY_ENTRY_IMPORT, &size);

    if (importDesrc == NULL)
    {
        return;
    }

    while (importDesrc->Name != NULL)
    {
        const char *importName = importDesrc->Name + (const char*)hookDest;
        if (stricmp(importName, moduleName) == 0)
        {
            break;
        }

        ++importDesrc;
    }

    if (importDesrc->Name == NULL)
    {
        return;
    }

    PIMAGE_THUNK_DATA thunk = (PIMAGE_THUNK_DATA)((char*)hookDest + importDesrc->FirstThunk);

    while (thunk->u1.Function != NULL)
    {
        FARPROC func = (FARPROC&)thunk->u1.Function;
        func = fixupProcAddress(func);

        if (func == oldProc)
        {
            SIZE_T writedBytes = 0;
            DWORD oldProtect;
            if (VirtualProtect(&thunk->u1.Function, sizeof(newProc), PAGE_READWRITE, &oldProtect))
            {
                WriteProcessMemory(GetCurrentProcess(), &thunk->u1.Function, &newProc, sizeof(newProc), &writedBytes);
                VirtualProtect(&thunk->u1.Function, sizeof(newProc), oldProtect, NULL);
            }
            return;
        }

        ++thunk;
    }
}

void APIHook::hookModule(HMODULE hookDest, bool hook)
{
    if (hook)
    {
        hookModuleProc(hookDest, m_moduleName.c_str(), m_origin, m_fake);
    }
    else
    {
        hookModuleProc(hookDest, m_moduleName.c_str(), m_fake, m_origin);
    }
}

FARPROC APIHook::getOriginProc()
{   
    return m_origin;
}

FARPROC APIHook::getFakeProc()
{
    return m_fake;
}

class AppAPIHookManager
{
public:
    void addHook(AppAPIHook* hook);
    void removeHook(AppAPIHook* hook);

    static AppAPIHookManager* getSingletonPtr();

    void addLoadLibraryListener(LoadLibraryListener l);
    void removeLoadLibraryListener(LoadLibraryListener l);

private:
    AppAPIHookManager();
    ~AppAPIHookManager();

    void applyToAllModule(AppAPIHook *hooker, bool hook);
    void applyToAllHook(HMODULE module, bool hook);

    static HMODULE WINAPI _LoadLibraryA(PCSTR pszModulePath);
    static HMODULE WINAPI _LoadLibraryW(PCWSTR pszModulePath);
    static HMODULE WINAPI _LoadLibraryExA(PCSTR pszModulePath, HANDLE hFile, DWORD dwFlags);
    static HMODULE WINAPI _LoadLibraryExW(PCWSTR pszModulePath, HANDLE hFile, DWORD dwFlags);
    static FARPROC WINAPI _GetProcAddress(HMODULE hmod, PCSTR pszProcName);

    void fireLibraryLoaded(HMODULE h);
    
private:
    typedef std::map<std::string, AppAPIHook*>  APIHookMap;
    typedef std::vector<LoadLibraryListener>    LoadLibraryListenerVec;

private:
    APIHookMap   m_hookMap;
    AppAPIHook  *m_innerHooker[5];
    HMODULE      m_thisModule;
    LoadLibraryListenerVec  m_loadLibListeners;
    Win32::CriticalSection  m_cs;
};

void addLoadLibraryListener(LoadLibraryListener l)
{
    AppAPIHookManager::getSingletonPtr()->addLoadLibraryListener(l);
}

void removeLoadLibraryListener(LoadLibraryListener l)
{
    AppAPIHookManager::getSingletonPtr()->removeLoadLibraryListener(l);
}

void AppAPIHookManager::addLoadLibraryListener(LoadLibraryListener l)
{
    Win32::SingleLocker locker(&m_cs);

    if (std::find(m_loadLibListeners.begin(), m_loadLibListeners.end(), l) == m_loadLibListeners.end())
    {
        m_loadLibListeners.push_back(l);
    }
}

void AppAPIHookManager::removeLoadLibraryListener(LoadLibraryListener l)
{
    Win32::SingleLocker locker(&m_cs);

    m_loadLibListeners.erase(
        std::remove(m_loadLibListeners.begin(), m_loadLibListeners.end(), l), m_loadLibListeners.end());
}

void AppAPIHookManager::fireLibraryLoaded(HMODULE h)
{
    if (h == NULL) return;

    for (LoadLibraryListenerVec::iterator iter = m_loadLibListeners.begin();
        iter != m_loadLibListeners.end();
        ++iter)
    {
        (*iter)(h);
    }
}

HMODULE WINAPI AppAPIHookManager::_LoadLibraryA(PCSTR pszModulePath)
{
    AppAPIHookManager *_this = getSingletonPtr();

    Win32::SingleLocker locker(&_this->m_cs);

    HMODULE h = _this->m_innerHooker[0]->getHooker()->getOriginProc(_LoadLibraryA)(pszModulePath);

    _this->applyToAllHook(h, true);
    _this->fireLibraryLoaded(h);

    return h;
}

HMODULE WINAPI AppAPIHookManager::_LoadLibraryW(PCWSTR pszModulePath)
{
    AppAPIHookManager *_this = getSingletonPtr();

    Win32::SingleLocker locker(&_this->m_cs);

    HMODULE h = _this->m_innerHooker[1]->getHooker()->getOriginProc(_LoadLibraryW)(pszModulePath);

    _this->applyToAllHook(h, true);
    _this->fireLibraryLoaded(h);

    return h;
}

HMODULE WINAPI AppAPIHookManager::_LoadLibraryExA(PCSTR pszModulePath, HANDLE hFile, DWORD dwFlags)
{
    AppAPIHookManager *_this = getSingletonPtr();

    Win32::SingleLocker locker(&_this->m_cs);

    HMODULE h = _this->m_innerHooker[2]->getHooker()->getOriginProc(_LoadLibraryExA)(pszModulePath, hFile, dwFlags);

    // 跳过资源dll
    if ((dwFlags & LOAD_LIBRARY_AS_DATAFILE) == 0)
    {
        _this->applyToAllHook(h, true);
        _this->fireLibraryLoaded(h);
    }

    return h;
}

HMODULE WINAPI AppAPIHookManager::_LoadLibraryExW(PCWSTR pszModulePath, HANDLE hFile, DWORD dwFlags)
{
    AppAPIHookManager *_this = getSingletonPtr();

    Win32::SingleLocker locker(&_this->m_cs);

    HMODULE h = _this->m_innerHooker[3]->getHooker()->getOriginProc(_LoadLibraryExW)(pszModulePath, hFile, dwFlags);

    // 跳过资源dll
    if ((dwFlags & LOAD_LIBRARY_AS_DATAFILE) == 0)
    {
        _this->applyToAllHook(h, true);
        _this->fireLibraryLoaded(h);
    }

    return h;
}

FARPROC WINAPI AppAPIHookManager::_GetProcAddress(HMODULE hmod, PCSTR pszProcName)
{
    Win32::SingleLocker locker(&getSingletonPtr()->m_cs);

    try
    {
        // 字符串有效性检测
        strlen(pszProcName);

        APIHookMap::iterator iter = getSingletonPtr()->m_hookMap.find(pszProcName);
        if (iter != getSingletonPtr()->m_hookMap.end())
        {
            return iter->second->getHooker()->getFakeProc();
        }

    }catch(...) {}

    FARPROC h = getSingletonPtr()->m_innerHooker[4]->getHooker()->getOriginProc(_GetProcAddress)(hmod, pszProcName);

    return h;
}

static HMODULE getThisModule()
{
    MEMORY_BASIC_INFORMATION info = {0};
    return VirtualQuery(&AppAPIHookManager::getSingletonPtr, &info, sizeof(info)) == 0 ? NULL : (HMODULE)info.AllocationBase;
}

AppAPIHookManager::AppAPIHookManager()
{
    m_thisModule = getThisModule();
#if HOOK_THIS_MODULE

    m_thisModule = NULL;
#endif

    // 有顺序的!!参见几个hook函数中使用的序号

    m_innerHooker[0] = new AppAPIHook("Kernel32.dll", "LoadLibraryA", &AppAPIHookManager::_LoadLibraryA);
    m_innerHooker[1] = new AppAPIHook("Kernel32.dll", "LoadLibraryW", &AppAPIHookManager::_LoadLibraryW);
    m_innerHooker[2] = new AppAPIHook("Kernel32.dll", "LoadLibraryExA", &AppAPIHookManager::_LoadLibraryExA);
    m_innerHooker[3] = new AppAPIHook("Kernel32.dll", "LoadLibraryExW", &AppAPIHookManager::_LoadLibraryExW);
    m_innerHooker[4] = new AppAPIHook("Kernel32.dll", "GetProcAddress", &AppAPIHookManager::_GetProcAddress);
}

AppAPIHookManager::~AppAPIHookManager()
{
    for (int i = 0; i < 5; ++i)
    {
        delete m_innerHooker[i];
    }
}

void AppAPIHookManager::applyToAllModule(AppAPIHook *hooker, bool hook)
{
    Win32::ToolHelp32Iterator iter(Win32::ToolHelp32Iterator::HT_Module);
    while (iter.hasMore())
    {
        HMODULE module = iter.getNextModule().hModule;
        if (module != m_thisModule)
        {
            hooker->getHooker()->hookModule(module, hook);
        }
    }
}

void AppAPIHookManager::applyToAllHook(HMODULE module, bool hook)
{
    if (module == NULL)
    {
        return;
    }

    for (APIHookMap::iterator iter = m_hookMap.begin();
        iter != m_hookMap.end();
        ++iter)
    {
        iter->second->getHooker()->hookModule(module, hook);
    }
}

void AppAPIHookManager::addHook(AppAPIHook* hook)
{
    Win32::SingleLocker locker(&m_cs);

    assert(m_hookMap.count(hook->getName()) == 0);
    m_hookMap[hook->getName()] = hook;

    applyToAllModule(hook, true);
}

void AppAPIHookManager::removeHook(AppAPIHook* hook)
{
    Win32::SingleLocker locker(&m_cs);

    applyToAllModule(hook, false);

    m_hookMap.erase(hook->getName());
}

AppAPIHookManager* AppAPIHookManager::getSingletonPtr()
{
    static AppAPIHookManager ls_mgr;
    return &ls_mgr;
}


AppAPIHook::AppAPIHook(const char *moduleName, const char* origin, void *fake):
m_name(origin)
{
    m_hooker = new APIHook(moduleName, origin, fake);

    AppAPIHookManager::getSingletonPtr()->addHook(this);
}

AppAPIHook::~AppAPIHook()
{
    AppAPIHookManager::getSingletonPtr()->removeHook(this);

    delete m_hooker;
}

const std::string& AppAPIHook::getName() const
{   
    return m_name;
}

APIHook* AppAPIHook::getHooker()
{   
    return m_hooker;
}


#pragma data_seg("Shared")
HHOOK g_hookGUI = NULL;
#pragma data_seg()
#pragma comment(linker, "/Section:Shared,rws")

static LRESULT CALLBACK guiHook_GetMsgProc(
    int code, WPARAM wParam, LPARAM lParam)
{
    return CallNextHookEx(g_hookGUI, code, wParam, lParam);
}

bool SystemAPIHook::hookGUI(bool hook, HWND hWnd)
{
    bool ret = false;

    if (hook)
    {
        if (g_hookGUI == NULL)
        {
            DWORD threadID = 0;
            if (hWnd != NULL)
            {
                threadID = GetWindowThreadProcessId(hWnd, NULL);
            }
            g_hookGUI = SetWindowsHookEx(WH_GETMESSAGE, guiHook_GetMsgProc, getThisModule(), threadID);
        }

        ret = g_hookGUI != NULL;
    }
    else
    {
        if (g_hookGUI != NULL)
        {   
            ret = UnhookWindowsHookEx(g_hookGUI) == TRUE;
            g_hookGUI = NULL;
        }
    }

    return ret;
}