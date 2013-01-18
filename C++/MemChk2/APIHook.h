#pragma once

#include <string>

// Ò²¹Ò¹³±¾Ä£¿é
#define HOOK_THIS_MODULE 0

class APIHook
{
public:
    APIHook(const char *moduleName, const char* origin, void *fake);
    ~APIHook(void);

    void hookModule(HMODULE hookDest, bool hook);

    template<typename T>
    T getOriginProc(T)  { return (T)m_origin; }
    FARPROC getOriginProc();
    FARPROC getFakeProc();

private:
    FARPROC         m_origin;
    FARPROC         m_fake;
    std::string     m_moduleName;
};

typedef void(*LoadLibraryListener)(HMODULE);
void addLoadLibraryListener(LoadLibraryListener l);
void removeLoadLibraryListener(LoadLibraryListener l);

class AppAPIHook
{
public:
    AppAPIHook(const char *moduleName, const char* origin, void *fake);
    ~AppAPIHook();

    const std::string& getName() const;
    APIHook* getHooker();

private:
    std::string      m_name;
    APIHook         *m_hooker;
};

class SystemAPIHook
{
public:
    static bool hookGUI(bool hook, HWND hWnd);
};