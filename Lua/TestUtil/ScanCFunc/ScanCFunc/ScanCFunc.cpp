// ScanCFunc.cpp : 定义 DLL 应用程序的入口点。
//

#include "stdafx.h"

#ifdef _MANAGED
#pragma managed(push, off)
#endif

BOOL APIENTRY DllMain( HMODULE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved
					 )
{
    return TRUE;
}

#ifdef _MANAGED
#pragma managed(pop)
#endif


#define SCAN_CFUNC_ABI  extern "C" __declspec(dllexport)


#include <string>

#include <windows.h>

#include <lua.hpp>

#include <luabind/luabind.hpp>
#include <luabind/function.hpp>

extern const std::string utf8ToAnsi(const char *);
extern const std::string ansiToUtf8(const char *s);

SCAN_CFUNC_ABI int luaopen_scan_cfunc(lua_State *l)
{
    using namespace luabind;

    open(l);

    module(l, "scan_cfunc")
    [
        def("ansi_to_utf8", ansiToUtf8),
        def("utf8_to_ansi", utf8ToAnsi)
    ];

    return 1;
}

enum TextEncodingType
{
    TET_Ansi = CP_ACP,
    TET_Utf8 = CP_UTF8,
};

static const std::string wc2mb(const wchar_t *p, TextEncodingType encodingT)
{
    assert(p != NULL && p[0] != 0);

    int requireChar = WideCharToMultiByte((UINT)encodingT, 0, p, -1, NULL, 0, NULL, NULL);
    assert(requireChar > 0);

    std::string s(requireChar, 0);
    WideCharToMultiByte((UINT)encodingT, 0, p, -1, (char*)s.c_str(), (int)s.size(), NULL, NULL);
    s.resize(s.size() - 1);

    return s;
}

static const std::wstring mb2wc(const char *p, TextEncodingType encodingT)
{
    assert(p != NULL && p[0] != 0);

    int requireChar = MultiByteToWideChar((UINT)encodingT, 0, p, -1, NULL, 0);
    assert(requireChar > 0);

    std::wstring s(requireChar, 0);
    MultiByteToWideChar((UINT)encodingT, 0, p, -1, (wchar_t*)s.c_str(), (int)s.size());
    s.resize(s.size() - 1);

    return s;
}

static const std::string ansiToUtf8(const char *s)
{
    return wc2mb(mb2wc(s, TET_Ansi).c_str(), TET_Utf8);
}

static const std::string utf8ToAnsi(const char *s)
{
    return wc2mb(mb2wc(s, TET_Utf8).c_str(), TET_Ansi);
}