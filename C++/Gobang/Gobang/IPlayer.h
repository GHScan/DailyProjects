#pragma once

#include <vector>
#include <string>

#if defined(GOBANG_EXPORTS)
#define DLL_ABI __declspec(dllexport)
#else
#define DLL_ABI __declspec(dllimport)
#endif

#define UM_PCPLAYER_THINKOVER WM_USER + 11

struct IPlayer
{
    virtual ~IPlayer() = 0 {};

    virtual void init(int size, HWND wnd) = 0;
    virtual void destroy() = 0;

    virtual void reset() = 0;
    virtual void notifyPlaced(int x, int y) = 0;
    // 返回值false表示是否已经平局
    virtual bool requirePlace(int *x = NULL, int *y = NULL) = 0;
    virtual bool canPeerWin() const = 0;
    virtual bool canWin() const = 0;

    virtual const char* getName() = 0;

    virtual void* getAlgo() { return NULL; }
};

DLL_ABI IPlayer * IPlayer_create(const char* type, const char* name);
DLL_ABI void IPlayer_getAIList(std::vector<std::string> &ais);