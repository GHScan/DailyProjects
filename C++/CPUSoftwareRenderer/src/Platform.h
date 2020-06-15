#ifndef PLATFORM_H
#define PLATFORM_H


#include <functional>

#include <SDL2/SDL.h>

#include "Utils.h"


#if _WIN32
#include <Windows.h>
#undef min
#undef max
#endif


inline void SaveBMP(
    std::string const &fname,
    int w, int h, uint32_t const *fbuf)
{
    SDL_Surface * surface = SDL_CreateRGBSurfaceFrom(
        (void*)fbuf, w, h, 32, w * 4, 
        0xff << 24, 0xff << 16, 0xff << 8, 0xff << 0);

    SDL_SaveBMP(surface, fname.c_str());
    SDL_FreeSurface(surface);
}


#if _WIN32
inline bool CreateConsole()
{
    if (!AllocConsole())
        return false;

    FILE* fDummy;
    freopen_s(&fDummy, "CONOUT$", "w", stdout);
    freopen_s(&fDummy, "CONOUT$", "w", stderr);
    freopen_s(&fDummy, "CONIN$", "r", stdin);
    std::cout.clear();
    std::clog.clear();
    std::cerr.clear();
    std::cin.clear();

    return true;
}
#endif

static bool MainLoop(
    int w, int h,
    std::function<void(uint32_t *fBuf, float elapse)> onDraw)
{
    if (SDL_Init(SDL_INIT_EVERYTHING))
        return false;
    ON_SCOPE_EXIT([]() { SDL_Quit(); });


    SDL_Window *window = SDL_CreateWindow(
        "MainLoop",
        SDL_WINDOWPOS_CENTERED,
        SDL_WINDOWPOS_CENTERED,
        w,
        h,
        SDL_WINDOW_SHOWN
    );
    if (window == nullptr)
        return false;
    ON_SCOPE_EXIT([=]() { SDL_DestroyWindow(window); });


    SDL_Renderer *renderer = SDL_CreateRenderer(
        window,
        -1,
        SDL_RENDERER_ACCELERATED
    );
    if (renderer == nullptr)
        return false;
    ON_SCOPE_EXIT([=]() { SDL_DestroyRenderer(renderer); });


    SDL_Texture *texture = SDL_CreateTexture(
        renderer, 
        SDL_PIXELFORMAT_RGBA8888,
        SDL_TEXTUREACCESS_STREAMING,
        w, h);
    if (texture == nullptr)
        return false;
    ON_SCOPE_EXIT([=]() { SDL_DestroyTexture(texture); });


    std::vector<uint32_t> fBuf(h * w);
    auto lastTime = std::chrono::steady_clock::now();

    for (;;)
    {
        SDL_Event ev;
        while (SDL_PollEvent(&ev) != 0)
        {
            if (ev.type == SDL_QUIT)
                return true;
        }

        auto currTime = std::chrono::steady_clock::now();
        auto elapse = std::chrono::duration<float>(currTime - lastTime).count();
        lastTime = currTime;

        onDraw(fBuf.data(), elapse);

        SDL_UpdateTexture(texture, nullptr, fBuf.data(), w * 4);

        SDL_RenderCopy(renderer, texture, nullptr, nullptr);
        SDL_RenderPresent(renderer);
    }

    return true;
}


#endif
