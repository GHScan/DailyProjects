// vim: fileencoding=gbk
#include "pch.h"

#include <ctime>

#include <fstream>

#include "SceneManager.h"
#include "Util.h"
#include "Log.h"
#include "VirtualPlatform.h"
#include "Vector.h"
#include "Geometry.h"
#include "Renderer.h"

#pragma warning(disable: 4996) // 'sscanf' was declared deprecated

SceneManager* g_sceneMgr;
Renderer* g_renderer;
bool g_outputLog;
bool g_printScreen;
clock_t g_lastTick;

int g_callCnt_rayIntersectPlane;
int g_callCnt_pointInTriangleCheck;
int g_callCnt_interpolate;

void switchSetting(int act)
{
    static bool s_enableSkybox = false;
    static bool s_enableSpec = false;
    static int s_maxTraceDepth = 0;
    static int s_shadowLevel = 0;
    static int s_aoLevel = 0;
    switch (act) {
        case 0:
            g_renderer->enableSpecular(s_enableSpec = !s_enableSpec);
            break;
        case 1:
            g_renderer->enableSkybox(s_enableSkybox = !s_enableSkybox);
            break;
        case 2:
            g_renderer->setMaxTraceDepth(
                    s_maxTraceDepth = (s_maxTraceDepth + 1) % 5);
            break;
        case 3:
            g_renderer->setShadowLevel(
                    s_shadowLevel = (s_shadowLevel + 1) % 2);
            break;
        case 4:
            g_renderer->setAOLevel(
                    s_aoLevel = (s_aoLevel + 1) % 5);
            break;
        default:
            break;
    }
}

void setupScene()
{
    g_sceneMgr = new SceneManager();
    g_renderer = new Renderer();

    {
        std::ifstream fi("Scene.txt");
        g_renderer->scanStream(fi);
        bool b = g_sceneMgr->load(fi);
        assert(b);
    }

    g_outputLog = false;
    g_printScreen = false;

    g_lastTick = clock();
}

void onDrawBuffer(char *buf, int w, int h, int pitch)
{
    {
        char *p = buf;
        for (int i = 0; i < h; ++i) {
            memset(p, 0, w * 4);
            p += pitch;
        }
    }

    g_callCnt_rayIntersectPlane = 0;
    g_callCnt_pointInTriangleCheck = 0;
    g_callCnt_interpolate = 0;

    g_renderer->render(g_sceneMgr, buf, w, h, pitch);

    Log::instance()->addMsg(
            "rayIntersectPlane : %f M\n"
            "pointInTriangleCheck : %f M\n"
            "interpolate: %f M\n", 
            g_callCnt_rayIntersectPlane / 1000000.f, 
            g_callCnt_pointInTriangleCheck / 1000000.f,
            g_callCnt_interpolate / 1000000.f);

    if (g_outputLog) {
        g_outputLog = false;
        Log::instance()->flushMsgToConsole();
    }
    else Log::instance()->clearMsg();

    if (g_printScreen) {
        g_printScreen = false;
        saveImage("Scan.png", buf, w, h, pitch);
    }

    {
        clock_t c = clock();
        float t = (c - g_lastTick) / 1000.f;
        if (t > 1) {
            cout << "render a frame time : " << t << endl;
        }
        g_lastTick = c;
    }
}

void onUpdate(float elapse)
{
    if(g_sceneMgr != NULL) g_sceneMgr->onUpdate(elapse);
}
void onMouseButtonDown(int btn, float x, float y)
{
    if(g_sceneMgr != NULL) g_sceneMgr->onMouseButtonDown(btn, x, y);
}
void onMouseButtonUp(int btn, float x, float y)
{
    if(g_sceneMgr != NULL) g_sceneMgr->onMouseButtonUp(btn, x, y);
}
void onMouseMove(float x, float y) 
{
    if(g_sceneMgr != NULL) g_sceneMgr->onMouseMove(x, y);
}
void onKeyDown(int k)
{
    if(g_sceneMgr != NULL) g_sceneMgr->onKeyDown(k);
}

void onKeyUp(int k)
{
    if(g_sceneMgr != NULL) g_sceneMgr->onKeyUp(k);
    if (k == 'O') g_outputLog = true;
    if (k == 'P') g_printScreen = true;
    if (k >= '1' && k <= '9') switchSetting(k - '1');
    if (k == 'C') {
        std::ofstream fo(getNotExistFileName("SceneCapture.txt").c_str());
        g_renderer->printStream(fo);
        g_sceneMgr->save(fo);
    }
}

void cleanupScene()
{
    sdelete(g_sceneMgr);
    sdelete(g_renderer);
}

