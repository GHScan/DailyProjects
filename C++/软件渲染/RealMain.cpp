// vim: fileencoding=gbk
#include "pch.h"

#include "SceneManager.h"
#include "Renderer.h"
#include "RenderStateEnums.h"
#include "Util.h"
#include "Log.h"
#include "VirtualPlatform.h"
#include "Vector.h"
#include "Geometry.h"

#pragma warning(disable: 4996) // 'sscanf' was declared deprecated

SceneManager* g_sceneMgr;
Renderer* g_renderer;
bool g_outputLog;
bool g_printScreen;
int g_renderState[7];

bool saveImage(const char *fname, const char *buf, int w, int h, int pitch);

void switchRenderState(int i)
{
    switch (i) {
        case 0:
            {
                E_ShadeMode states[] = { 
                    ESM_frame, ESM_const, ESM_flat, ESM_gouraud, ESM_phong, 
                };
                g_renderState[i] = (g_renderState[i] + 1) % arraySize(states);
                g_renderer->setShadeMode(states[g_renderState[i]]);
            }
            break;
        case 1:
            {
                E_TextureFilterType states[] = { 
                    ETFT_null, ETFT_point, ETFT_bilinear, ETFT_trilinear, ETFT_mipmapDbg,
                };
                g_renderState[i] = (g_renderState[i] + 1) % arraySize(states);
                g_renderer->setTextureFilterType(states[g_renderState[i]]);
            }
            break;
        case 2:
            {
                E_CullFace states[] = { 
                    ECF_null, ECF_back, ECF_front,
                };
                g_renderState[i] = (g_renderState[i] + 1) % arraySize(states);
                g_renderer->setCullFace(states[g_renderState[i]]);
            }
            break;
        case 3:
            {
                E_ZBufferType states[] = { 
                    EZBT_null, EZBT_zbuf, EZBT_1zbuf,
                };
                g_renderState[i] = (g_renderState[i] + 1) % arraySize(states);
                g_renderer->setZbufferType(states[g_renderState[i]]);
            }
            break;
        case 4:
            {
                E_ZSortType states[] = { 
                    EZST_null, EZST_near2Far, EZST_far2Near,
                };
                g_renderState[i] = (g_renderState[i] + 1) % arraySize(states);
                g_renderer->setZSortType(states[g_renderState[i]]);
            }
            break;
        case 5:
            {
                E_SpecularLight states[] = { 
                    ESL_disable, ESL_enbale,
                };
                g_renderState[i] = (g_renderState[i] + 1) % arraySize(states);
                g_renderer->setSpecularLight(states[g_renderState[i]]);
            }
            break;
        case 6:
            {
                E_TextureAddressingMode states[] = { 
                    ETAM_clamp, ETAM_repeat, ETAM_mirror
                };
                g_renderState[i] = (g_renderState[i] + 1) % arraySize(states);
                g_renderer->setTextureAddressingMode(states[g_renderState[i]]);
            }
            break;
        default: break;
    }
}

void setupScene()
{
    g_sceneMgr = new SceneManager("scene.txt");

    g_renderer = new Renderer(g_sceneMgr);

    g_outputLog = false;
    g_printScreen = false;
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

    if(g_renderer != NULL) g_renderer->render(buf, w, h, pitch);

    if (g_outputLog) {
        g_outputLog = false;
        Log::instance()->flushMsgToConsole();
    }
    else Log::instance()->clearMsg();

    if (g_printScreen) {
        g_printScreen = false;
        saveImage("Scan.png", buf, w, h, pitch);
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

static void setClipPlane()
{
    puts("Input plane params -a, b, c, d: (a * x + b * y + c * z = d)");
    char buf[128] = "";
    cin.getline(buf, sizeof(buf));
    if (buf[0] == 0) g_renderer->clearClipPlane();
    else {
        float a, b, c, d;
        if (sscanf(buf, "%f %f %f %f", &a, &b, &c, &d) == 4) {
            g_renderer->addClipPlane(Plane(Vector3(a, b, c).normalize(), d));
        }
        else puts("Wrong params number.");
    }
}

void onKeyUp(int k)
{
    if(g_sceneMgr != NULL) g_sceneMgr->onKeyUp(k);
    if (k == 'O') g_outputLog = true;
    if (k == 'P') g_printScreen = true;
    if (k >= '1' && k <= '7') switchRenderState(k - '1');
    if (k == 'C') setClipPlane();
}

void cleanupScene()
{
    sdelete(g_renderer);
    sdelete(g_sceneMgr);
}

