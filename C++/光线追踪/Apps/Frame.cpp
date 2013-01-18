// vim:fileencoding=gbk

#include "pch.h"

#include <d3dx9.h>
#include <dxerr.h>

#include "VirtualPlatform.h"

#pragma warning(disable : 4996)

class DXErrorTracer
{
public:
    DXErrorTracer(const char *file, DWORD line):
        m_line(line), m_file(file), m_hr(D3D_OK)
    {
    }
    DXErrorTracer& operator = (HRESULT hr) 
    {
        m_hr = hr; 
        return *this;
    }
    ~DXErrorTracer()
    {
        if (m_hr == D3D_OK) return;
        char buf[128] = "";
        sprintf(buf, "%s(%d): \n%s\n", m_file, m_line,  DXGetErrorDescription(m_hr));
        MessageBox(NULL, buf, "d3d err", MB_OK);
    }
private:
    const char *m_file;
    DWORD m_line;
    HRESULT m_hr;
};

void checkWin32err_(const char *file, int line)
{
    DWORD err = GetLastError();
    if (err == 0) return;
    char buf[128] = "";
    FormatMessage(
            FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS, 0,
            err, LANG_SYSTEM_DEFAULT, buf, sizeof(buf), NULL);
    char buf2[256] = "";
    sprintf(buf2, "%s(%d):\n%s\n", file, line, buf);
    MessageBox(NULL, buf2, "win32 err", MB_OK);
}

#define hr DXErrorTracer(__FILE__, __LINE__)
#define checkWin32err() checkWin32err_(__FILE__, __LINE__)

float degree2radian(float degree)
{
    return degree * 3.141592f / 180;
}

void setupDX();
void onUpdateDX(float elapse);
void onDisplayDX();
void cleanupDX();
void onResize(int w, int h);
class Window
{
public:
    Window():
        m_wnd(NULL), m_w(400), m_h(300)
    {
        s_ins = this;

        WNDCLASS wc = {0};
        wc.style = CS_HREDRAW | CS_VREDRAW; 
        wc.lpfnWndProc = (WNDPROC)s_mainWndProc; 
        wc.cbClsExtra = 0; 
        wc.cbWndExtra = 0; 
        wc.hInstance = GetModuleHandle(NULL);
        wc.hIcon = LoadIcon(NULL, IDI_APPLICATION); 
        wc.hCursor = LoadCursor(NULL, IDC_ARROW); 
        wc.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH); 
        wc.lpszMenuName = "MainMenu"; 
        wc.lpszClassName = "MainWindowClass"; 
        if (!RegisterClass(&wc)) { 
            checkWin32err();
            return;
        }

        RECT rt = {0, 0, getWidth(), getHeight(),};
        AdjustWindowRect(&rt, WS_OVERLAPPEDWINDOW, 0);
        int w = rt.right - rt.left;
        int h = rt.bottom - rt.top;

        m_wnd = CreateWindow( 
                "MainWindowClass",        // name of window class 
                "DXTest",            // title-bar string 
                WS_OVERLAPPEDWINDOW, // top-level window 
                (GetSystemMetrics(SM_CXSCREEN) - w) / 2, 
                (GetSystemMetrics(SM_CYSCREEN) - h) / 2, 
                w, h,
                (HWND)NULL,         // no owner window 
                (HMENU)NULL,        // use class menu 
                GetModuleHandle(NULL),           // handle to application instance 
                (LPVOID)NULL);      // no window-creation data 
        if (m_wnd == NULL) {
            checkWin32err();
            return;
        }
    }
    ~Window()
    {
        if (m_wnd != NULL) {
            DestroyWindow(m_wnd);
            m_wnd = NULL;
        }

        UnregisterClass("MainWindowClass", GetModuleHandle(NULL));
        s_ins = NULL;
    }
    HWND getHandle() { return m_wnd; }
    int getWidth() const { return m_w; }
    int getHeight() const { return m_h; }
    void mainLoop()
    {
        setupScene();

        UpdateWindow(m_wnd);
        ShowWindow(m_wnd, SW_SHOW);

        DWORD lastTick = GetTickCount();

        MSG msg;
        for (;;) {
            if (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
                if (msg.message == WM_QUIT) {
                    break;
                }

                TranslateMessage(&msg);
                DispatchMessage(&msg);
            }
            else {
                DWORD now = GetTickCount();
                float elapse = (now - lastTick) / 1000.0f;
                lastTick = now;

                onUpdate(elapse);
                onUpdateDX(elapse);
                onDisplayDX();
                // WaitMessage();
            }
        }

        cleanupScene();
    }
private:
    static LRESULT s_mainWndProc(HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
    {
        if (s_ins != NULL && hwnd == s_ins->getHandle()) {
            return s_ins->mainWndProc(hwnd, msg, wparam, lparam);
        }
        return DefWindowProc(hwnd, msg, wparam, lparam);
    }
    LRESULT mainWndProc(HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
    {
        float mouseX = (short)(lparam & 0xffff) / float(m_w);
        float mouseY = (short)((lparam >> 16) & 0xffff) / float(m_h);
        switch (msg) {
            case WM_CLOSE:
                DestroyWindow(m_wnd);
                m_wnd = NULL;
                break;
            case WM_DESTROY:
                PostQuitMessage(0);
                break;
            case WM_SIZE:
                RECT rt;
                GetClientRect(m_wnd, &rt);
                if (rt.right >= 4 && rt.bottom >= 4) {
                    m_w = rt.right, m_h = rt.bottom;
                    onResize(m_w, m_h);
                }
                break;
            case WM_KEYUP:
                onKeyUp((int)wparam);
                if (wparam == VK_ESCAPE) {
                    PostMessage(m_wnd, WM_CLOSE, 0, 0);
                }
                break;
            case WM_KEYDOWN:
                onKeyDown((int)wparam);
                break;
            case WM_LBUTTONDOWN:
                onMouseButtonDown(MOUSE_LBUTTON, mouseX, mouseY);
                SetCapture(m_wnd);
                break;
            case WM_LBUTTONUP:
                onMouseButtonUp(MOUSE_LBUTTON, mouseX, mouseY);
                ReleaseCapture();
                break;
            case WM_RBUTTONDOWN:
                onMouseButtonDown(MOUSE_RBUTTON, mouseX, mouseY);
                SetCapture(m_wnd);
                break;
            case WM_RBUTTONUP:
                onMouseButtonUp(MOUSE_RBUTTON, mouseX, mouseY);
                ReleaseCapture();
            case WM_MOUSEMOVE:
                onMouseMove(mouseX, mouseY);
                break;
            default:
                break;
        }
        return DefWindowProc(hwnd, msg, wparam, lparam);
    }
private:
    static Window *s_ins;
    HWND m_wnd;
    int m_w, m_h;
};
Window* Window::s_ins;

IDirect3D9 *g_d3d;
IDirect3DDevice9 *g_device;
Window* g_wnd;

IDirect3DVertexBuffer9 *g_vertexBuf;
IDirect3DTexture9 *g_tex;

namespace 
{
    struct Vertex
    {
        float x, y, z;
        float u, v;
        Vertex(float _x, float _y, float _z, float _u, float _v):
            x(_x), y(_y), z(_z), u(_u), v(_v){}
        static DWORD fvf;
    };
    DWORD Vertex::fvf = D3DFVF_XYZ | D3DFVF_TEX1;
}

void destroyDevice()
{
    if (g_device != NULL) {
        g_device->Release();
        g_device = NULL;
    }
}

bool createDevice()
{
    destroyDevice();

    bool vsyn = false;
    bool usePerHUD = false;

    D3DPRESENT_PARAMETERS params = {0};
    params.BackBufferWidth = g_wnd->getWidth();
    params.BackBufferHeight = g_wnd->getHeight();
    params.BackBufferFormat = D3DFMT_A8R8G8B8;
    params.MultiSampleType = D3DMULTISAMPLE_NONE;
    params.MultiSampleQuality = 0;
    params.BackBufferCount = 1;
    params.SwapEffect = D3DSWAPEFFECT_DISCARD;
    params.hDeviceWindow = g_wnd->getHandle();
    params.Windowed = TRUE;
    params.EnableAutoDepthStencil = TRUE;
    params.AutoDepthStencilFormat = D3DFMT_D24S8;
    params.Flags = 0;
    params.FullScreen_RefreshRateInHz = 0;
    params.PresentationInterval = vsyn ? 
        D3DPRESENT_INTERVAL_DEFAULT : D3DPRESENT_INTERVAL_IMMEDIATE;

    hr = g_d3d->CreateDevice(
            usePerHUD ? 1 : 0, 
            usePerHUD ? D3DDEVTYPE_REF : D3DDEVTYPE_HAL, 
            g_wnd->getHandle(), 
            D3DCREATE_PUREDEVICE | D3DCREATE_HARDWARE_VERTEXPROCESSING, 
            &params, &g_device);

    return g_device != NULL;
}

void onResize(int w, int h)
{
    cleanupDX();
    destroyDevice();

    if (createDevice())
    {
        setupDX();
    }
}
void setupDX()
{
    if (g_device == NULL) return;

    D3DVIEWPORT9 viewport = {0, 0, g_wnd->getWidth(), g_wnd->getHeight(), 0, 1};
    hr = g_device->SetViewport(&viewport);

    D3DXMATRIX projMat;
    D3DXMatrixOrthoLH(&projMat, 2, 2, 1, 2);
    hr = g_device->SetTransform(D3DTS_PROJECTION, &projMat);

    hr = g_device->CreateVertexBuffer(
            sizeof(Vertex) * 4, 
            D3DUSAGE_WRITEONLY, 
            Vertex::fvf, 
            D3DPOOL_MANAGED, 
            &g_vertexBuf, NULL);
    if (g_vertexBuf != NULL) {
        Vertex *pv = NULL;
        hr = g_vertexBuf->Lock(0, 0, (void**)&pv, 0);
        float z = 1.5;
        float offX = -1.0f / g_wnd->getWidth();
        float offY = -1.0f / g_wnd->getHeight();
        *pv++ = Vertex(-1 + offX, +1 + offY, z, 0, 0);
        *pv++ = Vertex(+1 + offX, +1 + offY, z, 1, 0);
        *pv++ = Vertex(-1 + offX, -1 + offY, z, 0, 1);
        *pv++ = Vertex(+1 + offX, -1 + offY, z, 1, 1);
        hr = g_vertexBuf->Unlock();
    }

    hr = g_device->CreateTexture(
            g_wnd->getWidth(), g_wnd->getHeight(), 1, 
            D3DUSAGE_DYNAMIC, D3DFMT_A8R8G8B8, D3DPOOL_DEFAULT,
            &g_tex, NULL);
    hr = g_device->SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_SELECTARG1);
}

void onUpdateDX(float elapse)
{
    if (g_device == NULL) return;
    if (g_tex != NULL) {
        D3DLOCKED_RECT lockedRt = {0};
        hr = g_tex->LockRect(0, &lockedRt, NULL, D3DLOCK_DISCARD);
        onDrawBuffer((char*)lockedRt.pBits, 
                g_wnd->getWidth(), g_wnd->getHeight(), 
                lockedRt.Pitch);
        hr = g_tex->UnlockRect(0);
    }
}

void onDisplayDX()
{
    if (g_device == NULL)  return;

    hr = g_device->Clear(0, NULL, D3DCLEAR_TARGET | D3DCLEAR_ZBUFFER, 0, 1, 0);
    hr = g_device->BeginScene();

    hr = g_device->SetTexture(0, g_tex);
    hr = g_device->SetStreamSource(0, g_vertexBuf, 0, sizeof(Vertex));
    hr = g_device->SetFVF(Vertex::fvf);
    hr = g_device->DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);

    hr = g_device->EndScene();
    hr = g_device->Present(0, 0, 0, 0);
}

void cleanupDX()
{
    if (g_vertexBuf != NULL) {
        g_vertexBuf->Release();
        g_vertexBuf = NULL;
    }

    if (g_tex != NULL) {
        g_tex->Release();
        g_tex = NULL;
    }
}

int main()
{
    g_d3d = Direct3DCreate9(D3D_SDK_VERSION);
    if (g_d3d == NULL) {
        cout << "create d3d failed!" << endl;
        return 1;
    }

    Window wnd;
    g_wnd = &wnd;    

    wnd.mainLoop();

    cleanupDX();
    destroyDevice();

    g_wnd = NULL;
    g_d3d->Release();
}
