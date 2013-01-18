// vim:fileencoding=gbk

#include "pch.h"

#include <d3dx9.h>
#include <dxerr.h>

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

void setup();
void onUpdate(float elapse);
void onDisplay();
void cleanup();
class Window
{
public:
    Window():
        m_wnd(NULL)
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

        m_wnd = CreateWindow( 
                "MainWindowClass",        // name of window class 
                "DXTest",            // title-bar string 
                WS_OVERLAPPEDWINDOW, // top-level window 
                CW_USEDEFAULT,       // default horizontal position 
                CW_USEDEFAULT,       // default vertical position 
                rt.right - rt.left, 
                rt.bottom - rt.top,
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
    int getWidth() const { return 400; }
    int getHeight() const { return 300; }
    void mainLoop()
    {
        setup();

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
                onDisplay();
                // WaitMessage();
            }
        }

        cleanup();
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
        switch (msg) {
            case WM_CLOSE:
                DestroyWindow(m_wnd);
                m_wnd = NULL;
                break;
            case WM_DESTROY:
                PostQuitMessage(0);
                break;
            case WM_KEYUP:
                if (wparam == VK_ESCAPE) {
                    PostMessage(m_wnd, WM_CLOSE, 0, 0);
                }
                break;
        }
        return DefWindowProc(hwnd, msg, wparam, lparam);
    }
private:
    static Window *s_ins;
    HWND m_wnd;
};
Window* Window::s_ins = NULL;

IDirect3D9 *g_d3d = NULL;
IDirect3DDevice9 *g_device = NULL;
Window* g_wnd = NULL;

ID3DXMesh *g_mesh = NULL;

void setup()
{
    D3DVIEWPORT9 viewport = {0, 0, g_wnd->getWidth(), g_wnd->getHeight(), 0, 1};
    hr = g_device->SetViewport(&viewport);

    D3DXMATRIX projMat;
    D3DXMatrixPerspectiveFovLH(&projMat, degree2radian(60), 
            g_wnd->getWidth() / (float)g_wnd->getHeight(), 0.1f, 50);
    hr = g_device->SetTransform(D3DTS_PROJECTION, &projMat);

    hr = g_device->LightEnable(0, TRUE);
    D3DLIGHT9 light;
    memset(&light, 0, sizeof(light));
    light.Type = D3DLIGHT_DIRECTIONAL;
    light.Direction = D3DXVECTOR3(-1, -1, 0);
    light.Diffuse = D3DXCOLOR(1, 1, 1, 1);
    light.Specular = D3DXCOLOR(1, 1, 1, 1);
    hr = g_device->SetLight(0, &light);
    hr = g_device->SetRenderState(D3DRS_SPECULARENABLE, TRUE);

    hr = D3DXCreateTeapot(g_device, &g_mesh, NULL);
}

D3DXMATRIX g_rotMat;
void onUpdate(float elapse)
{
    static float ls_f = 0;
    ls_f += elapse;
    D3DXMatrixIdentity(&g_rotMat);
    D3DXMATRIX mat;
    D3DXMatrixRotationX(&mat, sin(ls_f));
    g_rotMat *= mat;
    D3DXMatrixRotationY(&mat, cos(ls_f));
    g_rotMat *= mat;
    D3DXMatrixRotationZ(&mat, sin(ls_f));
    g_rotMat *= mat;
}

void onDisplay()
{
    hr = g_device->Clear(0, NULL, D3DCLEAR_TARGET | D3DCLEAR_ZBUFFER, D3DCOLOR_ARGB
            (255, 45, 50, 170), 1, 0);
    hr = g_device->BeginScene();

    D3DXMATRIX worldMat;
    D3DXMatrixTranslation(&worldMat, 0, 0, 3);
    worldMat = g_rotMat * worldMat;
    hr = g_device->SetTransform(D3DTS_WORLDMATRIX(0), &worldMat);

    D3DMATERIAL9 material = {0};
    material.Diffuse = D3DXCOLOR(1, 1, 1, 1);
    material.Specular = D3DXCOLOR(1, 1, 1, 1);
    material.Power = 16;
    hr = g_device->SetMaterial(&material);
    hr = g_mesh->DrawSubset(0);

    hr = g_device->EndScene();
    hr = g_device->Present(0, 0, 0, 0);
}

void cleanup()
{
}

int main()
{
    GetProcessId(NULL);

    bool vsyn = true;
    bool usePerHUD = false;

    g_d3d = Direct3DCreate9(D3D_SDK_VERSION);
    if (g_d3d == NULL) {
        cout << "create d3d failed!" << endl;
        return 1;
    }

    Window wnd;
    g_wnd = &wnd;

    {
        D3DPRESENT_PARAMETERS params = {0};
        params.BackBufferWidth = wnd.getWidth();
        params.BackBufferHeight = wnd.getHeight();
        params.BackBufferFormat = D3DFMT_A8R8G8B8;
        params.MultiSampleType = D3DMULTISAMPLE_NONE;
        params.MultiSampleQuality = 0;
        params.BackBufferCount = 1;
        params.SwapEffect = D3DSWAPEFFECT_DISCARD;
        params.hDeviceWindow = wnd.getHandle();
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
                wnd.getHandle(), 
                D3DCREATE_PUREDEVICE | D3DCREATE_HARDWARE_VERTEXPROCESSING, 
                &params, &g_device);
        if (g_device == NULL) {
            g_d3d->Release();
            return 2;
        }
    }

    wnd.mainLoop();

    g_wnd = NULL;
    g_device->Release();
    g_d3d->Release();
}
