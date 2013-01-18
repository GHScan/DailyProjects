#include "pch.h"

#include <cassert>
#include <cstdio>

#include <d3dx9.h>
#include <dxerr.h>

#include "Mesh.h"
#include "Matrix.h"
#include "Vector.h"

#pragma warning(disable: 4996) // 'sscanf' was declared deprecated

HRESULT hr = D3D_OK;
#define checkD3DError()  if (hr != D3D_OK) { DXTrace(__FILE__, __LINE__, hr, "...", TRUE); exit(1); } else {}

Matrix4x4 readMatrix()
{
    char buf[256] = "";
    puts("Use identity matrix?(y/n)");
    cin.getline(buf, sizeof(buf));
    if (buf[0] != 'n') return Matrix4x4::IDENTITY;

    Vector3 scale;
    Vector3 rotate;
    Vector3 translate;

    puts("Scale (x, y, z):\n");
    cin.getline(buf, sizeof(buf));
    if (sscanf(buf, "%f %f %f", &scale.x, &scale.y, &scale.z) != 3) {
        scale = Vector3(1);
    }
    cout << "Scale : " << scale << endl;

    puts("Rotate (in degrees) (yaw, pitch, roll):\n");
    cin.getline(buf, sizeof(buf));
    if (sscanf(buf, "%f %f %f", &rotate.x, &rotate.y, &rotate.z) != 3) {
        rotate = Vector3(0.f);
    }
    cout << "Rotate : " << rotate << endl;

    puts("Translate (x, y, z):\n");
    cin.getline(buf, sizeof(buf));
    if (sscanf(buf, "%f %f %f", &translate.x, &translate.y, &translate.z) != 3) {
        translate = Vector3(0.f);
    }
    cout << "Translate : " << translate << endl;

    return 
        Matrix4x4::fromScale(scale.x, scale.y, scale.z) *
        Matrix4x4::fromYawPitchRoll(rotate.x, rotate.y, rotate.z) * 
        Matrix4x4::fromTranslate(translate.x, translate.y, translate.z);
}

HWND createWindow()
{
    HWND wnd = NULL;

    WNDCLASS wc = {0};
    wc.style = CS_HREDRAW | CS_VREDRAW; 
    wc.lpfnWndProc = DefWindowProc;
    wc.cbClsExtra = 0; 
    wc.cbWndExtra = 0; 
    wc.hInstance = GetModuleHandle(NULL);
    wc.hIcon = LoadIcon(NULL, IDI_APPLICATION); 
    wc.hCursor = LoadCursor(NULL, IDC_ARROW); 
    wc.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH); 
    wc.lpszMenuName = "MainMenu"; 
    wc.lpszClassName = "MainWindowClass"; 
    if (!RegisterClass(&wc)) { 
        assert(0);
        return wnd;
    }

    RECT rt = {0, 0, 400, 300,};
    AdjustWindowRect(&rt, WS_OVERLAPPEDWINDOW, 0);

    wnd = CreateWindow( 
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
    if (wnd == NULL) assert(0);
    return wnd;
}
void destroyWindow(HWND wnd)
{
    if (wnd != NULL) {
        DestroyWindow(wnd);
    }
    UnregisterClass("MainWindowClass", GetModuleHandle(NULL));
}

class D3DEnv
{
public:
    D3DEnv(HWND wnd);
    ~D3DEnv();

    IDirect3DDevice9* getDevice() { return m_device; }

private:
    IDirect3DDevice9 *m_device;
    IDirect3D9    *m_d3d;
};
D3DEnv::D3DEnv(HWND wnd):
    m_d3d(NULL), m_device(NULL)
{
    m_d3d = Direct3DCreate9(D3D_SDK_VERSION);
    if (m_d3d == NULL) {
        cout << "create d3d failed!" << endl;
        assert(0);
    }

    bool vsyn = true;
    bool usePerHUD = false;

    D3DPRESENT_PARAMETERS params = {0};
    params.BackBufferWidth = 400;
    params.BackBufferHeight = 300;
    params.BackBufferFormat = D3DFMT_A8R8G8B8;
    params.MultiSampleType = D3DMULTISAMPLE_NONE;
    params.MultiSampleQuality = 0;
    params.BackBufferCount = 1;
    params.SwapEffect = D3DSWAPEFFECT_DISCARD;
    params.hDeviceWindow = wnd;
    params.Windowed = TRUE;
    params.EnableAutoDepthStencil = TRUE;
    params.AutoDepthStencilFormat = D3DFMT_D24S8;
    params.Flags = 0;
    params.FullScreen_RefreshRateInHz = 0;
    params.PresentationInterval = vsyn ? 
        D3DPRESENT_INTERVAL_DEFAULT : D3DPRESENT_INTERVAL_IMMEDIATE;

    hr = m_d3d->CreateDevice(
            usePerHUD ? 1 : 0, 
            usePerHUD ? D3DDEVTYPE_REF : D3DDEVTYPE_HAL, 
            wnd,
            D3DCREATE_PUREDEVICE | D3DCREATE_HARDWARE_VERTEXPROCESSING, 
            &params, &m_device);
    checkD3DError();
}
D3DEnv::~D3DEnv()
{
    if (m_device != NULL) m_device->Release();
    if (m_d3d != NULL) m_d3d->Release();
}

void saveMesh(const char *fname, ID3DXMesh *mesh, D3DXMATERIAL *mats, int matNum)
{
    assert(mesh != NULL);
    DWORD subCnt = 0;
    hr = mesh->GetAttributeTable(NULL, &subCnt);
    checkD3DError();
    D3DXATTRIBUTERANGE *subAttrs = new D3DXATTRIBUTERANGE[subCnt];
    hr = mesh->GetAttributeTable(subAttrs, &subCnt);
    checkD3DError();

    D3DVERTEXELEMENT9 vertexDeclare[MAX_FVF_DECL_SIZE];
    hr = mesh->GetDeclaration(vertexDeclare);
    checkD3DError();
    int elementOffset[EVEI_COUNT] = {0};
    int vt = 0;
    for (int i = 0; i < MAX_FVF_DECL_SIZE; ++i) {
        if (vertexDeclare[i].Stream == 0xff) break;
        if (vertexDeclare[i].UsageIndex > 0) continue;
        if (vertexDeclare[i].Usage == D3DDECLUSAGE_POSITION &&
                vertexDeclare[i].Type == D3DDECLTYPE_FLOAT3) {
            elementOffset[EVEI_position] = vertexDeclare[i].Offset;
            vt |= EVET_position;
        }
        else if (vertexDeclare[i].Usage == D3DDECLUSAGE_NORMAL &&
                vertexDeclare[i].Type == D3DDECLTYPE_FLOAT3) {
            elementOffset[EVEI_normal] = vertexDeclare[i].Offset;
            vt |= EVET_normal;
        }
        else if (vertexDeclare[i].Usage == D3DDECLUSAGE_TEXCOORD &&
                vertexDeclare[i].Type == D3DDECLTYPE_FLOAT2) {
            elementOffset[EVEI_texCoord] = vertexDeclare[i].Offset;
            vt |= EVET_texCoord;
        }
        else if (vertexDeclare[i].Usage == D3DDECLUSAGE_COLOR &&
                vertexDeclare[i].Type == D3DDECLTYPE_FLOAT3) {
            elementOffset[EVEI_color] = vertexDeclare[i].Offset;
            vt |= EVET_color;
        }
        else {}
    }
    assert(vt & EVET_position);

    char* vertexPtr = NULL;
    hr = mesh->LockVertexBuffer(D3DLOCK_READONLY, (void**)&vertexPtr);
    checkD3DError();

    char* indexPtr = NULL;
    hr = mesh->LockIndexBuffer(D3DLOCK_READONLY, (void**)&indexPtr);
    checkD3DError();

    Mesh m;

    const DWORD vertexSize = mesh->GetNumBytesPerVertex();
    for (int i = 0; i < (int)subCnt; ++i) {
        SubMesh *sub = new SubMesh();

        sub->vertexBuffer.setVertexType(vt);
        char* vtxPtr = vertexPtr + subAttrs[i].VertexStart * vertexSize;
        for (int vidx = 0; vidx < (int)subAttrs[i].VertexCount; ++vidx) {
            if (vt & EVET_position) {
                sub->vertexBuffer.addElement<EVEI_position>(*(Vector3*)(vtxPtr + elementOffset[EVEI_position]));
            }
            if (vt & EVET_normal) {
                sub->vertexBuffer.addElement<EVEI_normal>(*(Vector3*)(vtxPtr + elementOffset[EVEI_normal]));
            }
            if (vt & EVET_texCoord) {
                sub->vertexBuffer.addElement<EVEI_texCoord>(*(Vector2*)(vtxPtr + elementOffset[EVEI_texCoord]));
            }
            if (vt & EVET_color) {
                sub->vertexBuffer.addElement<EVEI_color>(*(Vector3*)(vtxPtr + elementOffset[EVEI_color]));
            }
            vtxPtr += vertexSize;
        }

        int* idxPtr = (int*)indexPtr + subAttrs[i].FaceStart * 3;
        int off = -(int)subAttrs[i].VertexStart;
        for (int tidx = 0; tidx < (int)subAttrs[i].FaceCount; ++tidx) {
            IndexTriangle tri(idxPtr[0] + off, idxPtr[1] + off, idxPtr[2] + off);
            sub->indexBuffer.addTriangle(tri);
            idxPtr += 3;
        }

        if (mats != NULL && i < matNum) {
            const D3DXMATERIAL *mat = mats + i;
            sub->mat.texture = mat->pTextureFilename == NULL ? "" : mat->pTextureFilename;
            sub->mat.ambientClr = Vector3(&mat->MatD3D.Ambient.r);
            sub->mat.diffuseClr = Vector3(&mat->MatD3D.Diffuse.r);
            sub->mat.specularClr = Vector3(&mat->MatD3D.Specular.r);
            sub->mat.emissiveClr = Vector3(&mat->MatD3D.Emissive.r);
            sub->mat.power = mat->MatD3D.Power;
        }
        sub->genVertexNormals();
        if (!(sub->vertexBuffer.getVertexType() & EVET_texCoord))
        {
            puts("Input texcoord matrix:");
            sub->genTexcoords(readMatrix());
        }
        m.addSubMesh(sub);
    }

    if (m.getSubCount() > 0 && m.sub(0)->indexBuffer.getTriangleCount() > 0) {
        m.save(fname);
    }

    mesh->UnlockIndexBuffer();
    mesh->UnlockVertexBuffer();

    delete[] subAttrs;
}

void printHelp()
{
        puts(
                "1. help:\n"
                "2. create:\n"
                "       box     - width(1), height(1), depth(1)\n"
                "       teapot  - \n"
                "       torus   - innerRadius(1), outerRadius(2), sides(20), rings(10)\n"
                "       cylinder- radius1(1), radius2(1), length(2), slices(10), stacks(20)\n"
                "       sphere  - radius(1), slices(10), stacks(10)\n"
                "3. load:\n"
                "       xFileName\n");
}

int main(int argc, char *argv[])
{
    if (argc == 1) {
        printHelp();
        return 0;
    }

    HWND wnd = createWindow();
    D3DEnv *d3d = new D3DEnv(wnd);

    ID3DXMesh *mesh = NULL;
    ID3DXBuffer *adjacency = NULL, *mats = NULL;
    DWORD matNum = 0;

    assert(argc >= 2);
    if (strcmp(argv[1], "create") == 0) {
        assert(argc >= 3);
        if (strcmp(argv[2], "box") == 0) {
            float w = 1, h = 1, d = 1;
            if (argc >= 6) {
                int succ = 0;
                succ += sscanf(argv[3], "%f", &w);
                succ += sscanf(argv[4], "%f", &h);
                succ += sscanf(argv[5], "%f", &d);
                assert(succ == 3);
            }
            hr = D3DXCreateBox(d3d->getDevice(), 
                    w, h, d, 
                    &mesh, &adjacency);
        }
        else if (strcmp(argv[2], "teapot") == 0) {
            hr = D3DXCreateTeapot(d3d->getDevice(), &mesh, &adjacency);
        }
        else if (strcmp(argv[2], "torus") == 0) {
            float innerRadius = 1, outerRadius = 2;
            int sides = 20, rings = 10;
            if (argc >= 7) {
                int succ = 0;
                succ += sscanf(argv[3], "%f", &innerRadius);
                succ += sscanf(argv[4], "%f", &outerRadius);
                succ += sscanf(argv[5], "%d", &sides);
                succ += sscanf(argv[6], "%d", &rings);
                assert(succ == 4);
            }
            hr = D3DXCreateTorus(
                    d3d->getDevice(), 
                    innerRadius, outerRadius, 
                    sides, rings, 
                    &mesh, &adjacency);
        }
        else if (strcmp(argv[2], "cylinder") == 0) {
            float radius1 = 1, radius2 = 1, length = 2;
            int slices = 10, stacks = 20;
            if (argc >= 8) {
                int succ = 0;
                succ += sscanf(argv[3], "%f", &radius1);
                succ += sscanf(argv[4], "%f", &radius2);
                succ += sscanf(argv[5], "%f", &length);
                succ += sscanf(argv[6], "%d", &slices);
                succ += sscanf(argv[7], "%d", &stacks);
                assert(succ == 5);
            }
            hr = D3DXCreateCylinder(d3d->getDevice(),
                    radius1, radius2, length, 
                    slices, stacks,
                    &mesh, &adjacency);
        }
        else if (strcmp(argv[2], "sphere") == 0) {
            float radius = 1;
            int slices = 10, stacks = 10;
            if (argc >= 6) {
                int succ = 0;
                succ += sscanf(argv[3], "%f", &radius);
                succ += sscanf(argv[4], "%d", &slices);
                succ += sscanf(argv[5], "%d", &stacks);
                assert(succ == 3);
            }
            hr = D3DXCreateSphere(d3d->getDevice(),
                    radius, 
                    slices, stacks,
                    &mesh, &adjacency);
        }
        else assert(0);
    }
    else if (strcmp(argv[1], "load") == 0) {
        assert(argc >= 3);
        hr = D3DXLoadMeshFromX(
                argv[2], 
                D3DXMESH_32BIT | D3DXMESH_SYSTEMMEM, 
                d3d->getDevice(), 
                &adjacency, &mats, NULL, &matNum, &mesh);
    }
    else if (strcmp(argv[1], "help") == 0) {
        printHelp();
        return 0;
    }
    else assert(0);
    checkD3DError();

    if (mesh != NULL) {
        if (adjacency != NULL) {
            hr = mesh->OptimizeInplace(
                    D3DXMESHOPT_COMPACT | D3DXMESHOPT_ATTRSORT | D3DXMESHOPT_STRIPREORDER,
                    (const DWORD*)adjacency->GetBufferPointer(),
                    NULL, NULL, NULL);
            checkD3DError();
        }
        if ((mesh->GetOptions() & D3DXMESH_32BIT) == 0) {
            ID3DXMesh *mesh2 = NULL;
            D3DVERTEXELEMENT9 vertexDeclare[MAX_FVF_DECL_SIZE];
            hr = mesh->GetDeclaration(vertexDeclare);
            checkD3DError();
            hr = mesh->CloneMesh(
                    mesh->GetOptions() | D3DXMESH_32BIT,
                    vertexDeclare,
                    d3d->getDevice(),
                    &mesh2);
            checkD3DError();
            assert(mesh2 != NULL);
            mesh->Release();
            mesh = mesh2;
        }
        saveMesh("destMesh.txt", mesh, mats != NULL ? (D3DXMATERIAL*)mats->GetBufferPointer() : NULL, matNum);
    }

    if (mats != NULL) mats->Release();
    if (adjacency != NULL) adjacency->Release();
    if (mesh != NULL) mesh->Release();

    delete d3d;
    destroyWindow(wnd);
}
