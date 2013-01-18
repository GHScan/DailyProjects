#include "pch.h"

#include <cstring>

#include <io.h>

#include <windows.h>
#include <gdiplus.h>

#include "Util.h"

#pragma comment(lib, "gdiplus")

using namespace Gdiplus;

class GdiplusEnv
{
public:
    GdiplusEnv()
    {
        GdiplusStartupInput gdiplusStartupInput;
        GdiplusStartup(&m_gdiplusToken, &gdiplusStartupInput, NULL);
    }
    ~GdiplusEnv()
    {
        GdiplusShutdown(m_gdiplusToken);
    }
private:
    ULONG_PTR m_gdiplusToken;
};
GdiplusEnv g_gdiplusEnv;

char* loadImage(const char *fname, 
        void*(*fmalloc)(int), int &w, int &h)
{
    wchar_t wfname[MAX_PATH];
    MultiByteToWideChar(CP_ACP, 0, fname, -1, wfname, sizeof(wfname) / sizeof(wchar_t));

    Bitmap *bm = Bitmap::FromFile(wfname);
    if (bm == NULL) return NULL;
    BitmapData data;
    Rect rt(0, 0, bm->GetWidth(), bm->GetHeight());
    if (bm->LockBits(&rt, ImageLockModeRead, PixelFormat32bppARGB, &data) == Ok) {
        w = rt.Width, h = rt.Height;
        char *buf = (char*)fmalloc(w * h * 4);
        char *src = (char*)data.Scan0, *dest = buf;
        for (int y = 0; y < h; ++y) {
            memcpy(dest, src, w * 4);
            src += data.Stride;
            dest += w * 4;
        }
        bm->UnlockBits(&data);
        return buf;
    }
    return NULL;
}

// from msdn
static int GetEncoderClsid(const WCHAR* format, CLSID* pClsid)
{
    UINT  num = 0;          // number of image encoders
    UINT  size = 0;         // size of the image encoder array in bytes

    ImageCodecInfo* pImageCodecInfo = NULL;
    GetImageEncodersSize(&num, &size);
    if(size == 0) return -1;  // Failure

    pImageCodecInfo = (ImageCodecInfo*)(malloc(size));
    if(pImageCodecInfo == NULL) return -1;  // Failure

    GetImageEncoders(num, size, pImageCodecInfo);

    for(UINT j = 0; j < num; ++j) {
        if(wcscmp(pImageCodecInfo[j].MimeType, format) == 0) {
            *pClsid = pImageCodecInfo[j].Clsid;
            free(pImageCodecInfo);
            return j;  // Success
        }    
    }

    free(pImageCodecInfo);
    return -1;  // Failure
}

static std::wstring getNotExistFileNameW(const char *fname)
{
    std::string s(getNotExistFileName(fname));
    wchar_t wfname[MAX_PATH];
    MultiByteToWideChar(CP_ACP, 0, s.c_str(), -1, wfname, sizeof(wfname) / sizeof(wchar_t));
    return std::wstring(wfname);
}

bool saveImage(const char *fname, const char *buf, int w, int h, int pitch)
{
    Bitmap bm(w, h, PixelFormat32bppARGB);
    BitmapData data;
    Rect rt(0, 0, bm.GetWidth(), bm.GetHeight());
    if (bm.LockBits(&rt, ImageLockModeWrite, PixelFormat32bppARGB, &data) == Ok) {
        const char *src = buf;
        char *dest = (char*)data.Scan0;
        for (int y = 0; y < h; ++y) {
            memcpy(dest, src, w * 4);
            dest += data.Stride;
            src += pitch;
        }
        bm.UnlockBits(&data);
    }
    else return false;

    CLSID pngClsid;
    if (GetEncoderClsid(L"image/png", &pngClsid) == -1) return false;
    return bm.Save(getNotExistFileNameW(fname).c_str(), &pngClsid, NULL) == Ok;
}
