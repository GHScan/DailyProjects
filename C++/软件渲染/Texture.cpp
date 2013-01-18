// vim: fileencoding=gbk
#include "pch.h"

#include <cassert>

#include "Texture.h"
#include "Util.h"

extern char* loadImage(const char *fname, void*(*fmalloc)(int), int &w, int &h);
extern bool saveImage(const char *fname, const char *buf, int w, int h, int pitch);

static float* loadTexture(const char *fname, int &w, int &h)
{
    char *sp = loadImage(fname, (void*(*)(int))malloc, w, h);
    if (sp == NULL) return NULL;
    float *p = (float*)malloc(w * h * 4 * sizeof(float));
    float* d = p;
    unsigned char* s = (unsigned char*)sp;
    for (int y = 0; y < h; ++y) {
        for (int x = 0; x < w; ++x) {
            d[0] = s[3] / 255.f;
            d[1] = s[2] / 255.f;
            d[2] = s[1] / 255.f;
            d[3] = s[0] / 255.f;
            d += 4;
            s += 4;
        }
    }
    free(sp);
    return p;
}
static bool saveTexture(const char *fname, const float *src, int w, int h)
{
    char *buf = (char*)malloc(w * h * 4);
    unsigned char *d = (unsigned char*)buf;
    for (int y = 0; y < h; ++y) {
        for (int x = 0; x < w; ++x) {
            d[0] = (int)(src[3] * 255);
            d[1] = (int)(src[2] * 255);
            d[2] = (int)(src[1] * 255);
            d[3] = (int)(src[0] * 255);
            d += 4;
            src += 4;
        }
    }
    bool b = saveImage(fname, buf, w, h, w * 4);
    free(buf);
    return b;
}
//----------------------------------------
// Texture
//----------------------------------------

Texture::Texture():
    m_w(0), m_h(0)
{
}
Texture::Texture(const char *fname):
    m_w(0), m_h(0)
{
    load(fname);
}

Texture::~Texture()
{
    clear();
}
void Texture::clear()
{
    for (int i = 0; i < (int)m_bufs.size(); ++i) {
        free(m_bufs[i]);
    }
    m_bufs.clear();
    m_w = m_h = 0;
}

bool Texture::valid() const
{
    return m_bufs.size() > 0;
}
bool Texture::hasMipmap() const
{
    return m_bufs.size() > 1;
}
int Texture::maxMipmapLevel() const
{
    if (m_bufs.empty()) return -1;
    return (int)m_bufs.size() - 1;
}

bool Texture::load(const char *fname)
{
    int w = 0, h = 0;
    float *p = loadTexture(fname, w, h);
    if (p == NULL) return false;
    clear();
    m_bufs.push_back(p);
    m_w = w, m_h = h;
    if (m_w == m_h && isPower2(m_w)) genMipmap();
    return true;
}
bool Texture::save(const char *fname) const
{
    assert(!m_bufs.empty());
    return saveTexture(fname, m_bufs[0], m_w, m_h);
}
void Texture::genMipmap()
{
    if (m_bufs.size() > 1) return;
    assert(m_w == m_h && isPower2(m_w));
    for (int w = m_w >> 1; w > 0; w >>= 1) {
        float *buf = (float*)malloc(w * w * 4 * sizeof(float));
        for (int y = 0; y < w; ++y) {
            float *d = buf + y * w * 4;
            float *s0 = m_bufs.back() + (y * 2 + 0) * (w << 1) * 4;
            float *s1 = m_bufs.back() + (y * 2 + 1) * (w << 1) * 4;
            for (int x = 0; x < w; ++x) {
                d[0] = (s0[0] + s0[4] + s1[0] + s1[4]) / 4;
                d[1] = (s0[1] + s0[5] + s1[1] + s1[5]) / 4;
                d[2] = (s0[2] + s0[6] + s1[2] + s1[6]) / 4;
                d[3] = (s0[3] + s0[7] + s1[3] + s1[7]) / 4;
                d += 4;
                s0 += 8;
                s1 += 8;
            }
        }
        m_bufs.push_back(buf);
    }
}

int Texture::width(int level) const 
{ 
    assert(level >= 0 && level < (int)m_bufs.size());
    return m_w >> level; 
}
int Texture::height(int level) const 
{ 
    assert(level >= 0 && level < (int)m_bufs.size());
    return m_h >> level; 
}
const float* Texture::data(int level) const 
{ 
    assert(level >= 0 && level < (int)m_bufs.size());
    return m_bufs[level];
}
//----------------------------------------
// TextureManager
//----------------------------------------
TextureManager* TextureManager::instance()
{
    static TextureManager s_mgr;
    return &s_mgr;
}
const Texture* TextureManager::find(const char *fname)
{
    assert(fname != NULL);
    TextureMap::iterator iter = m_map.find(fname);
    if (iter != m_map.end()) return iter->second;
    return m_map[fname] = new Texture(fname);
}
TextureManager::~TextureManager()
{
    clear();
}
void TextureManager::clear()
{
    for (TextureMap::iterator iter = m_map.begin();
            iter != m_map.end(); 
            ++iter) {
        delete iter->second;
    }
    m_map.clear();
}
