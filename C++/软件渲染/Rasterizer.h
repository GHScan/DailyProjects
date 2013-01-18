// vim: fileencoding=gbk

#ifndef RASTERIZER_H
#define RASTERIZER_H

#include <cassert>

#include <algorithm>

#include "Vector.h"
#include "Sampler.h"
#include "VectorT.h"
#include "Light.h"

struct Vector2;
struct Vector3;
class Light;

inline int colorVector2Int(const Vector3& clr)
{
    assert(clr.x >= 0 && clr.y >= 0 && clr.z >= 0);
    int r = (int)(clr.x * 255);
    int g = (int)(clr.y * 255);
    int b = (int)(clr.z * 255);
    return 
        ((r > 255 ? 255 : r) << 16) | 
        ((g > 255 ? 255 : g) << 8) | 
        (b > 255 ? 255 : b);
}
inline int color255Vector2Int(const Vector3& clr)
{
    assert(clr.x >= 0 && clr.y >= 0 && clr.z >= 0);
    int r = (int)clr.x;
    int g = (int)clr.y;
    int b = (int)clr.z;
    return 
        ((r > 255 ? 255 : r) << 16) | 
        ((g > 255 ? 255 : g) << 8) | 
        (b > 255 ? 255 : b);
}

struct PixelShader_Flat
{
    int operator () (
            float v, int color, const Sampler* sampler) const
    {
        return color;
    }
};
struct PixelShader_TexturedFlat
{
    int operator () (
            const Vector2& uv, const Vector3& color, const Sampler* sampler) const
    {
        return color255Vector2Int(color.multiply(sampler->sample(uv)));
    }
};
struct PixelShader_TexturedFlatWithSpecular
{
    int operator () (
            const Vector2& uv, const std::pair<Vector3, Vector3>& clrs, const Sampler* sampler) const
    {
        Vector3 clr(clrs.first);
        clr.multiplyInplace(sampler->sample(uv)) += clrs.second;
        return color255Vector2Int(clr);
    }
};
struct PixelShader_Gouraud
{
    int operator () (
            const Vector3& clr, int _, const Sampler* sampler) const
    {
        return color255Vector2Int(clr);
    }
};
struct PixelShader_TexturedGouraud
{
    int operator () (
            const Vector2T<Vector3, Vector2>& v, int _, const Sampler* sampler) const
    {
        const Vector3& clr = v.t0;
        const Vector2& uv = v.t1;
        return color255Vector2Int(clr.multiply(sampler->sample(uv)));
    }
};
struct PixelShader_TexturedGouraudWithSpecular
{
    int operator () (
            const Vector3T<Vector3, Vector3, Vector2>& v, int _, const Sampler* sampler) const
    {
        const Vector3& clr = v.t0;
        const Vector3& clr2 = v.t1;
        const Vector2& uv = v.t2;
        Vector3 r(clr);
        r.multiplyInplace(sampler->sample(uv)) += clr2;
        return color255Vector2Int(r);
    }
};
struct PixelShader_Phong
{
    // 这里其实忽略了顶点颜色；但是一则顶点颜色很少用，二则再加入一个差值量
    // 开销太大
    int operator () (
            const Vector2T<Vector3, Vector3>& v, const Light* l, const Sampler* sampler) const
    {
        const Vector3& pos = v.t0;
        const Vector3& norm = v.t1;
        return colorVector2Int(l->illuminate(pos, norm));
    }
};
struct PixelShader_PhongWithSpecular
{
    // 这里其实忽略了顶点颜色；但是一则顶点颜色很少用，二则再加入一个差值量
    // 开销太大
    int operator () (
            const Vector2T<Vector3, Vector3>& v, const Light* l, const Sampler* sampler) const
    {
        const Vector3& pos = v.t0;
        const Vector3& norm = v.t1;
        return colorVector2Int(l->illuminateWithSpecular(pos, norm));
    }
};
struct PixelShader_TexturedPhong
{
    int operator () (
            const Vector3T<Vector3, Vector3, Vector2>& v, const Light* l, const Sampler* sampler) const
    {
        const Vector3& pos = v.t0;
        const Vector3& norm = v.t1;
        const Vector2& uv = v.t2;
        Vector3 r(l->illuminate(pos, norm));
        r.multiplyInplace(sampler->sample(uv));
        return colorVector2Int(r);
    }
};
struct PixelShader_TexturedPhongWithSpecular
{
    int operator () (
            const Vector3T<Vector3, Vector3, Vector2>& v, const Light* l, const Sampler* sampler) const
    {
        const Vector3& pos = v.t0;
        const Vector3& norm = v.t1;
        const Vector2& uv = v.t2;
        Vector3 specular(Vector3::ZERO);
        Vector3 r = l->illuminateWithSeparateSpecular(pos, norm, specular);
        r.multiplyInplace(sampler->sample(uv)) += specular;
        return colorVector2Int(r);
    }
};

class Rasterizer
{
public:
    Rasterizer(char *buf, int w, int h, int pitch, 
            float *zbuf, Sampler* sampler);

    void drawPoint(int x, int y, int rgb);
    void drawLine(int x0, int y0, int x1, int y1, int rgb);

    void drawFrameTriangle(
            int x0, int y0, int x1, int y1, int x2, int y2,
            int rgb);

    template<E_ZBufferType zbufT, typename PixelShader, typename VectorN, typename ConstT>
    void drawTriangle(
            const Vector3& p0, const VectorN& d0, 
            const Vector3& p1, const VectorN& d1,
            const Vector3& p2, const VectorN& d2,
            const ConstT& c);

    int width() const;
    int height() const;
    int pitch() const;

private:
    template<E_ZBufferType zbufT, typename PixelShader, typename VectorN, typename ConstT>
    void drawTriangle_Top(
            const Vector3& p0, const VectorN& d0, 
            const Vector3& p1, const VectorN& d1,
            const Vector3& p2, const VectorN& d2,
            const ConstT& c);
    template<E_ZBufferType zbufT, typename PixelShader, typename VectorN, typename ConstT>
    void drawTriangle_Bottom(
            const Vector3& p0, const VectorN& d0, 
            const Vector3& p1, const VectorN& d1,
            const Vector3& p2, const VectorN& d2,
            const ConstT& c);
private:
    char *m_buf;
    float *m_zbuf;
    int m_w, m_h, m_pitch;
    Sampler *m_sampler;
};

template<E_ZBufferType zbufT, typename PixelShader, typename VectorN, typename ConstT>
void Rasterizer::drawTriangle(
        const Vector3& _p0, const VectorN& _d0, 
        const Vector3& _p1, const VectorN& _d1,
        const Vector3& _p2, const VectorN& _d2,
        const ConstT& c)
{
    Vector3 p0(_p0), p1(_p1), p2(_p2);
    VectorN d0(_d0), d1(_d1), d2(_d2);

    if (zbufT == EZBT_1zbuf) {
        p0.z = 1 / p0.z; d0 *= p0.z;
        p1.z = 1 / p1.z; d1 *= p1.z;
        p2.z = 1 / p2.z; d2 *= p2.z;
    }

    if (p0.y > p1.y) {
        std::swap(p0, p1);
        std::swap(d0, d1);
    }
    if (p0.y > p2.y) {
        std::swap(p0, p2);
        std::swap(d0, d2);
    }
    if (p1.y > p2.y) {
        std::swap(p1, p2);
        std::swap(d1, d2);
    }

    if (fequal(p0.y, p1.y)) {
        if (fequal(p1.y, p2.y)) return; // 退化
        if (fequal(p0.x, p1.x)) return; // 退化
        if (p0.x > p1.x) {
            std::swap(p0, p1);
            std::swap(d0, d1);
        }
        drawTriangle_Top<zbufT, PixelShader, VectorN, ConstT>(p0, d0, p1, d1, p2, d2, c);
    }
    else if (fequal(p1.y, p2.y)) {
        if (fequal(p1.x, p2.x)) return; // 退化
        if (p1.x > p2.x) {
            std::swap(p1, p2);
            std::swap(d1, d2);
        }
        drawTriangle_Bottom<zbufT, PixelShader, VectorN, ConstT>(p0, d0, p1, d1, p2, d2, c);
    }
    else {
        float r = (p1.y - p0.y) / (p2.y - p0.y);
        Vector3 p3(p2 * r  + p0 * (1 - r));
        p3.y = p1.y; // 避免误差
        VectorN d3(d2 * r + d0 * (1 - r));
        if (fequal(p1.x, p3.x)) return; // 退化
        if (p1.x > p3.x) {
            std::swap(p1, p3);
            std::swap(d1, d3);
        }
        drawTriangle_Bottom<zbufT, PixelShader, VectorN, ConstT>(p0, d0, p1, d1, p3, d3, c);
        drawTriangle_Top<zbufT, PixelShader, VectorN, ConstT>(p1, d1, p3, d3, p2, d2, c);
    }
}
template<E_ZBufferType zbufT, typename PixelShader, typename VectorN, typename ConstT>
void Rasterizer::drawTriangle_Top(
        const Vector3& p0, const VectorN& d0, 
        const Vector3& p1, const VectorN& d1,
        const Vector3& p2, const VectorN& d2,
        const ConstT& c)
{
    assert(fequal(p0.y, p1.y) && p0.y < p2.y && p0.x < p1.x);

    int yfirst = std::max((int)ceil(p0.y), 0);
    int ylast = std::min((int)ceil(p2.y) - 1, m_h - 1);
    char *buf = m_buf + yfirst * m_pitch;

    float ldx = (p2.x - p0.x) / (p2.y - p0.y);
    float rdx = (p2.x - p1.x) / (p2.y - p1.y);
    float lx = p0.x + ldx * (yfirst - p0.y);
    float rx = p1.x + rdx * (yfirst - p1.y);

    VectorN ldd = (d2 - d0) / (p2.y - p0.y);
    VectorN rdd = (d2 - d1) / (p2.y - p1.y);
    VectorN ld = d0 + ldd * (yfirst - p0.y);
    VectorN rd = d1 + rdd * (yfirst - p1.y);

    float ldz, rdz, lz, rz;
    float *zbuf;
    if (zbufT != EZBT_null) {
        ldz = (p2.z - p0.z) / (p2.y - p0.y);
        rdz = (p2.z - p1.z) / (p2.y - p1.y);
        lz = p0.z + ldz * (yfirst - p0.y);
        rz = p1.z + rdz * (yfirst - p1.y);
        zbuf = m_zbuf + yfirst * m_w;
    }

    PixelShader pshader;
    for (int y = yfirst; y <= ylast; ++y) {
        int xfirst = std::max((int)ceil(lx), 0);
        int xlast = std::min((int)ceil(rx) - 1, m_w - 1);

        VectorN dd = (rx - lx <= EPSILON) ? zero<VectorN>() : (rd - ld) / (rx - lx);
        VectorN d = ld + dd * (xfirst - lx);

        float dz, z;
        if (zbufT != EZBT_null) {
            dz = (rx - lx <= EPSILON) ? 0 : (rz -lz) / (rx - lx);
            z = lz + dz * (xfirst - lx);
        }

        for (int x = xfirst; x <= xlast; ++x) {

            bool draw = true;
            if (zbufT == EZBT_zbuf) {
                if (z < zbuf[x]) zbuf[x] = z;
                else draw = false;
            }
            if (zbufT == EZBT_1zbuf) {
                if (z > zbuf[x]) zbuf[x] = z;
                else draw = false;
            }

            if (draw) {
                if (zbufT == EZBT_1zbuf) {
                    ((int*)buf)[x] = pshader(d / z, c, m_sampler);
                }
                else {
                    ((int*)buf)[x] = pshader(d, c, m_sampler);
                }
            }

            d += dd;
            if (zbufT != EZBT_null) {
                z += dz;
            }
        }

        lx += ldx;
        rx += rdx;
        ld += ldd;
        rd += rdd;
        buf += m_pitch;
        if (zbufT != EZBT_null) {
            lz += ldz;
            rz += rdz;
            zbuf += m_w;
        }
    }
}
template<E_ZBufferType zbufT, typename PixelShader, typename VectorN, typename ConstT>
void Rasterizer::drawTriangle_Bottom(
        const Vector3& p0, const VectorN& d0, 
        const Vector3& p1, const VectorN& d1,
        const Vector3& p2, const VectorN& d2,
        const ConstT& c)
{
    assert(fequal(p1.y, p2.y) && p0.y < p1.y && p1.x < p2.x);

    int yfirst = std::max((int)ceil(p0.y), 0);
    int ylast = std::min((int)ceil(p1.y) - 1, m_h - 1);
    char *buf = m_buf + yfirst * m_pitch;

    float ldx = (p1.x - p0.x) / (p1.y - p0.y);
    float rdx = (p2.x - p0.x) / (p2.y - p0.y);
    float lx = p0.x + ldx * (yfirst - p0.y);
    float rx = p0.x + rdx * (yfirst - p0.y);

    VectorN ldd = (d1 - d0) / (p1.y - p0.y);
    VectorN rdd = (d2 - d0) / (p2.y - p0.y);
    VectorN ld = d0 + ldd * (yfirst - p0.y);
    VectorN rd = d0 + rdd * (yfirst - p0.y);

    float ldz, rdz, lz, rz;
    float *zbuf;
    if (zbufT != EZBT_null) {
        ldz = (p1.z - p0.z) / (p1.y - p0.y);
        rdz = (p2.z - p0.z) / (p2.y - p0.y);
        lz = p0.z + ldz * (yfirst - p0.y);
        rz = p0.z + rdz * (yfirst - p0.y);
        zbuf = m_zbuf + yfirst * m_w;
    }

    PixelShader pshader;
    for (int y = yfirst; y <= ylast; ++y) {
        int xfirst = std::max((int)ceil(lx), 0);
        int xlast = std::min((int)ceil(rx) - 1, m_w - 1);

        VectorN dd = (rx - lx <= EPSILON) ? zero<VectorN>() : (rd - ld) / (rx - lx);
        VectorN d = ld + dd * (xfirst - lx);

        float dz, z;
        if (zbufT != EZBT_null) {
            dz = (rx - lx <= EPSILON) ? 0 : (rz -lz) / (rx - lx);
            z = lz + dz * (xfirst - lx);
        }

        for (int x = xfirst; x <= xlast; ++x) {

            bool draw = true;
            if (zbufT == EZBT_zbuf) {
                if (z < zbuf[x]) zbuf[x] = z;
                else draw = false;
            }
            if (zbufT == EZBT_1zbuf) {
                if (z > zbuf[x]) zbuf[x] = z;
                else draw = false;
            }

            if (draw) {
                if (zbufT == EZBT_1zbuf) {
                    ((int*)buf)[x] = pshader(d / z, c, m_sampler);
                }
                else {
                    ((int*)buf)[x] = pshader(d, c, m_sampler);
                }
            }

            d += dd;
            if (zbufT != EZBT_null) {
                z += dz;
            }
        }

        lx += ldx;
        rx += rdx;
        ld += ldd;
        rd += rdd;
        buf += m_pitch;
        if (zbufT != EZBT_null) {
            lz += ldz;
            rz += rdz;
            zbuf += m_w;
        }
    }
}

#endif // #ifndef RASTERIZER_H
