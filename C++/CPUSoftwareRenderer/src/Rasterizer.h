#ifndef RASTERIZER_H
#define RASTERIZER_H

#include <atomic>

#include "Vector.h"
#include "Geometry.h"


// optimized version of CalculateBarycentric()
class BarycentricCalculator
{
public:
    BarycentricCalculator(Vector3 screenPts[3])
    {
        mPt0X = screenPts[0].Val[0];
        mPt0Y = screenPts[0].Val[1];
        mV10X = screenPts[1].Val[0] - screenPts[0].Val[0];
        mV20X = screenPts[2].Val[0] - screenPts[0].Val[0];
        mV10Y = screenPts[1].Val[1] - screenPts[0].Val[1];
        mV20Y = screenPts[2].Val[1] - screenPts[0].Val[1];
        mUVZ = mV10X * mV20Y - mV20X * mV10Y;
        mInvUVZ = 1 / mUVZ;
    }

    bool Valid() const
    {
        return !Equals(0, mUVZ);
    }

    Vector3 Execute(float x, float y)
    {
        float x0_ = mPt0X - x;
        float y0_ = mPt0Y - y;

        float uvX = mV20X * y0_ - x0_ * mV20Y;
        float uvY = x0_ * mV10Y - mV10X * y0_;
        
        float resY = uvX * mInvUVZ;
        float resZ = uvY * mInvUVZ;
        return Vector3(1 - resY - resZ, resY, resZ);
    }

private:
    float mV10X, mV20X;
    float mV10Y, mV20Y;
    float mUVZ, mInvUVZ;
    float mPt0X, mPt0Y;
};



inline Vector3 PerspectiveCorrect(
    Vector3 const &barycenteric, 
    Vector3 const &invW3)
{
    float invW = barycenteric.Dot(invW3);
    return barycenteric * invW3 * (1 / invW);
}


inline bool IsBackface(Vector4 ndcPts[3])
{
    auto dir10 = ndcPts[1] - ndcPts[0];
    auto dir20 = ndcPts[2] - ndcPts[0];
    return dir10.Cross(dir20).Val[2] < 0;
}

inline std::vector<Vector2> const& GetMultiSamplePattern(int sampleCount)
{
    switch (sampleCount)
    {
    case 2:
    {
        static std::vector<Vector2> gPosList =
        {
             {1 / 4.f, 1 / 4.f}, {3 / 4.f, 3 / 4.f},
        };
        return gPosList;
    }
    case 4:
    {
        static std::vector<Vector2> gPosList =
        {
            {5 / 8.f, 1 / 8.f}, {1 / 8.f, 3 / 8.f}, {7 / 8.f, 5 / 8.f}, {3 / 8.f, 7 / 8.f},
        };
        return gPosList;
    }
    case 8:
    {
        static std::vector<Vector2> gPosList =
        {
            {17 / 32.f, 1 / 32.f}, {9 / 32.f, 5 / 32.f}, {1 / 32.f, 9 / 32.f}, {25 / 32.f, 13 / 32.f},
            {5 / 32.f, 17 / 32.f}, {29 / 32.f, 21 / 32.f}, {21 / 32.f, 25 / 32.f}, {13 / 32.f, 29 / 32.f},
        };
        return gPosList;
    }
    case 16:
    {
        static std::vector<Vector2> gPosList =
        {
            {1 / 8.f, 1 / 8.f}, {3 / 8.f, 1 / 8.f}, {5 / 8.f, 1 / 8.f}, {7 / 8.f, 1 / 8.f},
            {1 / 8.f, 3 / 8.f}, {3 / 8.f, 3 / 8.f}, {5 / 8.f, 3 / 8.f}, {7 / 8.f, 3 / 8.f},
            {1 / 8.f, 5 / 8.f}, {3 / 8.f, 5 / 8.f}, {5 / 8.f, 5 / 8.f}, {7 / 8.f, 5 / 8.f},
            {1 / 8.f, 7 / 8.f}, {3 / 8.f, 7 / 8.f}, {5 / 8.f, 7 / 8.f}, {7 / 8.f, 7 / 8.f},
        };
        return gPosList;
    }
    default:
    {
        static std::vector<Vector2> gPosList;
        return gPosList;
    }
    }
}

inline void DrawTriangle(
    int tri,
    int w, int h,
    int x0, int y0, int x1, int y1,
    int *coverBuf, Vector3 *bcBuf,
    float *zBuf,
    Vector3 screenPts[3])
{
    int tileW = x1 - x0;
    int minX, maxX, minY, maxY;
    {
        AABB2 bb;
        for (int i = 0; i < 3; ++i)
            bb.AddPoint(screenPts[i].Proj<2>());

        minX = std::max(x0, int(std::floor(bb.Min.Val[0])));
        maxX = std::min(x1 - 1, int(std::ceil(bb.Max.Val[0])));
        minY = std::max(y0, int(std::floor(bb.Min.Val[1])));
        maxY = std::min(y1 - 1, int(std::ceil(bb.Max.Val[1])));
    }
    if (minX > maxX || minY > maxY)
        return;

    BarycentricCalculator bcCalc(screenPts);
    if (!bcCalc.Valid())
        return;

    Vector3 screenZs;
    for (int i = 0; i < 3; ++i)
        screenZs.Val[i] = screenPts[i].Val[2];

    for (int y = minY; y <= maxY; ++y)
    {
        for (int x = minX; x <= maxX; ++x)
        {
            Vector3 bc = bcCalc.Execute(x + 0.5f, y + 0.5f);
            if (bc.Val[0] < 0 || bc.Val[1] < 0 || bc.Val[2] < 0)
                continue;

            int off = y * w + x;
            int tileOff = (y - y0) * tileW + x - x0;

            float z = bc.Dot(screenZs);
            if (z < 0 || z > 1 || z < zBuf[off])
                continue;

            zBuf[off] = z;

            coverBuf[tileOff] = tri;
            bcBuf[tileOff] = bc;
        }
    }
}

inline void DrawTriangleMultiSample(
    int tri,
    int sampleCount, bool perSampleShading,
    int w, int h,
    int x0, int y0, int x1, int y1,
    int *coverBuf, Vector3 *bcBuf,
    float *zBuf,
    Vector3 screenPts[3])
{
    if (sampleCount == 1)
    {
        DrawTriangle(
            tri,
            w, h,
            x0, y0, x1, y1, 
            coverBuf, bcBuf,
            zBuf,
            screenPts);
        return;
    }

    int tileW = x1 - x0;
    int minX, maxX, minY, maxY;
    {
        AABB2 bb;
        for (int i = 0; i < 3; ++i)
            bb.AddPoint(screenPts[i].Proj<2>());

        minX = std::max(x0, int(std::floor(bb.Min.Val[0])));
        maxX = std::min(x1 - 1, int(std::ceil(bb.Max.Val[0])));
        minY = std::max(y0, int(std::floor(bb.Min.Val[1])));
        maxY = std::min(y1 - 1, int(std::ceil(bb.Max.Val[1])));
    }
    if (minX > maxX || minY > maxY)
        return;

    BarycentricCalculator bcCalc(screenPts);
    if (!bcCalc.Valid())
        return;

    Vector3 screenZs;
    for (int i = 0; i < 3; ++i)
        screenZs.Val[i] = screenPts[i].Val[2];

    auto &samplePosList = GetMultiSamplePattern(sampleCount);

    for (int y = minY; y <= maxY; ++y)
    {
        for (int x = minX; x <= maxX; ++x)
        {
            int off = (y * w + x) * sampleCount;
            int tileOff = ((y - y0) * tileW + x - x0) * sampleCount;

            bool shaded = false;
            Vector3 bc;

            for (int i = 0; i < sampleCount; ++i)
            {
                auto &samplePos = samplePosList[i];

                Vector3 sampleBc = bcCalc.Execute(x + samplePos.Val[0], y + samplePos.Val[1]);
                if (sampleBc.Val[0] < 0 || sampleBc.Val[1] < 0 || sampleBc.Val[2] < 0)
                    continue;

                float z = sampleBc.Dot(screenZs);
                if (z < 0 || z > 1 || z < zBuf[off + i])
                    continue;

                zBuf[off + i] = z;

                if (perSampleShading)
                {
                    bc = sampleBc;
                }
                else
                {
                    if (!shaded)
                    {
                        shaded = true;
                        bc = bcCalc.Execute(x + 0.5f, y + 0.5f);
                    }
                }

                coverBuf[tileOff + i] = tri;
                bcBuf[tileOff + i] = bc;
            }
        }
    }
}

template<typename TriangleAttr, typename PixelShader>
inline void RunPixelShaders(
    int sampleCount, bool perSampleShading,
    int w, int h, 
    uint32_t *fBuf, int fBufStride,
    int *coverBuf, Vector3 const *bcBuf,
    Vector3 const *invWBuf, TriangleAttr const *attrBuf, 
    PixelShader const &ps)
{
    for (int h0 = 0; h0 < h; ++h0, fBuf += fBufStride)
    {
        uint32_t *fBuf0 = fBuf;
        for (int w0 = 0; w0 < w; ++w0)
        {
            for (int i = 0; i < sampleCount; ++i)
            {
                int tri = coverBuf[i];
                coverBuf[i] = -1;
                if (tri != -1)
                {
                    uint32_t clr = ps(
                        PerspectiveCorrect(bcBuf[i], invWBuf[tri]), attrBuf[tri]);

                    fBuf0[i] = clr;
                    if (!perSampleShading)
                    {
                        for (int j = i + 1; j < sampleCount; ++j)
                        {
                            if (coverBuf[j] == tri)
                            {
                                coverBuf[j] = -1;
                                fBuf0[j] = clr;
                            }
                        }
                    }
                }
            }

            fBuf0 += sampleCount;
            coverBuf += sampleCount;
            bcBuf += sampleCount;
        }
    }
}


template<typename VertexShader, typename PixelShader>
inline void DrawMesh(
    int td,
    int sampleCount, bool perSampleShading,
    bool cullBackface,
    int w, int h, uint32_t *fBuf, float *zBuf,
    std::vector<int> const &triCounts,
    VertexShader const &vs, PixelShader const &ps)
{
    using TriangleAttri = typename std::result_of<VertexShader(Vector4[3], int, int)>::type;


    int totalTriCount = 0;
    for (int triCount : triCounts)
        totalTriCount += triCount;


    int hTileSize = h, wTileSize = w;
    if (td > 1)
    {
        int tileCount = 1;
        while (tileCount < td * td)
        {
            if (hTileSize > wTileSize)
                hTileSize = DivideUp(hTileSize, 2);
            else
                wTileSize = DivideUp(wTileSize, 2);
            tileCount *= 2;
        }
    }
    int hTileCount = DivideUp(h, hTileSize);
    int wTileCount = DivideUp(w, wTileSize);
    int tileCount = hTileCount * wTileCount;


    auto vpMat = Matrix4::Viewport(0, 0, w, h);


    static std::vector<Vector3> gTriScreenPtBuf;
    static std::vector<Vector3> gTriInvWBuf;
    static std::vector<TriangleAttri> gTriAttriBuf;
    static std::vector<std::vector<std::vector<int>>> gVisibleTriBufs;
    static std::vector<std::vector<int>> gCoverBufs;
    static std::vector<std::vector<Vector3>> gBcBufs;

    gTriScreenPtBuf.resize(totalTriCount * 3);
    gTriInvWBuf.resize(totalTriCount);
    gTriAttriBuf.resize(totalTriCount);
    gVisibleTriBufs.resize(td);
    for (auto &tdBuf : gVisibleTriBufs)
    {
        tdBuf.resize(tileCount);
        for (auto &tileBuf : tdBuf)
            tileBuf.clear();
    }
    gCoverBufs.resize(td);
    for (auto &buf : gCoverBufs)
        buf.resize(hTileSize * wTileSize * sampleCount, -1);
    gBcBufs.resize(td);
    for (auto &buf : gBcBufs)
        buf.resize(hTileSize * wTileSize * sampleCount);


    int triCountPerTd = DivideUp(totalTriCount, td);

    ParallelFor(td, [&](int tid)
    {
        auto &visibleTriBuf = gVisibleTriBufs[tid];

        int tri0 = tid * triCountPerTd;
        int triCountThisTd = std::min(triCountPerTd, totalTriCount - tri0);

        // figure out the batch & triangle idx
        int batch = 0;
        int batTri0 = tri0;
        while (batTri0 >= triCounts[batch])
        {
            batTri0 -= triCounts[batch];
            ++batch;
        }

        while (triCountThisTd-- > 0)
        {
            // process triangle
            do
            {
                Vector4 clipPts[3];

                auto attr = vs(clipPts, batch, batTri0);

                Vector4 ndcPts[3];
                Vector3 invWs;
                for (int i = 0; i < 3; ++i)
                {
                    float invW = 1 / clipPts[i].Val[3];
                    ndcPts[i] = clipPts[i] * invW;
                    invWs.Val[i] = invW;
                }
                if (cullBackface && IsBackface(ndcPts))
                    break;

                gTriInvWBuf[tri0] = invWs;
                gTriAttriBuf[tri0] = attr;

                Vector3 screenPts[3];
                for (int i = 0; i < 3; ++i)
                {
                    screenPts[i] = (ndcPts[i] * vpMat).Proj<3>();
                    gTriScreenPtBuf[tri0 * 3 + i] = screenPts[i];
                }

                {
                    AABB2 screenBB;
                    for (int i = 0; i < 3; ++i)
                        screenBB.AddPoint(screenPts[i].Proj<2>());

                    int wT0 = std::max(0, int(std::floor(screenBB.Min.Val[0]))) / wTileSize;
                    int wT1 = std::min(w - 1, int(std::ceil(screenBB.Max.Val[0]))) / wTileSize;
                    int hT0 = std::max(0, int(std::floor(screenBB.Min.Val[1]))) / hTileSize;
                    int hT1 = std::min(h - 1, int(std::ceil(screenBB.Max.Val[1]))) / hTileSize;
                    for (int h0 = hT0; h0 <= hT1; ++h0)
                    {
                        for (int x0 = wT0; x0 <= wT1; ++x0)
                            visibleTriBuf[h0 * wTileCount + x0].push_back(tri0);
                    }
                }

            } while (false);

            ++tri0;
            ++batTri0;
            if (batTri0 >= triCounts[batch])
            {
                batTri0 = 0;
                ++batch;
            }
        }
    });

    std::atomic<int> tileIdGen{0};
    ParallelFor(td, [&](int tid)
    {
        auto &coverBuf = gCoverBufs[tid];
        auto &bcBuf = gBcBufs[tid];

        for (;;)
        {
            int tile = tileIdGen++;
            if (tile >= tileCount) break;
            int wT0 = tile % wTileCount, hT0 = tile / wTileCount;
            int x0 = wT0 * wTileSize, y0 = hT0 * hTileSize;
            int x1 = std::min(w, x0 + wTileSize), y1 = std::min(h, y0 + hTileSize);

            for (int tid = 0; tid < td; ++tid)
            {
                for (int tri0 : gVisibleTriBufs[tid][tile])
                {
                    DrawTriangleMultiSample(
                        tri0,
                        sampleCount, perSampleShading,
                        w, h,
                        x0, y0, x1, y1,
                        coverBuf.data(), bcBuf.data(),
                        zBuf,
                        &gTriScreenPtBuf[tri0 * 3]);
                }
            }

            if (fBuf != nullptr)
            {
                int fBufStride = w * sampleCount;

                RunPixelShaders(
                    sampleCount, perSampleShading,
                    x1 - x0, y1 - y0,
                    fBuf + y0 * fBufStride + x0 * sampleCount, fBufStride,
                    coverBuf.data(), bcBuf.data(),
                    gTriInvWBuf.data(), gTriAttriBuf.data(),
                    ps);
            }
        }
    });
}


inline void ResolveMultiSampleBuffer(
    int td,
    int sampleCount,
    int w, int h,
    uint32_t *output, uint32_t const *input)
{
    int shift = int(std::log2(sampleCount));

    int n = w * h;
    int block = DivideUp(n, td);
    ParallelFor(td, [&](int tid)
    {
        int i0 = tid * block;
        int i1 = std::min(i0 + block, n);
        for (int i = i0; i < i1; ++i)
        {
            uint32_t const *pin = input + i * sampleCount;
            uint32_t rgba = pin[0];
            uint32_t r = (rgba >> 24) & 0xff;
            uint32_t g = (rgba >> 16) & 0xff;
            uint32_t b = (rgba >> 8) & 0xff;
            for (int j = 1; j < sampleCount; ++j)
            {
                rgba = pin[j];
                r += (rgba >> 24) & 0xff;
                g += (rgba >> 16) & 0xff;
                b += (rgba >> 8) & 0xff;
            }
            r >>= shift;
            g >>= shift;
            b >>= shift;
            output[i] = (r << 24) | (g << 16) | (b << 8) | 0xff;
        }
    });
}


template<typename T>
inline void ClearBuffer(
    int td, 
    T *buf, int size, T value)
{
    int bSize = DivideUp(size, td);
    ParallelFor(td, [&](int tid)
    {
        int i0 = tid * bSize;
        int i1 = std::min(size, i0 + bSize);
        std::fill(buf + i0, buf + i1, value);
    });
}


#endif
