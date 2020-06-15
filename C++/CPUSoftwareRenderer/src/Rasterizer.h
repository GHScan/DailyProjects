#ifndef RASTERIZER_H
#define RASTERIZER_H


#include <algorithm>

#include "Vector.h"
#include "BoundingBox.h"


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

    Vector3 Execute(float x, float y) const
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



inline std::vector<Vector2> const& GetMultiSamplePosition(int sampleCount)
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

    auto &samplePosList = GetMultiSamplePosition(sampleCount);

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



#endif
