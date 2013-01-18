// vim: fileencoding=gbk
#include "pch.h"

#include "TriGeometry.h"
#include "Util.h"
#include "Traceable.h"

extern int g_callCnt_pointInTriangleCheck;
extern int g_callCnt_interpolate;
bool isPointInTriangle(const Vector3& pt, const Triangle& tri, Vector2& ab)
{
    ++g_callCnt_pointInTriangleCheck;
    Vector3 v2(pt); v2 -= tri.p0;
    Vector3 v1(tri.p2); v1 -= tri.p0;
    Vector3 v0(tri.p1); v0 -= tri.p0;
    {
        // 解x, y
        float ls[4] = {v0.x, v1.x, v0.y, v1.y};
        float rs[2] = {v2.x, v2.y};
        if (solveEquations2(ls, rs, ab.data())) {
            return ab.x >= 0 && ab.y >= 0 && ab.x + ab.y <= 1;
        }
    }
    {
        // 解x, z
        float ls[4] = {v0.x, v1.x, v0.z, v1.z};
        float rs[2] = {v2.x, v2.z};
        if (solveEquations2(ls, rs, ab.data())) {
            return ab.x >= 0 && ab.y >= 0 && ab.x + ab.y <= 1;
        }
    }
    {
        // 解y, z
        float ls[4] = {v0.y, v1.y, v0.z, v1.z};
        float rs[2] = {v2.y, v2.z};
        if (solveEquations2(ls, rs, ab.data())) {
            return ab.x >= 0 && ab.y >= 0 && ab.x + ab.y <= 1;
        }
    }
    assert(0);
    return false;
}
void interpolateNormUV(
        TraceFragment& frag, const Triangle& tri, const Vector2& ab)
{
    ++g_callCnt_interpolate;
    Vector3 n10(tri.n1 - tri.n0), n20(tri.n2 - tri.n0);
    Vector2 uv10(tri.uv1 - tri.uv0), uv20(tri.uv2 - tri.uv0);
    frag.norm = tri.n0;
    frag.norm += n10 *= ab.x;
    frag.norm += n20 *= ab.y;
    frag.norm = frag.norm.normalize();

    if (!frag.mat->texture.empty() || !frag.mat->bumpTexture.empty()) {
        frag.uv = tri.uv0;
        frag.uv += uv10 *= ab.x;
        frag.uv += uv20 *= ab.y;
    }
}

void updateTangentSpace(TraceFragment& frag, const Triangle& tri)
{
    if (frag.mat->bumpTexture.empty()) return;

    Vector3 v1(tri.p1), v2(tri.p2);
    v1 -= tri.p0, v2 -= tri.p0;
    float uvMat[] = {
        tri.uv1.x - tri.uv0.x, tri.uv1.y - tri.uv0.y,
        tri.uv2.x - tri.uv0.x, tri.uv2.y - tri.uv0.y,
    };
    float det = uvMat[0] * uvMat[3] - uvMat[1] * uvMat[2];
    if (fequal(det, 0)) return ;
    float invDet = 1 / det;
    float invUVMat[] = {
        uvMat[3] * invDet, -uvMat[1] * invDet, 
        -uvMat[2] * invDet, uvMat[0] * invDet,
    };
    frag.tangentSpace[0] = Vector3(
            invUVMat[0] * v1.x + invUVMat[1] * v2.x, 
            invUVMat[0] * v1.y + invUVMat[1] * v2.y, 
            invUVMat[0] * v1.z + invUVMat[1] * v2.z);
    frag.tangentSpace[0] /= frag.tangentSpace[0].length();
    frag.tangentSpace[1] = frag.norm;
    frag.tangentSpace[2] = frag.tangentSpace[0].crossProduct(frag.tangentSpace[1]);
    frag.tangentSpace[0] = frag.tangentSpace[1].crossProduct(frag.tangentSpace[2]);
}
