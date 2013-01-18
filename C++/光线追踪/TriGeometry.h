// vim: fileencoding=gbk

#ifndef TRIGEOMETRY_H
#define TRIGEOMETRY_H

#include "Vector.h"
#include "Geometry.h"

struct TraceFragment;

struct Triangle
{
    Vector3 p0, p1, p2;
    Vector3 n0, n1, n2;
    Vector2 uv0, uv1, uv2;
    Plane plane;
};

bool isPointInTriangle(const Vector3& pt, const Triangle& tri, Vector2& ab);
void interpolateNormUV(
        TraceFragment& frag, const Triangle& tri, const Vector2& ab);
void updateTangentSpace(TraceFragment& frag, const Triangle& tri);

#endif // #ifndef TRIGEOMETRY_H
