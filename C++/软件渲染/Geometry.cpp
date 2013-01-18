// vim: fileencoding=gbk
#include "pch.h"

#include <cmath>
#include <cassert>

#include <vector>

#include "Geometry.h"
#include "Matrix.h"

//----------------------------------------
// 几何体
//----------------------------------------
void transform(Ray& r, const Matrix4x4& mat)
{
    Vector4 v4(r.pt);
    transform(v4, mat);
    assert(fequal(v4.w, 1));
    r.pt = v4.divW();

    v4 = Vector4(r.dir, 0);
    transform(v4, mat);
    assert(fequal(v4.w, 0));
    r.dir = (Vector3&)v4;
}
void transform(Plane& p, const Matrix4x4& mat)
{
    Vector3 pt(Vector3::ZERO);
    if (!fequal(p.normal.x, 0)) {
        pt = Vector3(p.d / p.normal.x, 0, 0);
    }
    else if (!fequal(p.normal.y, 0)) {
        pt = Vector3(0, p.d / p.normal.y, 0);
    }
    else {
        assert(!fequal(p.normal.z, 0));
        pt = Vector3(0, 0, p.d / p.normal.z);
    }

    Vector4 _norm(p.normal, 0);
    transform(_norm, mat);
    assert(fequal(_norm.w, 0));
    p.normal = (Vector3&)_norm;

    Vector4 _pt(pt);
    transform(_pt, mat);
    assert(fequal(_pt.w, 1));
    p.d = p.normal.dotProduct(_pt.divW());
}

void Sphere::merge(const Vector3& pt)
{
    assert(radius > 0);
    float dis = (pt - center).length();
    if (dis > radius) {
        float newRadius = (dis + radius) / 2;
        center = center * newRadius / dis + pt * (dis - newRadius) / dis;
        radius = newRadius;
    }
}
void Sphere::merge(const Sphere& o)
{
    Vector3 farestPt = o.center + (o.center - center).normalize() * o.radius;
    merge(farestPt);
}
Sphere Sphere::fromPoints(const Vector3* beginPt, const Vector3* endPt)
{
    assert(beginPt != NULL && endPt != NULL && beginPt + 2 <= endPt);
    Vector3 p0 = *beginPt++;
    Vector3 p1 = *beginPt++;
    Vector3 center = (p0 + p1) / 2;
    float radius = (p0 - center).length();
    Sphere r(center, radius);
    while (beginPt < endPt) {
        r.merge(*beginPt++);
    }
    return r;
}
void transform(Sphere& sphere, const Matrix4x4& mat)
{
    Vector4 vs[] = {Vector4(sphere.center), Vector4(sphere.center + Vector3(sphere.radius, 0, 0))};
    transform(vs, vs + 2, mat);
    assert(fequal(vs[0].w, 1) && fequal(vs[1].w, 1));
    sphere.center = vs[0].divW();
    sphere.radius = (vs[1].divW() - sphere.center).length();
}

void transform(AABB& aabb, const Matrix4x4& mat)
{
    std::vector<Vector3> c3 = aabb.getCorners();
    std::vector<Vector4> c4(c3.size());
    for (int i = 0; i < (int)c3.size(); ++i) c4[i] = Vector4(c3[i]);
    transform(&c4[0], &c4[0] + c4.size(), mat);
    for (int i = 0; i < (int)c3.size(); ++i) {
        assert(fequal(c4[i].w, 1));
        c3[i] = c4[i].divW();
    }
    aabb = AABB::fromPoints(&c3[0], &c3[0] + c3.size());
}
void AABB::merge(const Vector3& pt)
{
    if (pt.x < minPt.x) minPt.x = pt.x;
    if (pt.y < minPt.y) minPt.y = pt.y;
    if (pt.z < minPt.z) minPt.z = pt.z;
    if (pt.x > maxPt.x) maxPt.x = pt.x;
    if (pt.y > maxPt.y) maxPt.y = pt.y;
    if (pt.z > maxPt.z) maxPt.z = pt.z;
}
void AABB::merge(const AABB& o)
{
    if (o.minPt.x < minPt.x) minPt.x = o.minPt.x;
    if (o.minPt.y < minPt.y) minPt.y = o.minPt.y;
    if (o.minPt.z < minPt.z) minPt.z = o.minPt.z;
    if (o.maxPt.x > maxPt.x) maxPt.x = o.maxPt.x;
    if (o.maxPt.y > maxPt.y) maxPt.y = o.maxPt.y;
    if (o.maxPt.z > maxPt.z) maxPt.z = o.maxPt.z;
}
std::vector<Vector3> AABB::getCorners() const
{
    Vector3 diff = maxPt - minPt;
    Vector3 corners[8] = {
        Vector3(minPt), Vector3(minPt + Vector3(0, diff.y, 0)), 
        Vector3(minPt + Vector3(diff.x, diff.y, 0)), Vector3(minPt + Vector3(diff.x, 0, 0)),
        Vector3(minPt + Vector3(0, 0, diff.z)), Vector3(minPt + Vector3(0, diff.y, diff.z)), 
        Vector3(maxPt), Vector3(minPt + Vector3(diff.x, 0, diff.z)),
    };
    return std::vector<Vector3>(corners, corners + 8);
}
AABB AABB::fromPoints(const Vector3* beginPt, const Vector3* endPt)
{
    assert(beginPt != NULL && endPt != NULL && beginPt + 2 <= endPt);
    Vector3 pt = *beginPt++;
    AABB aabb(pt, pt);
    while (beginPt < endPt) {
        aabb.merge(*beginPt++);
    }
    return aabb;
}

PlaneList AABB::getPlanes(E_PlaneID pids) const
{
    std::vector<Vector3> corners = getCorners();
    return PlaneList::fromVolume6Corners(
            &corners[0], &corners[0] + 8, pids);
}

PlaneList Frustum::getPlanes(E_PlaneID pids) const
{
    std::vector<Vector3> corners = getCorners();
    return PlaneList::fromVolume6Corners(
            &corners[0], &corners[0] + 8, pids);
}
std::vector<Vector3> Frustum::getCorners() const
{
    float tanA = tan(degree2Radian(fovY / 2));
    float nearY = tanA * nearZ;
    float nearX = nearY * aspect;
    float farY = tanA * farZ;
    float farX = farY * aspect;

    Vector3 corners[8] = {
        Vector3(-nearX, -nearY, nearZ), Vector3(-nearX, nearY, nearZ),
        Vector3(nearX, nearY, nearZ), Vector3(nearX, -nearY, nearZ),
        Vector3(-farX, -farY, farZ), Vector3(-farX, farY, farZ),
        Vector3(farX, farY, farZ), Vector3(farX, -farY, farZ),
    };

    return std::vector<Vector3>(corners, corners + 8);
}

PlaneList PlaneList::fromPlanes(const Plane* beginP, const Plane* endP)
{
    assert(beginP != NULL && endP != NULL && beginP <= endP);
    PlaneList p;
    while (beginP < endP) {
        p.planes.push_back(*beginP++);
    }
    return p;
}
PlaneList PlaneList::fromVolume6Corners(
        const Vector3* begin , const Vector3* end, E_PlaneID pids)
{
    // 六面体8个顶点
    assert(begin + 8 == end);
    const Vector3* corners = begin;
    PlaneList p;
    if (pids & EPID_front)
    p.planes.push_back(Plane(corners[0], corners[1], corners[2]));
    if (pids & EPID_right)
    p.planes.push_back(Plane(corners[2], corners[6], corners[7]));
    if (pids & EPID_left)
    p.planes.push_back(Plane(corners[4], corners[5], corners[1]));
    if (pids & EPID_back)
    p.planes.push_back(Plane(corners[6], corners[5], corners[4]));
    if (pids & EPID_bottom)
    p.planes.push_back(Plane(corners[4], corners[0], corners[3]));
    if (pids & EPID_top)
    p.planes.push_back(Plane(corners[1], corners[5], corners[6]));
    return p;
}

//----------------------------------------
// 几何体空间关系
//----------------------------------------
bool intersect(const Ray& r, const Plane& p, float &t)
{
    float n0n1 = r.dir.dotProduct(p.normal);
    if (fequal(n0n1, 0)) {
        // 射线方向和平面法线垂直
        // 返回false表示即使射线位于平面上，也认为不相交
        return false;
    }
    float p0n1 = r.pt.dotProduct(p.normal);
    float _t = (p.d - p0n1) / n0n1;
    if (_t < 0) return false;
    t = _t;
    return true;
}
bool intersect(const Ray& r, const Sphere& sphere, float &t)
{
    assert(r.dir.isUnit());
    Vector3 pt2center = sphere.center - r.pt;
    float dis = pt2center.length();
    if (dis < sphere.radius) {
        // 内部
        t = sphere.radius + pt2center.dotProduct(r.dir);
        return true;
    }
    else {
        float cosAMul = pt2center.dotProduct(r.dir);
        if (cosAMul < 0) return false;
        float cosA = cosAMul / dis;
        float sinMaxA = sphere.radius / dis;
        if (cosA * cosA + sinMaxA * sinMaxA < 1) return false;
        t = cosAMul - sqrt(
                sphere.radius * sphere.radius - (
                    dis * dis - cosAMul * cosAMul));
        return true;
    }
}
bool intersect(const Ray& r, const PlaneList& planeList, float &t)
{
    bool b = false;
    float t0 = 0;
    for (int i = 0; i < (int)planeList.planes.size(); ++i) {
        float t2;
        if (intersect(r, planeList.planes[i], t2)) {
            if (!b) t0 = t2;
            else t0 = std::min(t0, t2);
            b = true;
        }
    }
    if (b) t = t0;
    return b;
}

bool intersect(const LineSeg& lineSeg, const Plane& p, float &t)
{
    Ray r(lineSeg.begin, (lineSeg.end - lineSeg.begin).normalize());
    float t0;
    if (intersect(r, p, t0) && t0 <= 1) {
        t = t0;
        return true;
    }
    return false;
}

float distance(const Plane& p, const Vector3& pt)
{
    return pt.dotProduct(p.normal) - p.d;
}
E_IntersectCase intersect(const Plane& p, const Sphere& sphere)
{
    float dis = distance(p, sphere.center);
    if (fabs(dis) <= sphere.radius) return EIC_intersection;
    return dis > 0 ? EIC_front : EIC_back;
}
E_IntersectCase intersect(const Plane& p, const std::vector<Vector3>& pointList)
{
    assert(!pointList.empty());
    float dis = distance(p, pointList[0]);
    if (dis >= 0) {
        for (int i = 1; i < (int)pointList.size(); ++i) {
            if (distance(p, pointList[i]) < 0) return EIC_intersection;
        }
        return EIC_front;
    }
    else {
        for (int i = 1; i < (int)pointList.size(); ++i) {
            if (distance(p, pointList[i]) >= 0) return EIC_intersection;
        }
        return EIC_back;
    }
}

E_IntersectCase intersect(const Sphere& sphere0, const Sphere& sphere1)
{
    float dis = (sphere0.center - sphere1.center).length();
    float radiusDiff = fabs(sphere0.radius - sphere1.radius);
    float radiusSum = sphere0.radius + sphere1.radius;
    if (dis > radiusSum) return EIC_front;
    else if (dis > radiusDiff) return EIC_intersection;
    else {
        // 包含关系统统认为相交, 不再具体划分方向
        return EIC_intersection;
        // 0包含1
        if (sphere0.radius >= sphere1.radius) return EIC_back;
        // 1包含0
        return EIC_front;
    }
}
E_IntersectCase intersect(const Sphere& sphere, const AABB& aabb)
{
    Vector3 off(sphere.radius);
    AABB t(sphere.center - off, sphere.center + off);
    return intersect(t, aabb);
}
E_IntersectCase intersect(const AABB& aabb0, const AABB& aabb1)
{
    if (aabb0.minPt.x > aabb1.maxPt.x) return EIC_front;
    if (aabb0.minPt.y > aabb1.maxPt.y) return EIC_front;
    if (aabb0.minPt.z > aabb1.maxPt.z) return EIC_front;
    if (aabb1.minPt.x > aabb0.maxPt.x) return EIC_front;
    if (aabb1.minPt.y > aabb0.maxPt.y) return EIC_front;
    if (aabb1.minPt.z > aabb0.maxPt.z) return EIC_front;
    // 相交和包含都判断为相交
    return EIC_intersection;
}

E_IntersectCase intersect(const PlaneList& planeList, const Sphere& sphere)
{
    for (int i = 0; i < (int)planeList.planes.size(); ++i) {
        if (intersect(planeList.planes[i], sphere) == EIC_front) return EIC_front;
    }
    return EIC_intersection;
}
E_IntersectCase intersect(const PlaneList& planeList, const std::vector<Vector3>& pointList)
{
    for (int i = 0; i < (int)planeList.planes.size(); ++i) {
        if (intersect(planeList.planes[i], pointList) == EIC_front) return EIC_front;
    }
    return EIC_intersection;
}
