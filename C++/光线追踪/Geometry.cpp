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
    transformPoint(r.pt, mat);
    transformDirection(r.dir, mat);
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

    transformDirection(p.normal, mat);

    transformPoint(pt, mat);
    p.d = p.normal.dotProduct(pt);
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
    Vector3 pt(sphere.center + Vector3(sphere.radius));
    transformPoint(sphere.center, mat);
    transformPoint(pt, mat);
    sphere.radius = (pt - sphere.center).length();
}

void transform(AABB& aabb, const Matrix4x4& mat)
{
    std::vector<Vector3> c3 = aabb.getCorners();
    for (int i = 0; i < (int)c3.size(); ++i) transformPoint(c3[i], mat);
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
    return makePlaneListWithVolume6Corners(
            &corners[0], &corners[0] + 8, pids);
}

PlaneList Frustum::getPlanes(E_PlaneID pids) const
{
    std::vector<Vector3> corners = getCorners();
    return makePlaneListWithVolume6Corners(
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

PlaneList makePlaneList(const Plane* beginP, const Plane* endP)
{
    assert(beginP != NULL && endP != NULL && beginP <= endP);
    PlaneList p;
    while (beginP < endP) {
        p.push_back(*beginP++);
    }
    return p;
}
PlaneList makePlaneListWithVolume6Corners(
        const Vector3* begin , const Vector3* end, E_PlaneID pids)
{
    // 六面体8个顶点
    assert(begin + 8 == end);
    const Vector3* corners = begin;
    PlaneList p;
    if (pids & EPID_front)
    p.push_back(Plane(corners[0], corners[1], corners[2]));
    if (pids & EPID_right)
    p.push_back(Plane(corners[2], corners[6], corners[7]));
    if (pids & EPID_left)
    p.push_back(Plane(corners[4], corners[5], corners[1]));
    if (pids & EPID_back)
    p.push_back(Plane(corners[6], corners[5], corners[4]));
    if (pids & EPID_bottom)
    p.push_back(Plane(corners[4], corners[0], corners[3]));
    if (pids & EPID_top)
    p.push_back(Plane(corners[1], corners[5], corners[6]));
    return p;
}

//----------------------------------------
// 几何体空间关系
//----------------------------------------
extern int g_callCnt_rayIntersectPlane;
E_IntersectCase intersect(const Ray& r, const Plane& p, float &t)
{
    ++g_callCnt_rayIntersectPlane;
    float n0n1 = r.dir.dotProduct(p.normal);
    if (fequal(n0n1, 0)) {
        // 射线方向和平面法线垂直
        // 返回false表示即使射线位于平面上，也认为不相交
        return EIC_none;
    }
    float p0n1 = r.pt.dotProduct(p.normal);
    float dis = p.d - p0n1;
    float _t = dis / n0n1;
    if (_t < 0) return EIC_none;
    t = _t;
    return dis > 0 ? EIC_back : EIC_front;
}
E_IntersectCase intersect(const Ray& r, const Sphere& sphere, float &t)
{
    assert(r.dir.isUnit());
    Vector3 pt2center = sphere.center - r.pt;
    float dis = pt2center.length();
    if (fequal(dis - sphere.radius, 0)) return EIC_none; // 在圆上
    if (dis < sphere.radius) {
        // 内部
        float pt2centerDotDir = pt2center.dotProduct(r.dir);
        Vector3 v3(r.dir);
        v3 *= pt2centerDotDir;
        v3 = pt2center - v3;
        t = pt2centerDotDir + sqrt(
                sphere.radius * sphere.radius - v3.lengthSqr());
        return EIC_back;
    }
    else {
        float cosAMul = pt2center.dotProduct(r.dir);
        if (cosAMul < 0) return EIC_none;
        float cosA = cosAMul / dis;
        float sinMaxA = sphere.radius / dis;
        if (cosA * cosA + sinMaxA * sinMaxA < 1) return EIC_none;
        t = cosAMul - sqrt(
                sphere.radius * sphere.radius - (
                    dis * dis - cosAMul * cosAMul));
        return EIC_front;
    }
}
E_IntersectCase intersect(const LineSeg& lineSeg, const Plane& p, float &t)
{
    Ray r(lineSeg.begin, (lineSeg.end - lineSeg.begin).normalize());
    float t0;
    E_IntersectCase ic = intersect(r, p, t0);
    if (ic != EIC_none && t0 <= 1) {
        t = t0;
        return ic;
    }
    return EIC_none;
}

float distance(const Plane& p, const Vector3& pt)
{
    return pt.dotProduct(p.normal) - p.d;
}
E_SpaceRelation intersect(const Plane& p, const Sphere& sphere)
{
    float dis = distance(p, sphere.center);
    if (fabs(dis) <= sphere.radius) return ESR_intersection;
    return dis > 0 ? ESR_front : ESR_back;
}
E_SpaceRelation intersect(const Plane& p, const std::vector<Vector3>& pointList)
{
    assert(!pointList.empty());
    float dis = distance(p, pointList[0]);
    if (dis >= 0) {
        for (int i = 1; i < (int)pointList.size(); ++i) {
            if (distance(p, pointList[i]) < 0) return ESR_intersection;
        }
        return ESR_front;
    }
    else {
        for (int i = 1; i < (int)pointList.size(); ++i) {
            if (distance(p, pointList[i]) >= 0) return ESR_intersection;
        }
        return ESR_back;
    }
}

E_SpaceRelation intersect(const Sphere& sphere0, const Sphere& sphere1)
{
    float dis = (sphere0.center - sphere1.center).length();
    float radiusDiff = fabs(sphere0.radius - sphere1.radius);
    float radiusSum = sphere0.radius + sphere1.radius;
    if (dis > radiusSum) return ESR_front;
    else if (dis > radiusDiff) return ESR_intersection;
    else {
        // 包含关系统统认为相交, 不再具体划分方向
        return ESR_intersection;
        // 0包含1
        if (sphere0.radius >= sphere1.radius) return ESR_back;
        // 1包含0
        return ESR_front;
    }
}
E_SpaceRelation intersect(const Sphere& sphere, const AABB& aabb)
{
    Vector3 off(sphere.radius);
    AABB t(sphere.center - off, sphere.center + off);
    return intersect(t, aabb);
}
E_SpaceRelation intersect(const AABB& aabb0, const AABB& aabb1)
{
    if (aabb0.minPt.x > aabb1.maxPt.x) return ESR_front;
    if (aabb0.minPt.y > aabb1.maxPt.y) return ESR_front;
    if (aabb0.minPt.z > aabb1.maxPt.z) return ESR_front;
    if (aabb1.minPt.x > aabb0.maxPt.x) return ESR_front;
    if (aabb1.minPt.y > aabb0.maxPt.y) return ESR_front;
    if (aabb1.minPt.z > aabb0.maxPt.z) return ESR_front;
    // 相交和包含都判断为相交
    return ESR_intersection;
}

bool isPointInside(const Vector3& pt, const PlaneList& planeList)
{
    for (int i = 0; i < (int)planeList.size(); ++i) {
        if (distance(planeList[i], pt) > EPSILON) return false;
    }
    return true;
}
E_SpaceRelation intersect(const PlaneList& planeList, const Sphere& sphere)
{
    for (int i = 0; i < (int)planeList.size(); ++i) {
        if (intersect(planeList[i], sphere) == ESR_front) return ESR_front;
    }
    return ESR_intersection;
}
E_SpaceRelation intersect(const PlaneList& planeList, const std::vector<Vector3>& pointList)
{
    for (int i = 0; i < (int)planeList.size(); ++i) {
        if (intersect(planeList[i], pointList) == ESR_front) return ESR_front;
    }
    return ESR_intersection;
}

bool reflect(Vector3& dir, const Vector3& norm)
{
    assert(dir.isUnit() && norm.isUnit());
    float d = dir.dotProduct(norm);
    // 垂直或不是入射
    if (d >= 0) return false;
    Vector3 t(norm);
    t *= d * -2;
    dir += t;
    return true;
}
bool refract(Vector3& dir, const Vector3& norm, float refractIndex)
{
    assert(dir.isUnit() && norm.isUnit());
    float cosI = dir.dotProduct(norm);
    // 垂直或不是入射
    if (cosI >= 0) return false;
    // 正面入射
    if (fequal(cosI, -1)) return true; 
    assert(refractIndex > EPSILON);

    // 公式: refract = vt * (1 / idx) + vn * sqrt(1 / cosI^2 + (cosI^2 - 1) / (idx^2 * cosI^2))
    // 其中vt是dir的垂直分量，vn是dir的norm方向分量
    float cosI2 = cosI * cosI;
    float invCosI2 = 1 / cosI2;
    float invIdx = 1 / refractIndex;
    float invIdx2 = invIdx * invIdx;
    Vector3 vn(norm);
    vn *= cosI;
    dir -= vn; // dir == vt

    dir *= invIdx;
    float f = invCosI2 + (cosI2 - 1) * invIdx2 * invCosI2;
    // 入射角太大，且类似玻璃入射空气，结果退化为反射
    if (f < 0) return false; 
    vn *= sqrt(f);
    dir += vn;
    return true;
}
bool rayIntersectAABB(const Ray& r, const AABB& aabb, float ts[2])
{
    float minT = -MAX_FLOAT, maxT = MAX_FLOAT;

    if (!fequal(r.dir.x, 0)) {
        float t0 = (aabb.minPt.x - r.pt.x) / r.dir.x;
        float t1 = (aabb.maxPt.x - r.pt.x) / r.dir.x;
        if (t0 > t1) std::swap(t0, t1);
        if (t0 > minT) minT = t0;
        if (t1 < maxT) maxT = t1;
    }
    else {
        if (r.pt.x < aabb.minPt.x || r.pt.x > aabb.maxPt.x) return false;
    }

    if (!fequal(r.dir.y, 0)) {
        float t0 = (aabb.minPt.y - r.pt.y) / r.dir.y;
        float t1 = (aabb.maxPt.y - r.pt.y) / r.dir.y;
        if (t0 > t1) std::swap(t0, t1);
        if (t0 > minT) minT = t0;
        if (t1 < maxT) maxT = t1;
    }
    else {
        if (r.pt.y < aabb.minPt.y || r.pt.y > aabb.maxPt.y) return false;
    }

    if (!fequal(r.dir.z, 0)) {
        float t0 = (aabb.minPt.z - r.pt.z) / r.dir.z;
        float t1 = (aabb.maxPt.z - r.pt.z) / r.dir.z;
        if (t0 > t1) std::swap(t0, t1);
        if (t0 > minT) minT = t0;
        if (t1 < maxT) maxT = t1;
    }
    else {
        if (r.pt.z < aabb.minPt.z || r.pt.z > aabb.maxPt.z) return false;
    }

    if (maxT < EPSILON) return false;
    else if (minT < EPSILON) {
        ts[0] = EPSILON;
        ts[1] = maxT;
        return true;
    }
    else {
        if (minT > maxT) return false;
        ts[0] = minT;
        ts[1] = maxT;
        return true;
    }
}

E_IntersectCase intersectStandardCylinder(const Ray& r, float &t)
{
    float minT = EPSILON, maxT = MAX_FLOAT;
    if (!fequal(r.dir.y, 0)) {
        float t0 = (-1 - r.pt.y) / r.dir.y;
        float t1 = (1 - r.pt.y) / r.dir.y;
        if (t0 > t1) std::swap(t0, t1);
        if (t1 < 0) return EIC_none;
        minT = std::max(minT, t0);
        maxT = std::min(t1, maxT);
    }
    else {
        if (r.pt.y < -1 || r.pt.y > 1) return EIC_none;
    }
    if (minT > maxT) return EIC_none;

    // x,z到原点距离为1，即解方程
    float a = r.dir.x * r.dir.x + r.dir.z * r.dir.z;
    float b = 2 * (r.pt.x * r.dir.x + r.pt.z * r.dir.z);
    float c = r.pt.x * r.pt.x + r.pt.z * r.pt.z - 1;
    float b2_4ac = b * b - 4 * a * c;
    if (b2_4ac < 0) return EIC_none;
    if (fequal(b2_4ac, 0)) { // 相切
        float _t = (-b + sqrt(b2_4ac)) / (2 * a);
        if (_t >= minT && _t <= maxT) {
            t = _t;
            return EIC_front;
        }
    }
    else {
        float inv2A = 1 / (2 * a);
        float sq_b2_4ac = sqrt(b2_4ac);
        float t0 = (-b - sq_b2_4ac) * inv2A;
        float t1 = (-b + sq_b2_4ac) * inv2A;
        assert(t0 < t1);
        if (t0 < 0) t0 = EPSILON;
        if (t1 <= minT || t0 >= maxT) return EIC_none;
        if (minT > EPSILON || t0 > EPSILON) { // 外部
            t = std::max(minT, t0);
            return EIC_front;
        }
        else { // 内部
            t = std::min(maxT, t1);
            return EIC_back;
        }
    }
    return EIC_none;
}
