// vim: fileencoding=gbk

#ifndef GEOMETRY_H
#define GEOMETRY_H

#include <cassert>

#include <vector>

#include "Vector.h"

struct Matrix4x4;
//----------------------------------------
// 几何体
//----------------------------------------
struct Ray
{
    Vector3 pt;
    Vector3 dir;
    Ray():pt(Vector3::ZERO), dir(Vector3::AXIS_Z){}
    Ray(const Vector3& _pt, const Vector3& _dir):
        pt(_pt), dir(_dir){ assert(dir.isUnit()); }
    Vector3 getPoint(float dis) const
    {
        Vector3 r(pt);
        r += Vector3(dir) *= dis;
        return r;
    }
    float getT(const Vector3& dest) const
    {
        Vector3 pt2Dest(dest);
        pt2Dest -= pt;
        if (!fequal(dir.x, 0)) return pt2Dest.x / dir.x;
        if (!fequal(dir.y, 0)) return pt2Dest.y / dir.y;
        if (!fequal(dir.z, 0)) return pt2Dest.z / dir.z;
        assert(0);
        return MAX_FLOAT;
    }
};
void transform(Ray& r, const Matrix4x4& mat);

struct LineSeg
{
    Vector3 begin;
    Vector3 end;
    LineSeg(const Vector3& _begin, const Vector3& _end):
    begin(_begin), end(_end){}
};

struct Plane
{
    Vector3 normal;
    float d; // x * n.x + y * n.y + z * n.z = d
    Plane(): normal(Vector3::AXIS_Y), d(0){}
    Plane(const Vector3& _norm, float _d): normal(_norm), d(_d){ assert(normal.isUnit()); }
    Plane(const Vector3& pt0, const Vector3& pt1, const Vector3& pt2)
    {
        normal = ((pt2 - pt1).crossProduct(pt0 - pt1)).normalize();
        d = pt0.dotProduct(normal);
    }
};
void transform(Plane& p, const Matrix4x4& mat);

struct Sphere
{
    Vector3 center;
    float radius;
    Sphere(): center(Vector3::ZERO), radius(1){}
    Sphere(const Vector3& _center, float _radius):center(_center), radius(_radius){}
    void merge(const Vector3& pt);
    void merge(const Sphere& o);

    static Sphere fromPoints(const Vector3* beginPt, const Vector3* endPt);
};
void transform(Sphere& sphere, const Matrix4x4& mat);

enum E_PlaneID
{
    EPID_front = 0x1,
    EPID_back = 0x2,
    EPID_left = 0x4,
    EPID_right = 0x8,
    EPID_top = 0x10,
    EPID_bottom = 0x20,
    EPID_all = 0x3f,
};

typedef std::vector<Plane> PlaneList;
static PlaneList makePlaneList(const Plane* beginP, const Plane* endP);
static PlaneList makePlaneListWithVolume6Corners(
        const Vector3* begin , const Vector3* end, E_PlaneID pids);

struct AABB
{
    Vector3 minPt;
    Vector3 maxPt;
    AABB():minPt(Vector3(-MAX_FLOAT)), maxPt(Vector3(MAX_FLOAT)){}
    AABB(const Vector3& _minPt, const Vector3& _maxPt): minPt(_minPt), maxPt(_maxPt){}
    void merge(const Vector3& pt);
    void merge(const AABB& o);

    std::vector<Vector3> getCorners() const;
    PlaneList getPlanes(E_PlaneID pids) const;

    bool operator == (const AABB& o) const 
    { 
        return minPt == o.minPt && maxPt == o.maxPt;
    }
    bool contain(const AABB& o) const 
    {
        AABB t(*this);
        t.merge(o);
        return t == *this;
    }
    bool contain(const Vector3& pt) const 
    {
        if (minPt.x > pt.x) return false;
        if (minPt.y > pt.y) return false;
        if (minPt.z > pt.z) return false;
        if (maxPt.x < pt.x) return false;
        if (maxPt.y < pt.y) return false;
        if (maxPt.z < pt.z) return false;
        return true;
    }

    static AABB fromPoints(const Vector3* beginPt, const Vector3* endPt);
};
void transform(AABB& aabb, const Matrix4x4& mat);

struct Frustum
{
    float nearZ, farZ;
    float fovY;
    float aspect;
    Frustum(float _fovY, float _aspect, float _nearZ, float _farZ):
        fovY(_fovY), aspect(_aspect), nearZ(_nearZ), farZ(_farZ){}

    std::vector<Vector3> getCorners() const;
    PlaneList getPlanes(E_PlaneID pids) const;
};

//----------------------------------------
// 几何体空间关系
//----------------------------------------
enum E_SpaceRelation
{
    ESR_front = 1,
    ESR_intersection = 0,
    ESR_back = -1,
};
enum E_IntersectCase
{
    EIC_none = 0,
    EIC_front = 1,
    EIC_back = 2,
};

E_IntersectCase intersect(const Ray& r, const Plane& p, float &t);
E_IntersectCase intersect(const Ray& r, const Sphere& sphere, float &t);

E_IntersectCase intersect(const LineSeg& lineSeg, const Plane& p, float &t);

float distance(const Plane& p, const Vector3& pt);
E_SpaceRelation intersect(const Plane& p, const Sphere& sphere);
E_SpaceRelation intersect(const Plane& p, const std::vector<Vector3>& pointList);

E_SpaceRelation intersect(const Sphere& sphere0, const Sphere& sphere1);
E_SpaceRelation intersect(const Sphere& sphere, const AABB& aabb);
E_SpaceRelation intersect(const AABB& aabb0, const AABB& aabb1);

bool isPointInside(const Vector3& pt, const PlaneList& planeList);
E_SpaceRelation intersect(const PlaneList& planeList, const Sphere& sphere);
E_SpaceRelation intersect(const PlaneList& planeList, const std::vector<Vector3>& pointList);

bool reflect(Vector3& dir, const Vector3& norm);
bool refract(Vector3& dir, const Vector3& norm, float refractIndex);

bool rayIntersectAABB(const Ray& r, const AABB& aabb, float ts[2]);

// 这里所谓的标准圆柱体特征是外接AABB为(-1, -1, -1)、(1, 1, 1)
E_IntersectCase intersectStandardCylinder(const Ray& r, float &t);

#endif // #ifndef GEOMETRY_H
