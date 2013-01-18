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
    Ray(const Vector3& _pt, const Vector3& _dir):
        pt(_pt), dir(_dir){ assert(dir.isUnit()); }
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

struct PlaneList
{
    std::vector<Plane> planes;
    static PlaneList fromPlanes(const Plane* beginP, const Plane* endP);
    static PlaneList fromVolume6Corners(
            const Vector3* begin , const Vector3* end, E_PlaneID pids);
};

struct AABB
{
    Vector3 minPt;
    Vector3 maxPt;
    AABB(const Vector3& _minPt, const Vector3& _maxPt): minPt(_minPt), maxPt(_maxPt){}
    void merge(const Vector3& pt);
    void merge(const AABB& o);

    std::vector<Vector3> getCorners() const;
    PlaneList getPlanes(E_PlaneID pids) const;

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
enum E_IntersectCase
{
    EIC_front = 1,
    EIC_intersection = 0,
    EIC_back = -1,
};

bool intersect(const Ray& r, const Plane& p, float &t);
bool intersect(const Ray& r, const Sphere& sphere, float &t);
bool intersect(const Ray& r, const PlaneList& planeList, float &t);

bool intersect(const LineSeg& lineSeg, const Plane& p, float &t);

float distance(const Plane& p, const Vector3& pt);
E_IntersectCase intersect(const Plane& p, const Sphere& sphere);
E_IntersectCase intersect(const Plane& p, const std::vector<Vector3>& pointList);

E_IntersectCase intersect(const Sphere& sphere0, const Sphere& sphere1);
E_IntersectCase intersect(const Sphere& sphere, const AABB& aabb);
E_IntersectCase intersect(const AABB& aabb0, const AABB& aabb1);

E_IntersectCase intersect(const PlaneList& planeList, const Sphere& sphere);
E_IntersectCase intersect(const PlaneList& planeList, const std::vector<Vector3>& pointList);

#endif // #ifndef GEOMETRY_H
