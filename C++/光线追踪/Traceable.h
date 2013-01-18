// vim: fileencoding=gbk

#ifndef TRACEABLE_H
#define TRACEABLE_H

#include <iostream>
#include <vector>

#include "Vector.h"
#include "Matrix.h"
#include "Material.h"
#include "Geometry.h"

class Mesh;
class TriTraceAccelerator_Base;

struct TraceFragment
{
    Vector3 pos;
    Vector3 norm;
    Vector2 uv;
    const Material *mat;
    Vector3 tangentSpace[3]; 
    TraceFragment(){}
};

struct ITraceable
{
    virtual ~ITraceable() = 0 {}
    virtual bool intersect(const Ray& r, int intersectFace, float &t, TraceFragment& frag) = 0;
    virtual bool intersectTest(const Ray& r) = 0;
    virtual bool intersectSimply(const Ray& r, int intersectFace, float &t) = 0;
    virtual void applyTransform(const Matrix4x4& mat) = 0;

    virtual void printStream(std::ostream& so) const = 0;
    virtual void scanStream(std::istream& si) = 0;
};
std::istream& operator >> (std::istream& si, ITraceable*& p);
std::ostream& operator << (std::ostream& so, const ITraceable* p);


class Traceable_Sphere:
    public ITraceable
{
public:
    Traceable_Sphere();
    Traceable_Sphere(const Material& mat);

    virtual bool intersect(const Ray& r, int intersectFace, float &t, TraceFragment& frag);
    virtual bool intersectTest(const Ray& r);
    virtual bool intersectSimply(const Ray& r, int intersectFace, float &t);
    virtual void applyTransform(const Matrix4x4& mat);
    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
private:
    Material m_mat;
    Sphere m_sphere;
    Vector3 m_xyz[3];
};

class Traceable_Box:
    public ITraceable
{
public:
    Traceable_Box();
    Traceable_Box(const Material& mat);

    virtual bool intersect(const Ray& r, int intersectFace, float &t, TraceFragment& frag);
    virtual bool intersectTest(const Ray& r);
    virtual bool intersectSimply(const Ray& r, int intersectFace, float &t);
    virtual void applyTransform(const Matrix4x4& mat);
    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
private:
    Material m_mat;
    AABB m_aabb;
    Matrix4x4 m_invTrans;
    Matrix4x4 m_trans;
};

class Traceable_Cylinder:
    public ITraceable
{
public:
    Traceable_Cylinder();
    Traceable_Cylinder(const Material& mat);

    virtual bool intersect(const Ray& r, int intersectFace, float &t, TraceFragment& frag);
    virtual bool intersectTest(const Ray& r);
    virtual bool intersectSimply(const Ray& r, int intersectFace, float &t);
    virtual void applyTransform(const Matrix4x4& mat);
    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
private:
    Material m_mat;
    Matrix4x4 m_invTrans;
    Matrix4x4 m_trans;
};

class Traceable_Mesh:
    public ITraceable
{
public:
    Traceable_Mesh();
    Traceable_Mesh(const char *meshName);
    ~Traceable_Mesh();
    virtual bool intersect(const Ray& r, int intersectFace, float &t, TraceFragment& frag);
    virtual bool intersectTest(const Ray& r);
    virtual bool intersectSimply(const Ray& r, int intersectFace, float &t);
    virtual void applyTransform(const Matrix4x4& mat);

    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
private:
    const Mesh *m_mesh;
    std::vector<TriTraceAccelerator_Base*> m_accelerators;
};

#endif // #ifndef TRACEABLE_H
