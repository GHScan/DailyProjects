// vim: fileencoding=gbk

#ifndef TRITRACEACCELERATOR_H
#define TRITRACEACCELERATOR_H

#include <iostream>
#include <vector>

#include "TriGeometry.h"
#include "Geometry.h"

struct SubMesh;
struct TraceFragment;
struct Ray;
struct Matrix4x4;
struct Material;

class TriTraceAccelerator_Base
{
public:
    TriTraceAccelerator_Base();
    virtual ~TriTraceAccelerator_Base();
    virtual void rebuild(const SubMesh* sub, const Matrix4x4& worldView);
    virtual bool intersect(const Ray& r, int intersectFace, float &t, TraceFragment& frag);
    virtual bool intersectTest(const Ray& r);
    virtual bool intersectSimply(const Ray& r, int intersectFace, float &t);
    virtual TriTraceAccelerator_Base* clone() const;
    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
protected:
    std::vector<Triangle> m_tris;
    const Material *m_mat;
    AABB m_aabb;
};

std::ostream& operator << (std::ostream& so, const TriTraceAccelerator_Base* p);
std::istream& operator >> (std::istream& si, TriTraceAccelerator_Base*& p);

#endif // #ifndef TRITRACEACCELERATOR_H
