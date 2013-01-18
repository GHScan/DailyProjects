// vim: fileencoding=gbk

#ifndef TRITRACEACCELERATOR_KDTREE_H
#define TRITRACEACCELERATOR_KDTREE_H

#include <iostream>

#include "TriTraceAccelerator.h"

struct SubMesh;
struct TraceFragment;
struct Ray;

class KDTree;
class TriTraceAccelerator_KDTree:
    public TriTraceAccelerator_Base
{
public:
    TriTraceAccelerator_KDTree();
    ~TriTraceAccelerator_KDTree();
    virtual void rebuild(const SubMesh* sub, const Matrix4x4& worldView);
    virtual bool intersect(const Ray& r, int intersectFace, float &t, TraceFragment& frag);
    virtual bool intersectTest(const Ray& r);
    virtual bool intersectSimply(const Ray& r, int intersectFace, float &t);
    virtual TriTraceAccelerator_Base* clone() const;
    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
private:
    KDTree *m_kdtree;
};

#endif // #ifndef TRITRACEACCELERATOR_KDTREE_H
