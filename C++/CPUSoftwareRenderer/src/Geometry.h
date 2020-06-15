#ifndef GEOMETRY_H
#define GEOMETRY_H


#include <limits>

#include "Util.h"
#include "Vector.h"
#include "Matrix.h"


template<int dim>
struct AxisAlignedBoundingBox
{
    Vector<dim> Min;
    Vector<dim> Max;

    AxisAlignedBoundingBox()
    {
        for (int i = 0; i < dim; ++i)
        {
            Min.Val[i] = std::numeric_limits<float>::max();
            Max.Val[i] = -std::numeric_limits<float>::max();
        }
    }

    void AddPoint(Vector<dim> const &pt)
    {
        for (int i = 0; i < dim; ++i)
        {
            Min.Val[i] = std::min(Min.Val[i], pt.Val[i]);
            Max.Val[i] = std::max(Max.Val[i], pt.Val[i]);
        }
    }

    bool Valid() const
    {
        return Min.Val[0] < Max.Val[0];
    }

    Vector<dim> Center() const
    {
        return (Min + Max) * 0.5f;
    }

    Vector<dim> Size() const
    {
        return Max - Min;
    }
};

using AABB2 = AxisAlignedBoundingBox<2>;
using AABB3 = AxisAlignedBoundingBox<3>;


namespace Detail
{

inline void Transform(
    AABB3 &newBounds,
    AABB3 const &bounds,
    Matrix4 const &trans,
    int i, Vector3 &pt)
{
    if (i == 3)
    {
        newBounds.AddPoint((Vector4(pt, 1) * trans).Proj<3>());
    }
    else
    {
        pt.Val[i] = bounds.Min.Val[i];
        Transform(newBounds, bounds, trans, i + 1, pt);
        pt.Val[i] = bounds.Max.Val[i];
        Transform(newBounds, bounds, trans, i + 1, pt);
    }
}

}

inline AABB3 Transform(AABB3 const &bounds, Matrix4 const &trans)
{
    AABB3 newBounds;
    Vector3 pt;
    Detail::Transform(newBounds, bounds, trans, 0, pt);
    return newBounds;
}


#endif
