#include "StdAfx.h"

#include "Math.h"

#include "MemoryCheck.h"

namespace Scan
{
    namespace Math
    {
        const Vector3 Vector3::UNIT_X(1, 0, 0);
        const Vector3 Vector3::UNIT_Y(0, 1, 0);
        const Vector3 Vector3::UNIT_Z(0, 0, 1);
        const Vector3 Vector3::ZERO(0, 0, 0);

        const Matrix33 Matrix33::IDENTITY(Vector3::UNIT_X, Vector3::UNIT_Y, Vector3::UNIT_Z);
    }
}