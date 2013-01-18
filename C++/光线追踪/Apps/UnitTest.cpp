#include "pch.h"

#include <cassert>

#include "Matrix.h"

int main()
{
    Matrix2x2 m22;
    Matrix3x3 m33;
    Matrix4x4 m44;

    assert(
    Matrix2x2(
        2, 1,
        -1, 2).determinant() == 5);
    assert(
    Matrix2x2(
        -3, 4,
        2, 5).determinant() == -23);

    assert(
    Matrix3x3(
        3, -2, 0,
        1, 4, -3,
        -1, 0, 2).determinant() == 22);

    m33 = Matrix3x3(
            -4, -3, 3,
            0, 2, -2,
            1, 4, -1);
    assert(
            m33.algebraicCofactor() == 
            Matrix3x3(
                6, -2, -2,
                9, 1, 13, 
                0, -8, -8));
    assert(
            m33.standardAdjugate() == 
    Matrix3x3(
        6, 9, 0,
        -2, 1, -8,
        -2, 13, -8));
    assert(m33.determinant() == -24);
    assert(m33.inverse() == 
            Matrix3x3(
                -1.0 / 4, -3.0 / 8, 0,
                1.0 / 12, -1.0 / 24, 1.0 / 3,
                1.0 / 12, -13.0 / 24, 1.0 / 3));

    assert(
            Matrix2x2::IDENTITY.inverse() * 
            Matrix2x2::IDENTITY ==
            Matrix2x2::IDENTITY);
    assert(
            Matrix2x2::IDENTITY.inverseForOrthogonal() * 
            Matrix2x2::IDENTITY ==
            Matrix2x2::IDENTITY);
    assert(
            Matrix3x3::IDENTITY.inverse() * 
            Matrix3x3::IDENTITY ==
            Matrix3x3::IDENTITY);
    assert(
            Matrix3x3::IDENTITY.inverseForOrthogonal() * 
            Matrix3x3::IDENTITY ==
            Matrix3x3::IDENTITY);
    assert(
            Matrix4x4::IDENTITY.inverse() * 
            Matrix4x4::IDENTITY ==
            Matrix4x4::IDENTITY);
    assert(
            Matrix4x4::IDENTITY.inverseForOrthogonal() * 
            Matrix4x4::IDENTITY ==
            Matrix4x4::IDENTITY);
}
