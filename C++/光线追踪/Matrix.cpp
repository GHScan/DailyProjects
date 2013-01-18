// vim: fileencoding=gbk
#include "pch.h"

#include <cmath>
#include <cassert>

#include "Vector.h"
#include "Matrix.h"
//----------------------------------------
// Matrix2x2
//----------------------------------------
Matrix2x2::Matrix2x2()
{
}
Matrix2x2::Matrix2x2(const float *p):
    x00(p[0]), x01(p[1]), x10(p[2]), x11(p[3])
{
}
Matrix2x2::Matrix2x2(
float _x00, float _x01,
float _x10, float _x11):
    x00(_x00), x01(_x01),
    x10(_x10), x11(_x11)
{
}

const float* Matrix2x2::operator [] (int i) const
{
    assert(i >= 0 && i < 2);
    return &x00 + i * 2;
}
float* Matrix2x2::operator [] (int i)
{
    return (float*)(*(const Matrix2x2*)this)[i];
}

const float* Matrix2x2::data() const
{
    return (*this)[0];
}
float* Matrix2x2::data()
{
    return (float*)((const Matrix2x2*)this)->data();
}

bool Matrix2x2::isOrthogonal() const
{
    const Vector2* v0 = (const Vector2*)(*this)[0];
    const Vector2* v1 = (const Vector2*)(*this)[1];
    if (!v0->isUnit()) return false;
    if (!v1->isUnit()) return false;
    if (!v0->isOrthogonal(*v1)) return false;
    return true;
}
Matrix2x2 Matrix2x2::algebraicCofactor() const
{
    return Matrix2x2(
            x11, -x10,
            -x01, x00);
}
Matrix2x2 Matrix2x2::standardAdjugate() const
{
    return algebraicCofactor().transpose();
}
float Matrix2x2::determinant() const
{
    return x00 * x11 - x01 * x10;
}
Matrix2x2 Matrix2x2::transpose() const
{
    return Matrix2x2(
            x00, x10,
            x01, x11);
}
Matrix2x2 Matrix2x2::inverse() const
{
    return standardAdjugate() / determinant();
}
Matrix2x2 Matrix2x2::inverseForOrthogonal() const
{
    assert(isOrthogonal());
    return transpose();
}

Matrix2x2& Matrix2x2::operator *= (const Matrix2x2& o)
{
    Vector2* v = (Vector2*)data();
    transform(v, v + 2, o);
    return *this;
}
Matrix2x2& Matrix2x2::operator *= (float f)
{
    x00 *= f, x01 *= f;
    x10 *= f, x11 *= f;
    return *this;
}
Matrix2x2& Matrix2x2::operator /= (float f)
{
    assert(!fequal(f, 0));
    f = 1 / f;
    return *this *= f;
}
bool Matrix2x2::operator == (const Matrix2x2& o) const
{
    return 
        fequal(x00, o.x00) && fequal(x01, o.x01) &&
        fequal(x10, o.x10) && fequal(x11, o.x11);
}
bool Matrix2x2::operator != (const Matrix2x2& o) const
{
    return !(*this == o);
}
Matrix2x2 operator * (const Matrix2x2& a, const Matrix2x2& b)
{
    return Matrix2x2(a) *= b;
}
Matrix2x2 operator * (const Matrix2x2& a, float f)
{
    return Matrix2x2(a) *= f;
}
Matrix2x2 operator / (const Matrix2x2& a, float f)
{
    return Matrix2x2(a) /= f;
}
void transform(Vector2& v, const Matrix2x2& mat)
{
    float t[2];
    t[0] = v.x * mat.x00 + v.y * mat.x10;
    t[1] = v.x * mat.x01 + v.y * mat.x11;
    v.x = t[0], v.y = t[1];
}
void transform(Vector2* begin, Vector2* end, const Matrix2x2& mat)
{
    for (; begin < end; ++begin) {
        transform(*begin, mat);
    }
}

Matrix2x2 Matrix2x2::ZERO = Matrix2x2(
        0, 0,
        0, 0);
Matrix2x2 Matrix2x2::IDENTITY = Matrix2x2(
        1, 0, 
        0, 1);


std::ostream& operator << (std::ostream& so, const Matrix2x2& o)
{
    so << '(';
    so << o.x00 << ',' << o.x01 << ',' << '\n';
    so << o.x10 << ',' << o.x11 << ')' << '\n';
    return so;
}
std::istream& operator >> (std::istream& si, Matrix2x2& o)
{
    char c;
    c = si.get(); assert(c == '(');

    si >> o.x00; c = si.get(); assert(c == ',');
    si >> o.x01; c = si.get(); assert(c == ',');

    si >> o.x10; c = si.get(); assert(c == ',');
    si >> o.x11; c = si.get(); assert(c == ')');

    return si;
}

//----------------------------------------
// Matrix3x3
//----------------------------------------

Matrix3x3::Matrix3x3()
{}
Matrix3x3::Matrix3x3(const float *p)
{
    float *d = data();
    for (int i = 0; i < 9; ++i) d[i] = p[i];
}
Matrix3x3::Matrix3x3(
        float _x00, float _x01, float _x02,
        float _x10, float _x11, float _x12,
        float _x20, float _x21, float _x22):
    x00(_x00), x01(_x01), x02(_x02),
    x10(_x10), x11(_x11), x12(_x12),
    x20(_x20), x21(_x21), x22(_x22)
{}
Matrix3x3::Matrix3x3(const Matrix2x2& m)
{
    x00 = m.x00, x01 = m.x01, x02 = 0;
    x10 = m.x10, x11 = m.x11, x12 = 0;
    x20 = 0, x21 = 0, x22 = 1;
}

const float* Matrix3x3::operator [] (int i) const
{
    assert(i >= 0 && i < 3);
    return &x00 + i * 3;
}
float* Matrix3x3::operator [] (int i)
{
    return (float*)(*(const Matrix3x3*)this)[i];
}

const float* Matrix3x3::data() const
{
    return (*this)[0];
}
float* Matrix3x3::data()
{
    return (float*)((const Matrix3x3*)this)->data();
}

bool Matrix3x3::isOrthogonal() const
{
    const Vector3* v0 = (const Vector3*)(*this)[0];
    const Vector3* v1 = (const Vector3*)(*this)[1];
    const Vector3* v2 = (const Vector3*)(*this)[2];
    if (!v0->isUnit()) return false;
    if (!v1->isUnit()) return false;
    if (!v2->isUnit()) return false;
    if (!v0->isOrthogonal(*v1)) return false;
    if (!v0->isOrthogonal(*v2)) return false;
    if (!v1->isOrthogonal(*v2)) return false;
    return true;
}
Matrix3x3 Matrix3x3::algebraicCofactor() const
{
    return Matrix3x3(
            +(x11 * x22 - x12 * x21), -(x10 * x22 - x12 * x20), +(x10 * x21 - x20 * x11),
            -(x01 * x22 - x02 * x21), +(x00 * x22 - x02 * x20), -(x00 * x21 - x01 * x20),
            +(x01 * x12 - x02 * x11), -(x00 * x12 - x02 * x10), +(x00 * x11 - x01 * x10));
}
Matrix3x3 Matrix3x3::standardAdjugate() const
{
    return algebraicCofactor().transpose();
}
float Matrix3x3::determinant() const
{
    return 
        x00 * x11 * x22 + x01 * x12 * x20 + x02 * x10 * x21 -
        x02 * x11 * x20 - x01 * x10 * x22 - x00 * x12 * x21;
}
Matrix3x3 Matrix3x3::transpose() const
{
    return Matrix3x3(
            x00, x10, x20,
            x01, x11, x21,
            x02, x12, x22);
}
Matrix3x3 Matrix3x3::inverse() const
{
    return standardAdjugate() / determinant();
}
Matrix3x3 Matrix3x3::inverseForOrthogonal() const
{
    assert(isOrthogonal());
    return transpose();
}
Matrix3x3 Matrix3x3::fromTranslate(float x, float y)
{
    return Matrix3x3(
            1, 0, 0,
            0, 1, 0,
            x, y, 1);
}
Matrix3x3 Matrix3x3::fromRotate(float degree)
{
    float radian = degree2Radian(degree);
    float cosA = cos(radian), sinA = sin(radian);
    return Matrix3x3(
            cosA, sinA, 0,
            -sinA, cosA, 0,
            0, 0, 1);
}
Matrix3x3 Matrix3x3::fromScale(float x, float y)
{
    return Matrix3x3(
            x, 0, 0,
            0, y, 0,
            0, 0, 1);
}

Matrix3x3& Matrix3x3::operator *= (const Matrix3x3& o)
{
    Vector3* v = (Vector3*)data();
    transform(v, v + 3, o);
    return *this;
}
Matrix3x3& Matrix3x3::operator *= (float f)
{
    x00 *= f, x01 *= f, x02 *= f;
    x10 *= f, x11 *= f, x12 *= f;
    x20 *= f, x21 *= f, x22 *= f;
    return *this;
}
Matrix3x3& Matrix3x3::operator /= (float f)
{
    assert(!fequal(f, 0));
    f = 1 / f;
    return *this *= f;
}
Matrix3x3 operator * (const Matrix3x3& a, const Matrix3x3& b)
{
    return Matrix3x3(a) *= b;
}
Matrix3x3 operator * (const Matrix3x3& a, float f)
{
    return Matrix3x3(a) *= f;
}
Matrix3x3 operator / (const Matrix3x3& a, float f)
{
    return Matrix3x3(a) /= f;
}
bool Matrix3x3::operator == (const Matrix3x3& o) const
{
    return 
        fequal(x00, o.x00) && fequal(x01, o.x01) && fequal(x02, o.x02) &&
        fequal(x10, o.x10) && fequal(x11, o.x11) && fequal(x12, o.x12) &&
        fequal(x20, o.x20) && fequal(x21, o.x21) && fequal(x22, o.x22);
}
bool Matrix3x3::operator != (const Matrix3x3& o) const
{
    return !(*this == o);
}
void transform(Vector3& v, const Matrix3x3& mat)
{
    float t[3];
    t[0] = v.x * mat.x00 + v.y * mat.x10 + v.z * mat.x20;
    t[1] = v.x * mat.x01 + v.y * mat.x11 + v.z * mat.x21;
    t[2] = v.x * mat.x02 + v.y * mat.x12 + v.z * mat.x22;
    v.x = t[0], v.y = t[1], v.z = t[2];
}
void transform(Vector3* begin, Vector3* end, const Matrix3x3& mat)
{
    for (; begin < end; ++begin) {
        transform(*begin, mat);
    }
}

Matrix3x3 Matrix3x3::ZERO = Matrix3x3(
        0, 0, 0,
        0, 0, 0,
        0, 0, 0);
Matrix3x3 Matrix3x3::IDENTITY = Matrix3x3(
        1, 0, 0,
        0, 1, 0,
        0, 0, 1);

std::ostream& operator << (std::ostream& so, const Matrix3x3& o)
{
    so << '(';
    so << o.x00 << ',' << o.x01 << ',' << o.x02 << ',' << '\n';
    so << o.x10 << ',' << o.x11 << ',' << o.x12 << ',' << '\n';
    so << o.x20 << ',' << o.x21 << ',' << o.x22 << ')' << '\n';
    return so;
}
std::istream& operator >> (std::istream& si, Matrix3x3& o)
{
    char c;
    c = si.get(); assert(c == '(');

    si >> o.x00; c = si.get(); assert(c == ',');
    si >> o.x01; c = si.get(); assert(c == ',');
    si >> o.x02; c = si.get(); assert(c == ',');

    si >> o.x10; c = si.get(); assert(c == ',');
    si >> o.x11; c = si.get(); assert(c == ',');
    si >> o.x12; c = si.get(); assert(c == ',');

    si >> o.x20; c = si.get(); assert(c == ',');
    si >> o.x21; c = si.get(); assert(c == ',');
    si >> o.x22; c = si.get(); assert(c == ')');

    return si;
}

//----------------------------------------
// Matrix4x4
//----------------------------------------
Matrix4x4::Matrix4x4()
{
}
Matrix4x4::Matrix4x4(const float *p)
{
    float *d = data();
    for (int i = 0; i < 16; ++i) d[i] = p[i];
}
Matrix4x4::Matrix4x4(
        float _x00, float _x01, float _x02, float _x03,
        float _x10, float _x11, float _x12, float _x13,
        float _x20, float _x21, float _x22, float _x23,
        float _x30, float _x31, float _x32, float _x33):
    x00(_x00), x01(_x01), x02(_x02), x03(_x03),
    x10(_x10), x11(_x11), x12(_x12), x13(_x13),
    x20(_x20), x21(_x21), x22(_x22), x23(_x23),
    x30(_x30), x31(_x31), x32(_x32), x33(_x33)
{}
Matrix4x4::Matrix4x4(const Matrix3x3& m):
    x00(m.x00), x01(m.x01), x02(m.x02), x03(0),
    x10(m.x10), x11(m.x11), x12(m.x12), x13(0),
    x20(m.x20), x21(m.x21), x22(m.x22), x23(0),
    x30(0), x31(0), x32(0), x33(1)
{
}

const float* Matrix4x4::operator [] (int i) const
{
    assert(i >= 0 && i < 4);
    return &x00 + i * 4;
}
float* Matrix4x4::operator [] (int i)
{
    return (float*)(*(const Matrix4x4*)this)[i];
}

const float* Matrix4x4::data() const
{
    return (*this)[0];
}
float* Matrix4x4::data()
{
    return (float*)((const Matrix4x4*)this)->data();
}

bool Matrix4x4::isOrthogonal() const
{
    const Vector4* v0 = (const Vector4*)(*this)[0];
    const Vector4* v1 = (const Vector4*)(*this)[1];
    const Vector4* v2 = (const Vector4*)(*this)[2];
    const Vector4* v3 = (const Vector4*)(*this)[3];
    if (!v0->isUnit()) return false;
    if (!v1->isUnit()) return false;
    if (!v2->isUnit()) return false;
    if (!v3->isUnit()) return false;
    if (!v0->isOrthogonal(*v1)) return false;
    if (!v0->isOrthogonal(*v2)) return false;
    if (!v0->isOrthogonal(*v3)) return false;
    if (!v1->isOrthogonal(*v2)) return false;
    if (!v1->isOrthogonal(*v3)) return false;
    if (!v2->isOrthogonal(*v3)) return false;
    return true;
}
Matrix4x4 Matrix4x4::algebraicCofactor() const
{
    float t[16];
    int off = 0;
    bool negative = false;
    for (int y = 0; y < 4; ++y) {
        for (int x = 0; x < 4; ++x) {
            float t2[9];
            int off2 = 0;
            for (int y2 = 0; y2 < 4; ++y2) {
                for (int x2 = 0; x2 < 4; ++x2) {
                    if (y2 == y || x2 == x) continue;
                    t2[off2++] = (*this)[y2][x2];
                }
            }

            t[off] = Matrix3x3(t2).determinant();
            if (negative) t[off] = -t[off];

            negative = !negative;
            ++off;
        }
        negative = !negative;
    }
    return Matrix4x4(t);
}
Matrix4x4 Matrix4x4::standardAdjugate() const
{
    return algebraicCofactor().transpose();
}
float Matrix4x4::determinant() const
{
    Matrix4x4 m = algebraicCofactor();
    return x00 * m.x00 + x01 * m.x01 + x02 * m.x02 + x03 * m.x03;
}
Matrix4x4 Matrix4x4::transpose() const
{
    return Matrix4x4(
            x00, x10, x20, x30,
            x01, x11, x21, x31,
            x02, x12, x22, x32,
            x03, x13, x23, x33);
}
Matrix4x4 Matrix4x4::inverse() const
{
    Matrix4x4 m = standardAdjugate();
    float det = x00 * m.x00 + x01 * m.x10 + x02 * m.x20 + x03 * m.x30;
    return m / det;
}
Matrix4x4 Matrix4x4::inverseForOrthogonal() const
{
    assert(isOrthogonal());
    return transpose();
}

Matrix4x4 Matrix4x4::fromTranslate(float x, float y, float z)
{
    return Matrix4x4(
            1, 0, 0, 0, 
            0, 1, 0, 0, 
            0, 0, 1, 0, 
            x, y, z, 1);
}
Matrix4x4 Matrix4x4::fromYawPitchRoll(float yaw, float pitch, float roll)
{
    Matrix4x4 m = fromRotateZ(roll);
    m *= fromRotateX(pitch);
    m *= fromRotateY(yaw);
    return m;
}
Matrix4x4 Matrix4x4::fromRotateX(float x)
{
    float radian = degree2Radian(x);
    float cosA = cos(radian), sinA = sin(radian);
    return Matrix4x4(
            1, 0, 0, 0,
            0, cosA, sinA, 0,
            0, -sinA, cosA, 0,
            0, 0, 0, 1);
}
Matrix4x4 Matrix4x4::fromRotateY(float y)
{
    float radian = degree2Radian(y);
    float cosA = cos(radian), sinA = sin(radian);
    return Matrix4x4(
            cosA, 0, -sinA, 0,
            0, 1, 0, 0,
            sinA, 0, cosA, 0,
            0, 0, 0, 1);
}
Matrix4x4 Matrix4x4::fromRotateZ(float z)
{
    float radian = degree2Radian(z);
    float cosA = cos(radian), sinA = sin(radian);
    return Matrix4x4(
            cosA, sinA, 0, 0,
            -sinA, cosA, 0, 0,
            0, 0, 1, 0,
            0, 0, 0, 1);
}
Matrix4x4 Matrix4x4::fromRotateAxis(const Vector3& dir, float degree)
{
    assert(dir.isUnit());
    float radian = degree2Radian(degree);
    float cosA = cos(radian), sinA = sin(radian);
    float x = dir.x, y = dir.y, z = dir.z;
    float _1subCosA = 1 - cosA;
    float x2 = x * x, y2 = y * y, z2 = z * z;
    float xy = x * y, xz = x * z, yz = y * z;

    return Matrix4x4(
            x2 * _1subCosA + cosA, xy * _1subCosA + z * sinA, xz * _1subCosA - y * sinA, 0, 
            xy * _1subCosA - z * sinA, y2 * _1subCosA + cosA, yz * _1subCosA + x * sinA, 0, 
            xz * _1subCosA + y * sinA, yz * _1subCosA - x * sinA, z2 * _1subCosA + cosA, 0, 
            0, 0, 0, 1);
}
Matrix4x4 Matrix4x4::fromScale(float x, float y, float z)
{
    return Matrix4x4(
            x, 0, 0, 0, 
            0, y, 0, 0, 
            0, 0, z, 0, 
            0, 0, 0, 1);
}
Matrix4x4 Matrix4x4::fromPerspectiveProj(
        float fovY, float aspect, float nearZ, float farZ)
{
    float radian = degree2Radian(fovY);
    float yScale = 1 / tan(radian / 2);
    float xScale = yScale / aspect;
    float zDiff = farZ - nearZ;
    return Matrix4x4(
            xScale, 0, 0, 0, 
            0, yScale, 0, 0, 
            0, 0, farZ / zDiff, 1, 
            0, 0, -nearZ * farZ / zDiff, 0);
}
Matrix4x4 Matrix4x4::fromPerspectiveOffProj(
        float l, float r, float b, float t, float nearZ, float farZ)
{
    float xDiff = r - l, yDiff = t - b, zDiff = farZ - nearZ;
    float xSum = r + l, ySum = t + b;
    float zMul = nearZ * farZ;
    return Matrix4x4(
            2 * nearZ / xDiff, 0, 0, 0, 
            0, 2 * nearZ / yDiff, 0, 0, 
            xSum / -xDiff, ySum / -yDiff, farZ / zDiff, 1, 
            0, 0, zMul / -zDiff, 0);
}
Matrix4x4 Matrix4x4::fromOrthoProj(float w, float h, float nearZ, float farZ)
{
    float zDiff = farZ - nearZ;
    return Matrix4x4(
            2 / w, 0, 0, 0,
            0, 2 / h, 0, 0,
            0, 0, 1 / zDiff, 0,
            0, 0, -nearZ / zDiff, 1);
}
Matrix4x4 Matrix4x4::fromOrthoOffProj(
        float l, float r, float b, float t, float nearZ, float farZ)
{
    float xDiff = r - l, yDiff = t - b, zDiff = farZ - nearZ;
    float xSum = r + l, ySum = t + b;
    return Matrix4x4(
            2 / xDiff, 0, 0, 0, 
            0, 2 / yDiff, 0, 0, 
            0, 0, 1 / zDiff, 0, 
            xSum / -xDiff, ySum / -yDiff, nearZ / -zDiff, l);
}
Matrix4x4 Matrix4x4::fromReflect(const Vector3& planeNorm, float d)
{
    Vector3 norm = planeNorm.normalize();
    float x = norm.x, y = norm.y, z = norm.z, w = -d;
    float xx = x * x * -2, xy = x * y * -2, xz = x * z * -2;
    float yy = y * y * -2, yz = y * z * -2;
    float zz = z * z * -2;
    float xw = x * w * -2, yw = y * w * -2, zw = z * w * -2;
    return Matrix4x4(
            xx + 1, xy, xz, 0, 
            xy, yy + 1, yz, 0, 
            xz, yz, zz + 1, 0, 
            xw, yw, zw, 1);
}

Matrix4x4& Matrix4x4::operator *= (const Matrix4x4& o)
{
    Vector4* v = (Vector4*)data();
    transform(v, v + 4, o);
    return *this;
}
Matrix4x4& Matrix4x4::operator *= (float f)
{
    x00 *= f, x01 *=f, x02 *= f, x03 *= f;
    x10 *= f, x11 *=f, x12 *= f, x13 *= f;
    x20 *= f, x21 *=f, x22 *= f, x23 *= f;
    x30 *= f, x31 *=f, x32 *= f, x33 *= f;
    return *this;
}
Matrix4x4& Matrix4x4::operator /= (float f)
{
    assert(!fequal(f, 0));
    f = 1 / f;
    return *this *= f;
}
bool Matrix4x4::operator == (const Matrix4x4& o) const
{
    return 
        fequal(x00, o.x00) && fequal(x01, o.x01) && fequal(x02, o.x02) && fequal(x03, o.x03) && 
        fequal(x10, o.x10) && fequal(x11, o.x11) && fequal(x12, o.x12) && fequal(x13, o.x13) && 
        fequal(x20, o.x20) && fequal(x21, o.x21) && fequal(x22, o.x22) && fequal(x23, o.x23) && 
        fequal(x30, o.x30) && fequal(x31, o.x31) && fequal(x32, o.x32) && fequal(x33, o.x33);
}
bool Matrix4x4::operator != (const Matrix4x4& o) const
{
    return !(*this == o);
}
Matrix4x4 operator * (const Matrix4x4& a, const Matrix4x4& b)
{
    return Matrix4x4(a) *= b;
}
Matrix4x4 operator * (const Matrix4x4& a, float f)
{
    return Matrix4x4(a) *= f;
}
Matrix4x4 operator / (const Matrix4x4& a, float f)
{
    return Matrix4x4(a) /= f;
}
void transform(Vector4& v, const Matrix4x4& mat)
{
    float t[4];
    t[0] = v.x * mat.x00 + v.y * mat.x10 + v.z * mat.x20 + v.w * mat.x30;
    t[1] = v.x * mat.x01 + v.y * mat.x11 + v.z * mat.x21 + v.w * mat.x31;
    t[2] = v.x * mat.x02 + v.y * mat.x12 + v.z * mat.x22 + v.w * mat.x32;
    t[3] = v.x * mat.x03 + v.y * mat.x13 + v.z * mat.x23 + v.w * mat.x33;
    v.x = t[0], v.y = t[1], v.z = t[2], v.w = t[3];
}
void transform(Vector4* begin, Vector4* end, const Matrix4x4& mat)
{
    for (; begin < end; ++begin) {
        transform(*begin, mat);
    }
}

Matrix4x4 Matrix4x4::ZERO = Matrix4x4(
        0, 0, 0, 0, 
        0, 0, 0, 0, 
        0, 0, 0, 0, 
        0, 0, 0, 0);
Matrix4x4 Matrix4x4::IDENTITY = Matrix4x4(
        1, 0, 0, 0, 
        0, 1, 0, 0, 
        0, 0, 1, 0, 
        0, 0, 0, 1);

std::ostream& operator << (std::ostream& so, const Matrix4x4& o)
{
    so << '(';
    so << o.x00 << ',' << o.x01 << ',' << o.x02 << ',' << o.x03 << ',' << '\n';
    so << o.x10 << ',' << o.x11 << ',' << o.x12 << ',' << o.x13 << ',' << '\n';
    so << o.x20 << ',' << o.x21 << ',' << o.x22 << ',' << o.x23 << ',' << '\n';
    so << o.x30 << ',' << o.x31 << ',' << o.x32 << ',' << o.x33 << ')' << '\n';
   return so;
}
std::istream& operator >> (std::istream& si, Matrix4x4& o)
{
    char c;
    c = si.get(); assert(c == '(');

    si >> o.x00; c = si.get(); assert(c == ',');
    si >> o.x01; c = si.get(); assert(c == ',');
    si >> o.x02; c = si.get(); assert(c == ',');
    si >> o.x03; c = si.get(); assert(c == ',');

    si >> o.x10; c = si.get(); assert(c == ',');
    si >> o.x11; c = si.get(); assert(c == ',');
    si >> o.x12; c = si.get(); assert(c == ',');
    si >> o.x13; c = si.get(); assert(c == ',');

    si >> o.x20; c = si.get(); assert(c == ',');
    si >> o.x21; c = si.get(); assert(c == ',');
    si >> o.x22; c = si.get(); assert(c == ',');
    si >> o.x23; c = si.get(); assert(c == ',');

    si >> o.x30; c = si.get(); assert(c == ',');
    si >> o.x31; c = si.get(); assert(c == ',');
    si >> o.x32; c = si.get(); assert(c == ',');
    si >> o.x33; c = si.get(); assert(c == ')');

    return si;
}

void transformPoint(Vector3& pt, const Matrix4x4& mat)
{
    Vector4 v4(pt, 1);
    transform(v4, mat);
    assert(fequal(v4.w, 1));
    pt = (Vector3&)v4;
}

void transformDirection(Vector3& dir, const Matrix4x4& mat)
{
    assert(dir.isUnit());
    Vector4 v4(dir, 0);
    transform(v4, mat);
    assert(fequal(v4.w, 0));
    dir = (Vector3&)v4;
    dir /= dir.length();
}
