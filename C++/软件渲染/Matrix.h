// vim: fileencoding=gbk

#ifndef MATRIX_H
#define MATRIX_H

#include <iostream>

struct Vector2;
struct Vector3;
struct Vector4;
//----------------------------------------
// Matrix2x2
//----------------------------------------
struct Matrix2x2
{
    float x00, x01;
    float x10, x11;

    Matrix2x2();
    explicit Matrix2x2(const float *p);
    Matrix2x2(
    float _x00, float _x01,
    float _x10, float _x11);

    const float* operator [] (int i) const;
    float* operator [] (int i);

    const float* data() const;
    float* data();

    bool isOrthogonal() const;             // 正交性
    Matrix2x2 algebraicCofactor() const;    // 代数余子式
    Matrix2x2 standardAdjugate() const;     // 标准伴随矩阵
    float determinant() const;              // 行列式
    Matrix2x2 transpose() const;            // 转置
    Matrix2x2 inverse() const;              // 逆
    Matrix2x2 inverseForOrthogonal() const; // 对正交矩阵求逆

    Matrix2x2& operator *= (const Matrix2x2& o);
    Matrix2x2& operator *= (float f);
    Matrix2x2& operator /= (float f);
    bool operator == (const Matrix2x2& o) const;
    bool operator != (const Matrix2x2& o) const;

    static Matrix2x2 ZERO;
    static Matrix2x2 IDENTITY;
};
Matrix2x2 operator * (const Matrix2x2& a, const Matrix2x2& b);
Matrix2x2 operator * (const Matrix2x2& a, float f);
Matrix2x2 operator / (const Matrix2x2& a, float f);
void transform(Vector2& v, const Matrix2x2& mat);
void transform(Vector2* begin, Vector2* end, const Matrix2x2& mat);

std::ostream& operator << (std::ostream& so, const Matrix2x2& o);
std::istream& operator >> (std::istream& si, Matrix2x2& o);

//----------------------------------------
// Matrix3x3
//----------------------------------------
struct Matrix3x3
{
    float x00, x01, x02;
    float x10, x11, x12;
    float x20, x21, x22;

    Matrix3x3();
    explicit Matrix3x3(const float *p);
    Matrix3x3(
    float _x00, float _x01, float _x02,
    float _x10, float _x11, float _x12,
    float _x20, float _x21, float _x22);
    Matrix3x3(const Matrix2x2& m);

    const float* operator [] (int i) const;
    float* operator [] (int i);

    const float* data() const;
    float* data();

    bool isOrthogonal() const;             // 正交性
    Matrix3x3 algebraicCofactor() const;    // 代数余子式
    Matrix3x3 standardAdjugate() const;     // 标准伴随矩阵
    float determinant() const;              // 行列式
    Matrix3x3 transpose() const;            // 转置
    Matrix3x3 inverse() const;              // 逆
    Matrix3x3 inverseForOrthogonal() const; // 对正交矩阵求逆

    static Matrix3x3 fromTranslate(float x, float y);
    static Matrix3x3 fromRotate(float degree);
    static Matrix3x3 fromScale(float x, float y);

    Matrix3x3& operator *= (const Matrix3x3& o);
    Matrix3x3& operator *= (float f);
    Matrix3x3& operator /= (float f);
    bool operator == (const Matrix3x3& o) const;
    bool operator != (const Matrix3x3& o) const;

    static Matrix3x3 ZERO;
    static Matrix3x3 IDENTITY;
};
Matrix3x3 operator * (const Matrix3x3& a, const Matrix3x3& b);
Matrix3x3 operator * (const Matrix3x3& a, float f);
Matrix3x3 operator / (const Matrix3x3& a, float f);
void transform(Vector3& v, const Matrix3x3& mat);
void transform(Vector3* begin, Vector3* end, const Matrix3x3& mat);

std::ostream& operator << (std::ostream& so, const Matrix3x3& o);
std::istream& operator >> (std::istream& si, Matrix3x3& o);
//----------------------------------------
// Matrix4x4
//----------------------------------------
struct Matrix4x4
{
    float x00, x01, x02, x03;
    float x10, x11, x12, x13;
    float x20, x21, x22, x23;
    float x30, x31, x32, x33;

    Matrix4x4();
    explicit Matrix4x4(const float *p);
    Matrix4x4(
    float _x00, float _x01, float _x02, float _x03,
    float _x10, float _x11, float _x12, float _x13,
    float _x20, float _x21, float _x22, float _x23,
    float _x30, float _x31, float _x32, float _x33);
    Matrix4x4(const Matrix3x3& m);

    const float* operator [] (int i) const;
    float* operator [] (int i);

    const float* data() const;
    float* data();

    bool isOrthogonal() const;             // 正交性
    Matrix4x4 algebraicCofactor() const;    // 代数余子式
    Matrix4x4 standardAdjugate() const;     // 标准伴随矩阵
    float determinant() const;              // 行列式
    Matrix4x4 transpose() const;            // 转置
    Matrix4x4 inverse() const;              // 逆
    Matrix4x4 inverseForOrthogonal() const; // 对正交矩阵求逆

    static Matrix4x4 fromTranslate(float x, float y, float z);
    static Matrix4x4 fromYawPitchRoll(float yaw, float pitch, float roll);
    static Matrix4x4 fromRotateX(float x);
    static Matrix4x4 fromRotateY(float y);
    static Matrix4x4 fromRotateZ(float z);
    static Matrix4x4 fromRotateAxis(const Vector3& dir, float degree);
    static Matrix4x4 fromScale(float x, float y, float z);
    static Matrix4x4 fromPerspectiveProj(
            float fovY, float aspect, float nearZ, float farZ);
    static Matrix4x4 fromPerspectiveOffProj(
            float l, float r, float b, float t, float nearZ, float farZ);
    static Matrix4x4 fromOrthoProj(float w, float h, float nearZ, float farZ);
    static Matrix4x4 fromOrthoOffProj(
            float l, float r, float b, float t, float nearZ, float farZ);
    static Matrix4x4 fromReflect(const Vector3& planeNorm, float d);

    Matrix4x4& operator *= (const Matrix4x4& o);
    Matrix4x4& operator *= (float f);
    Matrix4x4& operator /= (float f);
    bool operator == (const Matrix4x4& o) const;
    bool operator != (const Matrix4x4& o) const;

    static Matrix4x4 ZERO;
    static Matrix4x4 IDENTITY;
};
Matrix4x4 operator * (const Matrix4x4& a, const Matrix4x4& b);
Matrix4x4 operator * (const Matrix4x4& a, float f);
Matrix4x4 operator / (const Matrix4x4& a, float f);
void transform(Vector4& v, const Matrix4x4& mat);
void transform(Vector4* begin, Vector4* end, const Matrix4x4& mat);

std::ostream& operator << (std::ostream& so, const Matrix4x4& o);
std::istream& operator >> (std::istream& si, Matrix4x4& o);

#endif // #ifndef MATRIX_H
