// vim: fileencoding=gbk

#ifndef VECTOR_H
#define VECTOR_H

#include <cassert>
#include <cmath>

#include <iostream>

#include "Util.h"

//----------------------------------------
// Vector2
//----------------------------------------

struct Vector2
{
    float x, y;

    Vector2(){}
    explicit Vector2(float val): x(val), y(val){}
    Vector2(float _x, float _y):x(_x), y(_y){}
    explicit Vector2(const float *p): x(p[0]), y(p[1]){}

    const float& operator [] (int i) const 
    {
        assert(i >= 0 && i < 2);
        return (&x)[i];
    }
    float& operator [] (int i) { return (float&)(*(const Vector2*)this)[i]; }

    const float* data() const { return &x;}
    float* data() { return &x; }

    float length() const { return sqrt(lengthSqr());}
    float lengthSqr() const { return x * x + y * y; }
    Vector2 normalize() const
    {
        float len = length();
        if (!fequal(len, 0)) {
            float invLen = 1 / len;
            return Vector2(x * invLen, y * invLen);
        }
        return *this;
    }

    float dotProduct(const Vector2& o) const
    {
        return x * o.x + y * o.y;
    }

    Vector2& operator += (const Vector2& o) 
    {
        x += o.x, y += o.y; return *this;
    }
    Vector2& operator -= (const Vector2& o) 
    {
        x -= o.x, y -= o.y; return *this;
    }
    Vector2& operator *= (float f)
    {
        x *= f, y *= f; return *this;
    }
    Vector2& operator /= (float f)
    {
        assert(!fequal(f, 0));
        f = 1 / f;
        x *= f, y *= f; return *this;
    }
    Vector2 operator - () const
    {
        return Vector2(-x, -y);
    }
    bool operator == (const Vector2& o) const
    {
        return fequal(x, o.x) && fequal(y, o.y);
    }
    bool operator != (const Vector2& o) const
    {
        return !(*this == o);
    }
    Vector2& multiplyInplace(const Vector2& o)
    {
        x *= o.x, y *= o.y;
        return *this;
    }
    Vector2 multiply(const Vector2& o) const
    {
        return Vector2(*this).multiplyInplace(o);
    }

    bool isUnit() const { return fequal(lengthSqr(), 1); }
    bool isOrthogonal(const Vector2& o) const { return fequal(dotProduct(o), 0); }

    static Vector2 ZERO;
    static Vector2 UNIT_SCALE;
};
inline Vector2 operator + (const Vector2& a, const Vector2& b)
{
    return Vector2(a) += b;
}
inline Vector2 operator - (const Vector2& a, const Vector2& b)
{
    return Vector2(a) -= b;
}
inline Vector2 operator * (const Vector2& a, float f)
{
    return Vector2(a) *= f;
}
inline Vector2 operator / (const Vector2& a, float f)
{
    return Vector2(a) /= f;
}

std::ostream& operator << (std::ostream& so, const Vector2& o);
std::istream& operator >> (std::istream& si, Vector2& o);

//----------------------------------------
// Vector3
//----------------------------------------
struct Vector3
{
    float x, y, z;

    Vector3(){}
    explicit Vector3(float val): x(val), y(val), z(val){}
    Vector3(float _x, float _y, float _z):x(_x), y(_y), z(_z){}
    explicit Vector3(const float *p): x(p[0]), y(p[1]), z(p[2]){}
    explicit Vector3(const Vector2& v, float _z = 1): x(v.x), y(v.y), z(_z){}

    const float& operator [] (int i) const 
    {
        assert(i >= 0 && i < 3);
        return (&x)[i];
    }
    float& operator [] (int i) { return (float&)(*(const Vector3*)this)[i]; }

    const float* data() const { return &x;}
    float* data() { return &x; }

    float length() const { return sqrt(lengthSqr());}
    float lengthSqr() const { return x * x + y * y + z * z; }
    Vector3 normalize() const
    {
        float len = length();
        if (!fequal(len, 0)) {
            float invLen = 1 / len;
            return Vector3(x * invLen, y * invLen, z * invLen);
        }
        return *this;
    }

    Vector3 crossProduct(const Vector3& o) const
    {
        return Vector3(
                y * o.z - z * o.y,
                z * o.x - x * o.z, 
                x * o.y - y * o.x);
    }
    float dotProduct(const Vector3& o) const
    {
        return x * o.x + y * o.y + z * o.z;
    }
    Vector3& multiplyInplace(const Vector3& o)
    {
        x *= o.x, y *= o.y, z *= o.z;
        return *this;
    }
    Vector3 multiply(const Vector3& o) const
    {
        return Vector3(*this).multiplyInplace(o);
    }

    Vector3& operator += (const Vector3& o) 
    {
        x += o.x, y += o.y, z += o.z; return *this;
    }
    Vector3& operator -= (const Vector3& o) 
    {
        x -= o.x, y -= o.y, z -= o.z; return *this;
    }
    Vector3& operator *= (float f)
    {
        x *= f, y *= f, z *= f; return *this;
    }
    Vector3& operator /= (float f)
    {
        assert(!fequal(f, 0));
        f = 1 / f;
        x *= f, y *= f, z *= f; return *this;
    }
    Vector3 operator - () const
    {
        return Vector3(-x, -y, -z);
    }
    bool operator == (const Vector3& o) const
    {
        return fequal(x, o.x) && fequal(y, o.y) && fequal(z, o.z);
    }
    bool operator != (const Vector3& o) const
    {
        return !(*this == o);
    }

    Vector2 divZ() const 
    {
        if (fequal(z, 0)) return Vector2(x, y);
        float invZ = 1 / z;
        return Vector2(x * invZ, y * invZ);
    }

    bool isUnit() const { return fequal(lengthSqr(), 1); }
    bool isOrthogonal(const Vector3& o) const { return fequal(dotProduct(o), 0); }

    static Vector3 ZERO;
    static Vector3 UNIT_SCALE;
    static Vector3 AXIS_X;
    static Vector3 AXIS_Y;
    static Vector3 AXIS_Z;
    static Vector3 AXIS_NEGATIVE_X;
    static Vector3 AXIS_NEGATIVE_Y;
    static Vector3 AXIS_NEGATIVE_Z;
};
inline Vector3 operator + (const Vector3& a, const Vector3& b)
{
    return Vector3(a) += b;
}
inline Vector3 operator - (const Vector3& a, const Vector3& b)
{
    return Vector3(a) -= b;
}
inline Vector3 operator * (const Vector3& a, float f)
{
    return Vector3(a) *= f;
}
inline Vector3 operator / (const Vector3& a, float f)
{
    return Vector3(a) /= f;
}


std::ostream& operator << (std::ostream& so, const Vector3& o);
std::istream& operator >> (std::istream& si, Vector3& o);
//----------------------------------------
// Vector4
//----------------------------------------
struct Vector4
{
    float x, y, z, w;

    Vector4(){}
    explicit Vector4(float val): x(val), y(val), z(val), w(val){}
    Vector4(float _x, float _y, float _z, float _w):x(_x), y(_y), z(_z), w(_w){}
    explicit Vector4(const float *p): x(p[0]), y(p[1]), z(p[2]), w(p[3]){}
    explicit Vector4(const Vector3& v, float _w = 1): x(v.x), y(v.y), z(v.z), w(_w){}

    const float& operator [] (int i) const 
    {
        assert(i >= 0 && i < 4);
        return (&x)[i];
    }
    float& operator [] (int i) { return (float&)(*(const Vector4*)this)[i]; }

    const float* data() const { return &x;}
    float* data() { return &x; }

    float length() const { return sqrt(lengthSqr());}
    float lengthSqr() const { return x * x + y * y + z * z + w * w; }
    Vector4 normalize() const
    {
        float len = length();
        if (!fequal(len, 0)) {
            float invLen = 1 / len;
            return Vector4(x * invLen, y * invLen, z * invLen, w * invLen);
        }
        return *this;
    }

    float dotProduct(const Vector4& o) const
    {
        return x * o.x + y * o.y + z * o.z + w * o.w;
    }
    Vector4& multiplyInplace(const Vector4& o)
    {
        x *= o.x, y *= o.y, z *= o.z, w *= o.w;
        return *this;
    }
    Vector4 multiply(const Vector4& o) const
    {
        return Vector4(*this).multiplyInplace(o);
    }

    Vector4& operator += (const Vector4& o) 
    {
        x += o.x, y += o.y, z += o.z, w += o.w; return *this;
    }
    Vector4& operator -= (const Vector4& o) 
    {
        x -= o.x, y -= o.y, z -= o.z, w -= o.w; return *this;
    }
    Vector4& operator *= (float f)
    {
        x *= f, y *= f, z *= f, w *= f; return *this;
    }
    Vector4& operator /= (float f)
    {
        assert(!fequal(f, 0));
        f = 1 / f;
        x *= f, y *= f, z *= f, w *= f; return *this;
    }
    Vector4 operator - () const
    {
        return Vector4(-x, -y, -z, -w);
    }
    bool operator == (const Vector4& o) const
    {
        return fequal(x, o.x) && fequal(y, o.y) && fequal(z, o.z) && fequal(w, o.w);
    }
    bool operator != (const Vector4& o) const
    {
        return !(*this == o);
    }

    Vector3 divW() const
    {
        if (fequal(w, 0)) return Vector3(x, y, z);
        float invW = 1 / w;
        return Vector3(x * invW, y * invW, z * invW);
    }

    bool isUnit() const { return fequal(lengthSqr(), 1); }
    bool isOrthogonal(const Vector4& o) const { return fequal(dotProduct(o), 0); }

    static Vector4 ZERO;
    static Vector4 UNIT_SCALE;
};
inline Vector4 operator + (const Vector4& a, const Vector4& b)
{
    return Vector4(a) += b;
}
inline Vector4 operator - (const Vector4& a, const Vector4& b)
{
    return Vector4(a) -= b;
}
inline Vector4 operator * (const Vector4& a, float f)
{
    return Vector4(a) *= f;
}
inline Vector4 operator / (const Vector4& a, float f)
{
    return Vector4(a) /= f;
}

std::ostream& operator << (std::ostream& so, const Vector4& o);
std::istream& operator >> (std::istream& si, Vector4& o);
#endif // #ifndef VECTOR_H
