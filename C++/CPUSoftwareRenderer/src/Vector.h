#ifndef VECTOR_H
#define VECTOR_H


#include <cmath>
#include <cctype>

#include <iostream>
#include <initializer_list>

#include "Util.h"


template<int dim>
struct Vector final
{
    float Val[dim];

    Vector()
    {
    }

    explicit Vector(float v)
    {
        std::fill(Val, Val + dim, v);
    }

    Vector(float x, float y)
    {
        static_assert(dim == 2, "");
        Val[0] = x;
        Val[1] = y;
    }

    Vector(float x, float y, float z)
    {
        static_assert(dim == 3, "");
        Val[0] = x;
        Val[1] = y;
        Val[2] = z;
    }

    Vector(std::initializer_list<float> vs)
    {
        std::copy(vs.begin(), vs.end(), Val);
    }

    template<int dim2>
    Vector(Vector<dim2> const &other, float def)
    {
        static_assert(dim2 < dim, "");

        int i = 0;
        for (; i < dim2; ++i)
            Val[i] = other.Val[i];
        for (; i < dim; ++i)
            Val[i] = def;
    }

    float Dot(Vector const &other) const
    {
        float res = 0;
        for (int i = 0; i < dim; ++i)
            res += Val[i] * other.Val[i];
        return res;
    }

    Vector Normalize() const
    {
        float invLen = 1 / std::sqrt(Dot(*this));

        Vector res;
        for (int i = 0; i < dim; ++i)
            res.Val[i] = Val[i] * invLen;
        return res;
    }

    Vector Cross(Vector const &other) const
    {
        static_assert(dim == 3, "");
        return Vector
        {
            Val[1] * other.Val[2] - Val[2] * other.Val[1],
            Val[2] * other.Val[0] - Val[0] * other.Val[2],
            Val[0] * other.Val[1] - Val[1] * other.Val[0],
        };
    }

    template<int dim2>
    Vector<dim2> Proj() const
    {
        static_assert(dim2 < dim, "");

        Vector<dim2> res;
        for (int i = 0; i < dim2; ++i)
            res.Val[i] = Val[i];
        return res;
    }
};


template<int dim>
inline Vector<dim> operator + (Vector<dim> const &a, Vector<dim> const &b)
{
    Vector<dim> res;
    for (int i = 0; i < dim; ++i)
        res.Val[i] = a.Val[i] + b.Val[i];
    return res;
}

template<int dim>
inline Vector<dim> operator - (Vector<dim> const &a, Vector<dim> const &b)
{
    Vector<dim> res;
    for (int i = 0; i < dim; ++i)
        res.Val[i] = a.Val[i] - b.Val[i];
    return res;
}

template<int dim>
inline Vector<dim> operator * (Vector<dim> const &a, Vector<dim> const &b)
{
    Vector<dim> res;
    for (int i = 0; i < dim; ++i)
        res.Val[i] = a.Val[i] * b.Val[i];
    return res;
}

template<int dim>
inline Vector<dim> operator * (Vector<dim> const &a, float b)
{
    Vector<dim> res;
    for (int i = 0; i < dim; ++i)
        res.Val[i] = a.Val[i] * b;
    return res;
}

template<int dim>
inline Vector<dim> operator + (Vector<dim> const &a, float b)
{
    Vector<dim> res;
    for (int i = 0; i < dim; ++i)
        res.Val[i] = a.Val[i] + b;
    return res;
}

// Vector4 
template<>
struct Vector<4> final
{
    union
    {
        float Val[4];
        __m128 SIMD;
    };

    Vector()
    {
    }

    explicit Vector(__m128 simd)
        : SIMD(simd)
    {
    }

    explicit Vector(float v)
        : SIMD(_mm_set1_ps(v))
    {
    }

    Vector(float x, float y, float z, float w)
        : SIMD(_mm_set_ps(w, z, y, x))
    {
    }

    template<int dim>
    Vector(Vector<dim> const &other, float def)
    {
        static_assert(dim < 4, "");

        int i = 0;
        for (; i < dim; ++i)
            Val[i] = other.Val[i];
        for (; i < 4; ++i)
            Val[i] = def;
    }

    template<int dim>
    Vector<dim> Proj() const
    {
        static_assert(dim < 4, "");

        Vector<dim> res;
        for (int i = 0; i < dim; ++i)
            res.Val[i] = Val[i];
        return res;
    }

    float Dot(Vector const &other) const
    {
        __m128 r1 = _mm_mul_ps(SIMD, other.SIMD);
        __m128 shuf = _mm_shuffle_ps(r1, r1, _MM_SHUFFLE(2, 3, 0, 1));
        __m128 sums = _mm_add_ps(r1, shuf);
        shuf = _mm_movehl_ps(shuf, sums);
        sums = _mm_add_ss(sums, shuf);
        return _mm_cvtss_f32(sums);
    }

    Vector Normalize() const
    {
        __m128 r1 = _mm_mul_ps(SIMD, SIMD);
        __m128 shuf = _mm_shuffle_ps(r1, r1, _MM_SHUFFLE(2, 3, 0, 1));
        __m128 sums = _mm_add_ps(r1, shuf);
        shuf = _mm_movehl_ps(shuf, sums);
        sums = _mm_add_ss(sums, shuf);
        sums = _mm_broadcastss_ps(sums);
        return Vector(_mm_mul_ps(SIMD, _mm_rsqrt_ps(sums)));
    }

    Vector Cross(Vector const &other) const
    {
        __m128 m0 = _mm_mul_ps(
            _mm_shuffle_ps(SIMD, SIMD, _MM_SHUFFLE(3, 0, 2, 1)),
            _mm_shuffle_ps(other.SIMD, other.SIMD, _MM_SHUFFLE(3, 1, 0, 2)));
        __m128 m1 = _mm_mul_ps(
            _mm_shuffle_ps(SIMD, SIMD, _MM_SHUFFLE(3, 1, 0, 2)), _mm_shuffle_ps
            (other.SIMD, other.SIMD, _MM_SHUFFLE(3, 0, 2, 1)));
        return Vector(_mm_sub_ps(m0, m1));
    }
};


inline Vector<4> operator + (Vector<4> const &a, Vector<4> const &b)
{
    Vector<4> res;
    res.SIMD = _mm_add_ps(a.SIMD, b.SIMD);
    return res;
}

inline Vector<4> operator - (Vector<4> const &a, Vector<4> const &b)
{
    Vector<4> res;
    res.SIMD = _mm_sub_ps(a.SIMD, b.SIMD);
    return res;
}

inline Vector<4> operator * (Vector<4> const &a, Vector<4> const &b)
{
    Vector<4> res;
    res.SIMD = _mm_mul_ps(a.SIMD, b.SIMD);
    return res;
}

inline Vector<4> operator * (Vector<4> const &a, float b)
{
    Vector<4> res;
    res.SIMD = _mm_mul_ps(a.SIMD, _mm_set1_ps(b));
    return res;
}

inline Vector<4> operator + (Vector<4> const &a, float b)
{
    Vector<4> res;
    res.SIMD = _mm_add_ps(a.SIMD, _mm_set1_ps(b));
    return res;
}



template<int dim>
inline std::ostream& operator << (std::ostream& out, Vector<dim> const &vec)
{
    for (int i = 0; i < dim; ++i)
    {
        if (i > 0)
            out << ",";
        out << vec.Val[i];
    }
    return out;
}
template<int dim>
inline std::istream& operator >> (std::istream& in, Vector<dim> &vec)
{
    for (int i = 0; i < dim; ++i)
    {
        if (i > 0)
            RAssert(Read<char>(in) == ',');
        in >> vec.Val[i];
    }
    return in;
}


template<int dim>
inline Vector<dim> Reflect(Vector<dim> const &invIn, Vector<dim> const &norm)
{
    auto y = norm * invIn.Dot(norm);
    auto x = invIn - y;
    return y - x;
}


using Vector2 = Vector<2>;
using Vector3 = Vector<3>;
using Vector4 = Vector<4>;



#endif
