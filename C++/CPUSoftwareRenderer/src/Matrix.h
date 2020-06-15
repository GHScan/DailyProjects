#ifndef MATRIX_H
#define MATRIX_H


#include <iostream>

#include <immintrin.h>

#include "Utils.h"
#include "Math.h"
#include "Vector.h"


template<int m, int n>
struct Matrix
{
    float Val[m][n];

    Matrix<n, m> Transpose() const
    {
        Matrix<n, m> res;

        for (int n0 = 0; n0 < n; ++n0)
        {
            for (int m0 = 0; m0 < m; ++m0)
            {
                res.Val[n0][m0] = Val[m0][n0];
            }
        }

        return res;
    }

    static Matrix Identity()
    {
        static_assert(m == n, "");

        Matrix res = { 0 };
        for (int i = 0; i < m; ++i)
            res.Val[i][i] = 1.f;

        return res;
    }

    Vector<n> Row(int m0) const
    {
        return Row_(m0, Detail::Integer<n>{});
    }
    Vector<2> Row_(int m0, Detail::Integer<2>) const
    {
        return Vector<2>(Val[m0][0], Val[m0][1]);
    }
    Vector<4> Row_(int m0, Detail::Integer<4>) const
    {
        return Vector<4>(_mm_loadu_ps(Val[m0]));
    }


    Matrix Inverse() const
    {
        static_assert(m == n, "");
        return Inverse_(Detail::Integer<m>{});
    }

    Matrix Inverse_(Detail::Integer<2>) const
    {
        Matrix res;

        float const * m_ = &Val[0][0];
        float *invOut = &res.Val[0][0];

        float det = m_[0] * m_[3] - m_[1] * m_[2];
        float invDet = 1 / det;

        invOut[0] = m_[3] * invDet;
        invOut[1] = -m_[1] * invDet;
        invOut[2] = -m_[2] * invDet;
        invOut[3] = m_[0] * invDet;

        return res;
    }
};


template<>
struct Matrix<4, 4>
{
    float Val[4][4];

    Matrix Transpose() const
    {
        Matrix res;

        for (int n0 = 0; n0 < 4; ++n0)
        {
            for (int m0 = 0; m0 < 4; ++m0)
            {
                res.Val[n0][m0] = Val[m0][n0];
            }
        }

        return res;
    }

    static Matrix Identity()
    {
        Matrix res = { 0 };
        for (int i = 0; i < 4; ++i)
            res.Val[i][i] = 1.f;

        return res;
    }

    Matrix Inverse() const
    {
        Matrix res;

        float const *m_ = &Val[0][0];
        float *invOut = &res.Val[0][0];
        float inv[16];

        inv[0] = m_[5] * m_[10] * m_[15] -
            m_[5] * m_[11] * m_[14] -
            m_[9] * m_[6] * m_[15] +
            m_[9] * m_[7] * m_[14] +
            m_[13] * m_[6] * m_[11] -
            m_[13] * m_[7] * m_[10];

        inv[4] = -m_[4] * m_[10] * m_[15] +
            m_[4] * m_[11] * m_[14] +
            m_[8] * m_[6] * m_[15] -
            m_[8] * m_[7] * m_[14] -
            m_[12] * m_[6] * m_[11] +
            m_[12] * m_[7] * m_[10];

        inv[8] = m_[4] * m_[9] * m_[15] -
            m_[4] * m_[11] * m_[13] -
            m_[8] * m_[5] * m_[15] +
            m_[8] * m_[7] * m_[13] +
            m_[12] * m_[5] * m_[11] -
            m_[12] * m_[7] * m_[9];

        inv[12] = -m_[4] * m_[9] * m_[14] +
            m_[4] * m_[10] * m_[13] +
            m_[8] * m_[5] * m_[14] -
            m_[8] * m_[6] * m_[13] -
            m_[12] * m_[5] * m_[10] +
            m_[12] * m_[6] * m_[9];

        inv[1] = -m_[1] * m_[10] * m_[15] +
            m_[1] * m_[11] * m_[14] +
            m_[9] * m_[2] * m_[15] -
            m_[9] * m_[3] * m_[14] -
            m_[13] * m_[2] * m_[11] +
            m_[13] * m_[3] * m_[10];

        inv[5] = m_[0] * m_[10] * m_[15] -
            m_[0] * m_[11] * m_[14] -
            m_[8] * m_[2] * m_[15] +
            m_[8] * m_[3] * m_[14] +
            m_[12] * m_[2] * m_[11] -
            m_[12] * m_[3] * m_[10];

        inv[9] = -m_[0] * m_[9] * m_[15] +
            m_[0] * m_[11] * m_[13] +
            m_[8] * m_[1] * m_[15] -
            m_[8] * m_[3] * m_[13] -
            m_[12] * m_[1] * m_[11] +
            m_[12] * m_[3] * m_[9];

        inv[13] = m_[0] * m_[9] * m_[14] -
            m_[0] * m_[10] * m_[13] -
            m_[8] * m_[1] * m_[14] +
            m_[8] * m_[2] * m_[13] +
            m_[12] * m_[1] * m_[10] -
            m_[12] * m_[2] * m_[9];

        inv[2] = m_[1] * m_[6] * m_[15] -
            m_[1] * m_[7] * m_[14] -
            m_[5] * m_[2] * m_[15] +
            m_[5] * m_[3] * m_[14] +
            m_[13] * m_[2] * m_[7] -
            m_[13] * m_[3] * m_[6];

        inv[6] = -m_[0] * m_[6] * m_[15] +
            m_[0] * m_[7] * m_[14] +
            m_[4] * m_[2] * m_[15] -
            m_[4] * m_[3] * m_[14] -
            m_[12] * m_[2] * m_[7] +
            m_[12] * m_[3] * m_[6];

        inv[10] = m_[0] * m_[5] * m_[15] -
            m_[0] * m_[7] * m_[13] -
            m_[4] * m_[1] * m_[15] +
            m_[4] * m_[3] * m_[13] +
            m_[12] * m_[1] * m_[7] -
            m_[12] * m_[3] * m_[5];

        inv[14] = -m_[0] * m_[5] * m_[14] +
            m_[0] * m_[6] * m_[13] +
            m_[4] * m_[1] * m_[14] -
            m_[4] * m_[2] * m_[13] -
            m_[12] * m_[1] * m_[6] +
            m_[12] * m_[2] * m_[5];

        inv[3] = -m_[1] * m_[6] * m_[11] +
            m_[1] * m_[7] * m_[10] +
            m_[5] * m_[2] * m_[11] -
            m_[5] * m_[3] * m_[10] -
            m_[9] * m_[2] * m_[7] +
            m_[9] * m_[3] * m_[6];

        inv[7] = m_[0] * m_[6] * m_[11] -
            m_[0] * m_[7] * m_[10] -
            m_[4] * m_[2] * m_[11] +
            m_[4] * m_[3] * m_[10] +
            m_[8] * m_[2] * m_[7] -
            m_[8] * m_[3] * m_[6];

        inv[11] = -m_[0] * m_[5] * m_[11] +
            m_[0] * m_[7] * m_[9] +
            m_[4] * m_[1] * m_[11] -
            m_[4] * m_[3] * m_[9] -
            m_[8] * m_[1] * m_[7] +
            m_[8] * m_[3] * m_[5];

        inv[15] = m_[0] * m_[5] * m_[10] -
            m_[0] * m_[6] * m_[9] -
            m_[4] * m_[1] * m_[10] +
            m_[4] * m_[2] * m_[9] +
            m_[8] * m_[1] * m_[6] -
            m_[8] * m_[2] * m_[5];

        float det = m_[0] * inv[0] + m_[1] * inv[4] + m_[2] * inv[8] + m_[3] * inv[12];
        float invDet = 1 / det;

        for (int i = 0; i < 16; i++)
            invOut[i] = inv[i] * invDet;

        return res;
    }

    Vector<4> Row(int m0) const
    {
        return Vector4(_mm_loadu_ps(Val[m0]));
    }

    static Matrix Translate(Vector3 const &off)
    {
        float x = off.Val[0], y = off.Val[1], z = off.Val[2];

        Matrix<4, 4> res =
        {
            {
                1, 0, 0, 0,
                0, 1, 0, 0,
                0, 0, 1, 0,
                x, y, z, 1,
            },
        };
        return res;
    }

    static Matrix Scale(Vector3 const &scale)
    {
        float x = scale.Val[0], y = scale.Val[1], z = scale.Val[2];

        Matrix<4, 4> res =
        {
            {
                x, 0, 0, 0,
                0, y, 0, 0,
                0, 0, z, 0,
                0, 0, 0, 1,
            },
        };
        return res;
    }

    static Matrix Rotate(Degree angle, Vector3 const &dir)
    {
        float c = Cos(angle);
        float s = Sin(angle);

        float x = dir.Val[0], y = dir.Val[1], z = dir.Val[2];

        Matrix res =
        {
            {
                x * x * (1 - c) + c, x * y * (1 - c) + z * s, x  * z * (1 - c) - y * s, 0,
                x * y * (1 - c) - z * s, y * y * (1 - c) + c, y * z * (1 - c) + x * s, 0,
                x * z * (1 - c) + y * s, y * z * (1 - c) - x * s, z * z * (1 - c) + c, 0,
                0, 0, 0, 1,
            },
        };

        return res;
    }

    static Matrix LookAt(Vector3 const &eye, Vector3 const &target, Vector3 const &upDir)
    {
        Vector3 forward = (eye - target).Normalize();
        Vector3 left = upDir.Cross(forward).Normalize();
        Vector3 up = forward.Cross(left);

        Matrix res;

        Vector4 *p = (Vector4*)&res;
        p[0] = Vector4(left, -left.Dot(eye));
        p[1] = Vector4(up, -up.Dot(eye));
        p[2] = Vector4(forward, -forward.Dot(eye));
        p[3] = Vector4(0, 0, 0, 1);

        return res.Transpose();
    }

    static Matrix Perspective(float l, float r, float t, float b, float n_, float f)
    {
        Matrix res =
        {
            {
                2 * n_ / (r - l), 0, 0, 0,
                0, 2 * n_ / (t - b), 0, 0,
                (r + l) / (r - l), (t + b) / (t - b), (f + n_) / ((f - n_) * 2) - 0.5f, -1,
                0, 0, f * n_ / (f - n_), 0,
            },
        };

        return res;
    }

    static Matrix Perspective(Degree fovY, float aspect, float n_, float f)
    {
        float t = Tan(Degree{ fovY.Val / 2 }) * n_;
        float r = aspect * t;
        return Perspective(-r, r, t, -t, n_, f);
    }

    static Matrix Orthographic(float l, float r, float t, float b, float n_, float f)
    {
        Matrix res =
        {
            {
                2 / (r - l), 0, 0, 0,
                0, 2 / (t - b), 0, 0,
                0, 0, 1 / (f - n_), 0,
                -(r + l) / (r - l), -(t + b) / (t - b), (f + n_) / ((f - n_) * 2) + 0.5f, 1,
            }
        };

        return res;
    }

    static Matrix Viewport(int x, int y, int w, int h)
    {
        Matrix res =
        {
            {
                w / 2.f, 0, 0, 0,
                0, -h / 2.f, 0, 0,
                0, 0, 1, 0,
                w / 2.f + x, h / 2.f + y, 0, 1,
            },
        };

        return res;
    }

};

using Matrix2 = Matrix<2, 2>;
using Matrix3 = Matrix<3, 3>;
using Matrix4 = Matrix<4, 4>;




template<int m, int n, int k>
inline Matrix<m, n> operator * (Matrix<m, k> const &a, Matrix<k, n> const &b)
{
    Matrix<m, n> res;
    for (int m0 = 0; m0 < m; ++m0)
    {
        for (int n0 = 0; n0 < n; ++n0)
        {
            float v = 0;
            for (int k0 = 0; k0 < k; ++k0)
            {
                v += a.Val[m0][k0] * b.Val[k0][n0];
            }
            res.Val[m0][n0] = v;
        }
    }
    return res;
}
template<int m, int n>
inline Vector<n> operator * (Vector<m> const &vec, Matrix<m, n> const &mat)
{
    Vector<n> res;
    for (int n0 = 0; n0 < n; ++n0)
    {
        float v = 0;
        for (int m0 = 0; m0 < m; ++m0)
            v += vec.Val[m0] * mat.Val[m0][n0];
        res.Val[n0] = v;
    }
    return res;
}
template<int m>
inline Vector<4> operator * (Vector<m> const &vec, Matrix<m, 4> const &mat)
{
    Vector<4> res;
    res.SIMD = _mm_mul_ps(_mm_set1_ps(vec.Val[0]), _mm_loadu_ps(mat.Val[0]));
    For<m - 1>([&](auto m0)
    {
        res.SIMD = _mm_fmadd_ps(
            _mm_set1_ps(vec.Val[m0.Val + 1]),
            _mm_loadu_ps(mat.Val[m0.Val + 1]), res.SIMD);
    });
    return res;
}


template<int m, int n>
inline std::ostream& operator << (std::ostream &out, Matrix<m, n> const &mat)
{
    for (int m0 = 0; m0 < m; ++m0)
    {
        for (int n0 = 0; n0 < n; ++n0)
        {
            if (n0 > 0)
                out << ",";
            out << mat.Val[m0][n0];
        }
        out << "\n";
    }
    return out;
}
template<int m, int n>
inline std::istream& operator >> (std::istream &in, Matrix<m, n> const &mat)
{
    for (int m0 = 0; m0 < m; ++m0)
    {
        for (int n0 = 0; n0 < n; ++n0)
        {
            if (n0 > 0)
                RAssert(Read<char>(in) == ',');
            in >> mat.Val[m0][n0];
        }
    }
    return in;
}



#endif
