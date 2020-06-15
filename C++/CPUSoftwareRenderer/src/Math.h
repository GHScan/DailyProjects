#ifndef MATH_H
#define MATH_H


#include <cmath>

#include <algorithm>

#include <immintrin.h>


template<typename T>
inline T Clamp(T v, T minV, T maxV)
{
    return std::max(minV, std::min(maxV, v));
}

template<typename T>
inline T Lerp(T const &a, T const &b, float t)
{
    return a * (1 - t) + b * t;
}


inline float Pow(float x, float y)
{
    int flipped = 0;
    if (y < 0)
    {
        flipped = 1;
        y = -y;
    }

    int intY = (int)y;
    union { float f; int x; } u = { x };
    u.x = (int)((y - intY) * (u.x - 1065353216) + 1065353216);

    float r = 1.0f;
    while (intY)
    {
        if (intY & 1)
        {
            r *= x;
        }
        x *= x;
        intY >>= 1;
    }

    r *= u.f;
    return flipped ? 1.0f / r : r;
}

inline float Pow3(float x)
{
    return x * x * x;
}

// x, y should be in [0, 1]
inline __m128 Pow(__m128 x, __m128 y)
{
    auto t0 = _mm_sub_epi32(_mm_castps_si128(x), _mm_set1_epi32(1064866805));
    auto t1 = _mm_add_ps(_mm_mul_ps(y, _mm_cvtepi32_ps(t0)), _mm_set1_ps(1064866805.f));
    return _mm_castsi128_ps(_mm_cvtps_epi32(t1));
}


/* max. rel. error <= 1.72886892e-3 on [-87.33654, 88.72283] */
inline __m128 Exp(__m128 x)
{
    __m128 f, p, r;
    __m128i t, j;
    const __m128 a = _mm_set1_ps(12102203.0f); /* (1 << 23) / log(2) */
    const __m128i m = _mm_set1_epi32(0xff800000); /* mask for integer bits */
    const __m128 ttm23 = _mm_set1_ps(1.1920929e-7f); /* exp2(-23) */
    const __m128 c0 = _mm_set1_ps(0.3371894346f);
    const __m128 c1 = _mm_set1_ps(0.657636276f);
    const __m128 c2 = _mm_set1_ps(1.00172476f);

    t = _mm_cvtps_epi32(_mm_mul_ps(a, x));
    j = _mm_and_si128(t, m);            /* j = (int)(floor (x/log(2))) << 23 */
    t = _mm_sub_epi32(t, j);
    f = _mm_mul_ps(ttm23, _mm_cvtepi32_ps(t)); /* f = (x/log(2)) - floor (x/log(2)) */
    p = c0;                              /* c0 */
    p = _mm_mul_ps(p, f);               /* c0 * f */
    p = _mm_add_ps(p, c1);              /* c0 * f + c1 */
    p = _mm_mul_ps(p, f);               /* (c0 * f + c1) * f */
    p = _mm_add_ps(p, c2);              /* p = (c0 * f + c1) * f + c2 ~= 2^f */
    r = _mm_castsi128_ps(_mm_add_epi32(j, _mm_castps_si128(p))); /* r = p * 2^i*/
    return r;
}


#define kPi     3.14159265359f

struct Radian final
{
    float Val;
};
struct Degree final
{
    float Val;
};

inline Degree ToDegree(Radian angle)
{
    return Degree{ angle.Val * 180.f / kPi };
}
inline Radian ToRadian(Degree angle)
{
    return Radian{ angle.Val * kPi / 180.f };
}


inline float Sin(Radian angle)
{
    return std::sin(angle.Val);
}
inline float Sin(Degree angle)
{
    return Sin(ToRadian(angle));
}

inline float Cos(Radian angle)
{
    return std::cos(angle.Val);
}
inline float Cos(Degree angle)
{
    return Cos(ToRadian(angle));
}

inline float Tan(Radian angle)
{
    return std::tan(angle.Val);
}
inline float Tan(Degree angle)
{
    return Tan(ToRadian(angle));
}



#endif
