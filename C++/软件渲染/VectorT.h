// vim: fileencoding=gbk

#ifndef VECTORT_H
#define VECTORT_H

#include <cassert>

template<typename T0, typename T1>
struct Vector2T
{
    T0 t0;
    T1 t1;
    Vector2T(const T0& _t0, const T1& _t1): 
        t0(_t0), t1(_t1){}

    Vector2T& operator += (const Vector2T& o)
    {
        t0 += o.t0; t1 += o.t1;
        return *this;
    }
    Vector2T& operator -= (const Vector2T& o)
    {
        t0 -= o.t0; t1 -= o.t1;
        return *this;
    }
    template<typename T>
    Vector2T& operator *= (const T& val) 
    {
        t0 *= val; t1 *= val;
        return *this;
    }
    template<typename T>
    Vector2T& operator /= (const T& val)
    {
        assert(val != 0);
        return *this *= 1 / val;
    }
    static Vector2T ZERO;
};
template<typename T0, typename T1>
Vector2T<T0, T1> Vector2T<T0, T1>::ZERO = Vector2T<T0, T1>(
        zero<T0>(), zero<T1>());

template<typename T0, typename T1>
Vector2T<T0, T1> operator + (const Vector2T<T0, T1>& a, const Vector2T<T0, T1>& b)
{
    return Vector2T<T0, T1>(a) += b;
}
template<typename T0, typename T1>
Vector2T<T0, T1> operator - (const Vector2T<T0, T1>& a, const Vector2T<T0, T1>& b)
{
    return Vector2T<T0, T1>(a) -= b;
}
template<typename T0, typename T1, typename T>
Vector2T<T0, T1> operator * (const Vector2T<T0, T1>& a, const T& val)
{
    return Vector2T<T0, T1>(a) *= val;
}
template<typename T0, typename T1, typename T>
Vector2T<T0, T1> operator / (const Vector2T<T0, T1>& a, const T& val)
{
    return Vector2T<T0, T1>(a) /= val;
}

template<typename T0, typename T1, typename T2>
struct Vector3T
{
    T0 t0;
    T1 t1;
    T2 t2;
    Vector3T(const T0& _t0, const T1& _t1, const T2& _t2): 
        t0(_t0), t1(_t1), t2(_t2){}

    Vector3T& operator += (const Vector3T& o)
    {
        t0 += o.t0; t1 += o.t1; t2 += o.t2;
        return *this;
    }
    Vector3T& operator -= (const Vector3T& o)
    {
        t0 -= o.t0; t1 -= o.t1; t2 -= o.t2;
        return *this;
    }
    template<typename T>
    Vector3T& operator *= (const T& val) 
    {
        t0 *= val; t1 *= val; t2 *= val;
        return *this;
    }
    template<typename T>
    Vector3T& operator /= (const T& val)
    {
        assert(val != 0);
        return *this *= 1 / val;
    }
    static Vector3T ZERO;
};
template<typename T0, typename T1, typename T2>
Vector3T<T0, T1, T2> Vector3T<T0, T1, T2>::ZERO = Vector3T<T0, T1, T2>(
        zero<T0>(), zero<T1>(), zero<T2>());

template<typename T0, typename T1, typename T2>
Vector3T<T0, T1, T2> operator + (
        const Vector3T<T0, T1, T2>& a, const Vector3T<T0, T1, T2>& b)
{
    return Vector3T<T0, T1, T2>(a) += b;
}
template<typename T0, typename T1, typename T2>
Vector3T<T0, T1, T2> operator - (
        const Vector3T<T0, T1, T2>& a, const Vector3T<T0, T1, T2>& b)
{
    return Vector3T<T0, T1, T2>(a) -= b;
}
template<typename T0, typename T1, typename T2, typename T>
Vector3T<T0, T1, T2> operator * (
        const Vector3T<T0, T1, T2>& a, const T& val)
{
    return Vector3T<T0, T1, T2>(a) *= val;
}
template<typename T0, typename T1, typename T2, typename T>
Vector3T<T0, T1, T2> operator / (const Vector3T<T0, T1, T2>& a, const T& val)
{
    return Vector3T<T0, T1, T2>(a) /= val;
}

template<typename T> 
inline T zero() { return T::ZERO; }
template<>
inline char zero() { return 0; }
template<>
inline int zero() { return 0; }
template<>
inline float zero() { return 0.f; }

#endif // #ifndef VECTORT_H
