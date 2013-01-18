// vim: fileencoding=gbk

#ifndef UTIL_H
#define UTIL_H

//----------------------------------------
// 宏
//----------------------------------------
#define _TO_STR(s) #s
#define TO_STR(s) _TO_STR(s)

#define _CONN(a, b) a##b
#define CONN(a, b) _CONN(a, b)

//----------------------------------------
// 数学
//----------------------------------------
extern const float PI;

extern const float EPSILON;
extern const float EPSILON4;
extern const float EPSILON5;

extern const float MIN_FLOAT;
extern const float MAX_FLOAT;


inline bool fequal(float a, float b, float epsilon = EPSILON)
{
    return a < b ? (b - a) < epsilon : (a - b) < epsilon;
}

inline float degree2Radian(float f){ return f * PI / 180; }
inline float radian2Degree(float f) { return f * 180 / PI; }

inline bool isPower2(int i)
{
    return i > 0 && ((i - 1) & i) == 0;
}

//----------------------------------------
// 其他
//----------------------------------------

template<typename T>
inline void sdelete(T*& p) { delete p; p = NULL; }

template<typename T, int n>
inline int arraySize(T (&a)[n]) { return n; }

#endif // #ifndef UTIL_H
