// test2008.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include <limits>

struct Float
{
    unsigned    m_f   : 23;
    unsigned    m_e   : 8;
    unsigned    m_sig : 1;
};

float floatMin()
{
    Float f;
    f.m_sig = 0;
    f.m_f = 0;
    f.m_e = 1;
    return (float&)f;
}

float floatMax()
{
    Float f;
    f.m_sig = 0;
    f.m_f = 0x7fffff;
    f.m_e = 0xfe;
    return (float&)f;
}

float floatNegativeInf()
{
    Float f;
    f.m_sig = 1;
    f.m_f = 0;
    f.m_e = 0xff;
    return (float&)f;
}

float floatPositiveInf()
{
    Float f;
    f.m_sig = 0;
    f.m_f = 0;
    f.m_e = 0xff;
    return (float&)f;
}

float floatNaN()
{
    Float f;
    f.m_sig = 0;
    f.m_f = 1;
    f.m_e = 0xff;
    return (float&)f;
}

int main()
{
    float f = 0;
    printf("%9.9e -> %9.9e\n", floatMin(), std::numeric_limits<float>::min());
    printf("%9.9e -> %9.9e\n", floatMax(), std::numeric_limits<float>::max());
    printf("%9.9e -> %9.9e\n", floatNegativeInf(), -1 / f);
    printf("%9.9e -> %9.9e\n", floatPositiveInf(), 1 / f);
    printf("%9.9e -> %9.9e\n", floatNaN(), f / f);
}