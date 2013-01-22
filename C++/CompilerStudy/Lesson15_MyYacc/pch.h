#ifndef PCH_H
#define PCH_H

#include <stdio.h>
#include <assert.h>
#include <time.h>

#include <iostream>
#include <algorithm>
#include <fstream>
#include <vector>
#include <string>
#include <map>
#include <set>
#include <memory>

using namespace std;

#ifdef _MSC_VER
#pragma warning(disable : 4018)
#pragma warning(disable : 4996)
#endif

string format(const char *fmt, ...);
string readFile(const string& fname);

class Exception:
    public exception
{
public:
    Exception(const char *file, int line, const char *msg)
    {
        m_str = format("%s(%d) : %s\n", file, line, msg);
    }
    ~Exception() throw() {}
    const char *what () const throw() { return m_str.c_str(); }
private:
    string m_str;
};
class Timer
{
public:
    Timer(const string& name):
        m_name(name)
    {
        m_start = clock();
    }
    ~Timer()
    {
        cout << format("Timer(%s) : %f\n", m_name.c_str(), float(clock() - m_start) / CLOCKS_PER_SEC);
    }
private:
    clock_t m_start;
    string m_name;
};

template<typename T>
struct VectorBuilder
{
    vector<T> vec;
    template<typename U>
    VectorBuilder& operator << (const U& val)
    {
        vec.push_back(T(val));
        return *this;
    }
};

#define _TO_STRING(s) #s
#define TO_STRING(s) _TO_STRING(s)

#define ASSERT1(b, msg) if (b); else throw Exception(__FILE__, __LINE__, (string(#b) + msg).c_str())
#define ASSERT(b) ASSERT1(b, "")

#define COUNT_OF(a) (sizeof(a) / sizeof((a)[0]))

#endif
