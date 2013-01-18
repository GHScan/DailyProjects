#pragma once

#include <cassert>

#include <vector>
#include <string>
#include <sstream>

// 版本: 2010.7.9.1

// 常用工具
// 解决基本需求!!!
// 轻量

namespace Scan
{
    typedef std::string             String;
    typedef std::vector<String>     StringVec;
    typedef std::istringstream      IStringStream;
    typedef std::ostringstream      OStringStream;
    typedef std::ifstream           IFStream;
    typedef std::ofstream           OFStream;

    typedef unsigned char       uint8;
    typedef unsigned short      uint16;
    typedef unsigned int        uint32;
    typedef unsigned long long  uint64;

    int split(StringVec &result, const String &src, const String& delm = ", ");
    const String trim(const String& src);

    template<typename T>
    inline const String toString(const T& v)
    {
        OStringStream o;
        o << v;
        return o.str();
    }

    template<typename T>
    inline bool fromString(T &v, const String& s)
    {
        IStringStream i(s);
        i >> v;
        return i.eof() && !i.fail();
    }

    template<typename T>
    inline void safe_delete(T* &p)
    {
        delete p;
        p = NULL;
    }

    template<typename DestT, typename SrcT>
    inline DestT force_cast(SrcT arg)
    {
        assert(sizeof(DestT) == sizeof(SrcT));
        union{ SrcT src; DestT dest;} t = { arg };
        return t.dest;
    }

#define countOf(a)  (sizeof((a)) / sizeof((a)[0]))
#define toZero(a)   memset(&a, 0, sizeof(a))
}