#ifndef PCH_H
#define PCH_H

#define CHECK_MEMORY_LEAKS

#ifdef _MSC_VER
#pragma warning(disable : 4996)

#ifdef CHECK_MEMORY_LEAKS
#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>
#endif

#endif

#include <assert.h>
#include <string.h>

#include <algorithm>
#include <memory>
#include <exception>
#include <string>
#include <sstream>
#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <set>

using namespace std;

#define _TO_STRING(s) #s
#define TO_STRING(s) _TO_STRING(s)
#define FILE_LINE __FILE__"(" TO_STRING(__LINE__) ")"

#ifdef NDEBUG
#define ASSERT1(b, msg) if (b); else throw Exception(string(FILE_LINE " : " #b "->") + msg)
#define ASSERT(b) ASSERT1(b, "")
#else
//#define ASSERT1(b, msg)  assert(b && msg)
//#define ASSERT(b) assert(b)
#define ASSERT1(b, msg) if (b); else throw Exception(string(FILE_LINE " : " #b "->") + msg)
#define ASSERT(b) ASSERT1(b, "")
#endif

class Exception:
    public exception {
public:
    Exception(const string& s): m_s(s) {
        assert(0);
    }
    ~Exception() throw(){}
    const char* what() const throw() {
        return m_s.c_str();
    }
    void addLine(const string& line) { m_s += "\n" + line; }
private:
    string m_s;
};

string format(const char *fmt, ...);

#endif
