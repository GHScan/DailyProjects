#ifndef PCH_H
#define PCH_H

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

class Exception:
    public exception
{
public:
    Exception(const string& s): m_s(s){}
    ~Exception() throw(){}
    const char* what() const throw()
    {
        return m_s.c_str();
    }
private:
    string m_s;
};

string format(const char *fmt, ...);
string tabString(int n);

#define _TO_STRING(s) #s
#define TO_STRING(s) _TO_STRING(s)
#define FILE_LINE __FILE__"(" TO_STRING(__LINE__) ")"

#define ASSERT1(b, msg) if (b); else throw Exception(string(FILE_LINE " : " #b "->") + msg)
#define ASSERT(b) ASSERT1(b, "")

#ifdef _MSC_VER
#pragma warning(disable : 4996)
#endif

#endif
