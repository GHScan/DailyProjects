#pragma once

#include <cassert>

#include <sstream>
#include <string>
#include <exception>

#include <boost/preprocessor.hpp>

#include <windows.h>

namespace NS_Util
{
    class StringCreator
    {
    public:
        template<typename T>
        StringCreator(const T& v)
        {
            std::ostringstream os;
            os << v;
            m_s = os.str();
        }
        const std::string& getString() const { return m_s; }
    private:
        std::string m_s;
    };
}

#define _STRINGCREATOR_ARG(nextD, curD, data)        BOOST_PP_COMMA_IF(curD) NS_Util::StringCreator BOOST_PP_CAT(arg, curD)
#define _ARG_GETSTRING_INTO_OS(nextD, curD, data)    BOOST_PP_IF(curD, << '\t', BOOST_PP_EMPTY()) << BOOST_PP_CAT(arg, curD).getString()
#define _PRINT_IMPL(nextD, curD, data)\
    inline void print(BOOST_PP_REPEAT(curD, _STRINGCREATOR_ARG, NULL))\
{\
    cout BOOST_PP_REPEAT(curD, _ARG_GETSTRING_INTO_OS, NULL);\
    cout << endl;\
}

BOOST_PP_REPEAT(10, _PRINT_IMPL, NULL)

#undef _PRINT_IMPL
#undef _ARG_GETSTRING_INTO_OS
#undef _STRINGCREATOR_ARG

std::string format(const char *fmt, ...);

#define _STRING_MACRO(a) #a
#define STRING_MACRO(a) _STRING_MACRO(a)

#define FILE_LINE  __FILE__ "(" STRING_MACRO(__LINE__) ")"

#ifdef _DEBUG
#define verify assert
#else
#define verify(exp)\
    if (exp){}\
    else\
{\
    const char *msg = FILE_LINE " : \n" STRING_MACRO(exp) "\n";\
    ::OutputDebugString(msg);\
    ::MessageBox(NULL, msg, "¶ÏÑÔÊ§°Ü", MB_OK | MB_ICONERROR);\
    ::DebugBreak();\
}
#endif

#define verify_throw(exp)\
    if (exp){}\
    else\
{\
    const char *msg = FILE_LINE " : \n" STRING_MACRO(exp) "\n";\
    throw std::exception(msg);\
}

template<typename T>
inline std::string toString(const T& val)
{
    std::ostringstream so; 
    so << val;
    return so.str();
}

template<typename T>
inline T parseString(const std::string& s, bool *ok)
{
    std::istringstream si(s);
    T val;
    si >> val;
    if (ok != NULL) *ok = si.eof() && !si.fail();
    return val;
}