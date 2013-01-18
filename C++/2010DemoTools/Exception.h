#pragma once

#include <cassert>

#include <exception>
#include <string>
#include <sstream>

#include <boost/preprocessor.hpp>

#include "CompileEnvironment.h"

namespace Scan
{
    typedef std::string     String;
    typedef unsigned int    uint32;

    /**
        @brief 异常基类
    */
    class Exception:
        public std::exception
    {
    public:
        Exception(const String &describe, const String& file, uint32 line, const String& func):
          m_describe(describe), m_file(file), m_line(line), m_func(func){}

        const String& getDescribe() const
        {
            return m_describe;
        }

        const String& getFile() const
        {
            return m_file;
        }

        uint32 getLine() const
        {
            return m_line;
        }

        const String& getFunction() const
        {
            return m_func;
        }

        const String& getFullDescribe() const
        {
            if (m_fullDescribe.empty())
            {
                m_fullDescribe =  m_file + "(" + 
                    static_cast<const std::ostringstream&>(std::ostringstream() << m_line).str() + ") : ";

                m_fullDescribe += "[" + String(getExceptionType()) + "]";

                if (!m_func.empty())
                {
                    m_fullDescribe += "[" + m_func + "] ";
                }

                m_fullDescribe += m_describe;
            }
            return m_fullDescribe;
        }

        virtual const char* what() const
        {
            return getFullDescribe().c_str();
        }

        virtual const char *getExceptionType() const = 0;

    private:
        String          m_describe;
        String          m_file;
        uint32          m_line;
        String          m_func;
        mutable String  m_fullDescribe;
    };

    /**
        @brief 支持的异常类型
    */
    enum ExceptionType
    {
        ET_InvalidParams, /**< 无效的参数 */
        ET_FileNotFound, /**< 文件没有找到 */
        ET_IOError, /**< IO错误 */
        ET_LibraryCallFailed, /**< 库调用发生错误 */
        ET_FatalError, /**< 致命错误 */
    };

    /**
        @brief 将异常类型转化为描述字符串的帮助类
    */
    template<ExceptionType e>
    struct ExecptionTypeToString;

#define _SCAN_REGISTER_EXCEPTIONTYPESTRING(e) \
    template<> \
    struct ExecptionTypeToString<e> \
    { \
        static const char* getString() \
        { \
            return #e; \
        } \
    };

    _SCAN_REGISTER_EXCEPTIONTYPESTRING(ET_InvalidParams);
    _SCAN_REGISTER_EXCEPTIONTYPESTRING(ET_FileNotFound);
    _SCAN_REGISTER_EXCEPTIONTYPESTRING(ET_IOError);
    _SCAN_REGISTER_EXCEPTIONTYPESTRING(ET_LibraryCallFailed);
    _SCAN_REGISTER_EXCEPTIONTYPESTRING(ET_FatalError);

#undef _SCAN_REGISTER_EXCEPTIONTYPESTRING

    /**
        @brief 具体的派生异常类型
    */
    template<ExceptionType e>
    class SubException:
        public Exception
    {
    public:
        SubException(const String &describe, const String& file, uint32 line, const String& func):
          Exception(describe, file, line, func){}

          virtual const char *getExceptionType() const
          {
              return ExecptionTypeToString<e>::getString();
          }
    };
}

/**
    @brief 抛出一个特定类型的异常
*/
#define SCAN_THROW(exceptionType, s)    throw Scan::SubException<exceptionType>(s, __FILE__, __LINE__, SCAN_FUNCTION)

/**
    @brief 校验, 在release下抛出异常
*/
#if defined(DEBUG) || defined(_DEBUG)
#define SCAN_EXCP_VERIFY(b, exceptionType)  assert(b)
#else
#define SCAN_EXCP_VERIFY(b, exceptionType)  if (b);else SCAN_THROW(exceptionType, BOOST_PP_STRINGIZE(b))
#endif