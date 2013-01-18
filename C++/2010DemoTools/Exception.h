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
        @brief �쳣����
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
        @brief ֧�ֵ��쳣����
    */
    enum ExceptionType
    {
        ET_InvalidParams, /**< ��Ч�Ĳ��� */
        ET_FileNotFound, /**< �ļ�û���ҵ� */
        ET_IOError, /**< IO���� */
        ET_LibraryCallFailed, /**< ����÷������� */
        ET_FatalError, /**< �������� */
    };

    /**
        @brief ���쳣����ת��Ϊ�����ַ����İ�����
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
        @brief ����������쳣����
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
    @brief �׳�һ���ض����͵��쳣
*/
#define SCAN_THROW(exceptionType, s)    throw Scan::SubException<exceptionType>(s, __FILE__, __LINE__, SCAN_FUNCTION)

/**
    @brief У��, ��release���׳��쳣
*/
#if defined(DEBUG) || defined(_DEBUG)
#define SCAN_EXCP_VERIFY(b, exceptionType)  assert(b)
#else
#define SCAN_EXCP_VERIFY(b, exceptionType)  if (b);else SCAN_THROW(exceptionType, BOOST_PP_STRINGIZE(b))
#endif