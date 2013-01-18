#pragma once

#include <cassert>

#include <sstream>
#include <fstream>

#include <boost/utility.hpp>

#include "TypeDefine.h"
#include "Preprocessor.h"

namespace Scan
{

/**
    @brief ǿ������ת��
    �൱��(Type)v, ���ǲ�����warning.
    ֻ������Դ��Ŀ�����;�����ͬ�ߴ��ʱ��ſ���ת��.
*/
template<typename DesT, typename SrcT>
inline DesT force_cast(SrcT src)
{
    BOOST_STATIC_ASSERT(sizeof(SrcT) == sizeof(DesT));
    union { SrcT src; DesT des; }temp = {src};
    return temp.des;
}

template<typename T>
inline void safe_delete(T*& p) {    delete p; p = NULL; }
template<typename T>
inline void safe_Release(T*& p) {   if (p != NULL) p->Release(); p = NULL; }
template<typename T, size_t n>
inline size_t sizeOfArray(T (&a)[n])  { return n; }    

/**
    @brief ����һ�����ַ���
*/
inline const String& getEmptyString()
{
    static String ls_empty;
    return ls_empty;
}

/**
    @brief ��ָ������ת��Ϊ�ַ���
*/
template<typename T>
inline const String toString(const T &i)
{
    return static_cast<std::ostringstream&>(std::ostringstream() << i).str();
}

inline const String toString(const char* i) // ���ⱻ����ָ�봦��
{
    return i;
}
inline const String& toString(const String& i) // ���Ч��
{
    return i;
}
inline const String toString(char c) // ����ms��ostringstream����bug, ���Զ�char���⴦��
{
    return String(1, c);
}
inline const String toString(bool b) // ʹ��true/false
{
    return b ? "true" : "false";
}

/**
    @brief ���ַ�������ȡ�ض����͵�����
    @return true��ʾת���ɹ�
*/
template<typename T>
inline bool fromString(T &obj, const String& src)
{
    std::istringstream si(src);
    si >> obj;
    return /*si.eof() && */!si.fail(); // ע�͵�eof, �������л���Ҫ����ȫƥ����
}
template<typename T>
inline bool fromString(T* &obj, const String& src)  // �ػ�����ָ��
{
    std::istringstream si(src);
    si >> std::hex >> reinterpret_cast<size_t&>(obj);
    return /*si.eof() && */!si.fail();
}
inline bool fromString(String& obj, const String& src)  // Ϊ�˱�֤��ȷ��.���ⱻ�ո���и���
{
    obj = src;
    return true;
}
inline bool fromString(char& obj, const String& src) // ��Ϊֻ��ȡһ���ַ�, ģ���Ĭ��ʵ����eof���᷵����
{
    if (src.size() == 1) 
    {
        obj = src[0]; return true;
    }
    return false;
}
inline bool fromString(bool &b, const String& src)  // ʹ��true/false
{
    b = src == "true";
    return true;
}

/**
    @brief һ�����ػ�locale
    ��������������, locale���Ϊansi
*/
class NativeLocale:
    private Copyable<false>
{
public:
    NativeLocale();
    ~NativeLocale();

private:
    char *m_lastLocale;
};

/**
    @brief ��std::type_info�İ�װ
*/
class TypeInfoWrapper:
    private Copyable<true>
{
public:
    TypeInfoWrapper(const std::type_info& info):
      m_info(info){}

      friend bool operator == (const TypeInfoWrapper& l, const TypeInfoWrapper& r);
      friend bool operator < (const TypeInfoWrapper& l, const TypeInfoWrapper& r);

private:
    const std::type_info&     m_info;
};
inline bool operator == (const TypeInfoWrapper& l, const TypeInfoWrapper& r)
{
    return l.m_info == r.m_info;
}
inline bool operator != (const TypeInfoWrapper& l, const TypeInfoWrapper& r)
{
    return !(l == r);
}
inline bool operator < (const TypeInfoWrapper& l, const TypeInfoWrapper& r)
{
    return l.m_info.before(r.m_info) == 1 ? true : false;
}
inline bool operator >= (const TypeInfoWrapper& l, const TypeInfoWrapper& r)
{
    return !(l < r);
}
inline bool operator > (const TypeInfoWrapper& l, const TypeInfoWrapper& r)
{
    return r < l;
}
inline bool operator <= (const TypeInfoWrapper& l, const TypeInfoWrapper& r)
{
    return !(r < l);
}

void messageBox(const String& msg, const String& title);
void breakDebuger();
unsigned long long getCpuFrequency();

#define ASSERT_DEBUG         assert

#ifdef _DEBUG
#define ASSERT               ASSERT_DEBUG
#else
#define ASSERT(exp) \
    do  \
    {   \
        if (exp) break; \
        Scan::messageBox("�ļ�(��): " SCAN_PP_FILE_LINE "\n����    :" __FUNCTION__ "\n���ʽ  :" # exp, "����ʧ��");    \
        Scan::breakDebuger();    \
    } while (0)
#endif

class StringPrinter:
    private Copyable<false>
{
public:
    StringPrinter(){}
    explicit StringPrinter(const String& s) { setString(s); }

    void setString(const String& s) { m_os.str(s); }
    const String getString() const  { return m_os.str(); }

    std::ostringstream& stream() { m_os.str(""); return m_os; }
    std::ostringstream& pushStream() { return m_os; }

    StringPrinter& format(const char *fmt, ...);
    StringPrinter& pushFormat(const char *fmt, ...);

    StringPrinter& printToFile(const String& fileName);
    StringPrinter& printToDebugger();
    StringPrinter& printToStdout();
    StringPrinter& printToStderr();

private:
    std::ostringstream  m_os;
};


#define CACHED_OUTPUT_FILE_SIZE  5
class CachedOutputFileManager:
    private Copyable<false>
{
public:
    static CachedOutputFileManager* getSingletonPtr();
    std::ofstream& getFile(const String& fileName);

private:
    String        m_fileNames[CACHED_OUTPUT_FILE_SIZE];
    std::ofstream m_files[CACHED_OUTPUT_FILE_SIZE];
    long          m_lastVisit[CACHED_OUTPUT_FILE_SIZE];
};

class Timer:
    private Copyable<false>
{
public:
    Timer(): m_beginCounter(0) { reset(); }
    float getElapseSeconds() const;
    unsigned long long getElapseCounter() const;
    void reset();

private:
    unsigned long long  m_beginCounter;
};

class ScopeTimer:
    public Timer
{
public:
    ScopeTimer(
        const String& s, 
        FILE *print2File = stdout, bool print2Debuger = true): m_s(s), m_print2File(print2File), m_print2Debugger(print2Debuger){}
    ~ScopeTimer();
private:
    FILE    *m_print2File;
    bool     m_print2Debugger;
    String   m_s;
};

/**
    @brief �����Ĳ���, �ܺ���; qt������
    �纯��fun(const vector<int>& v), ������������fun(vector<int>() << 3 << 5) !
 */
template<typename T, typename A>
std::vector<T, A>& operator << (std::vector<T, A>& v, typename std::vector<T, A>::value_type const& t)
{
    v.push_back(t); return v;
}

}