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
    @brief 强制类型转换
    相当于(Type)v, 但是不会有warning.
    只有在来源和目标类型具有相同尺寸的时候才可用转化.
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
    @brief 返回一个空字符串
*/
inline const String& getEmptyString()
{
    static String ls_empty;
    return ls_empty;
}

/**
    @brief 将指定类型转化为字符串
*/
template<typename T>
inline const String toString(const T &i)
{
    return static_cast<std::ostringstream&>(std::ostringstream() << i).str();
}

inline const String toString(const char* i) // 避免被当做指针处理
{
    return i;
}
inline const String& toString(const String& i) // 提高效率
{
    return i;
}
inline const String toString(char c) // 由于ms的ostringstream存在bug, 所以对char特殊处理
{
    return String(1, c);
}
inline const String toString(bool b) // 使用true/false
{
    return b ? "true" : "false";
}

/**
    @brief 从字符串中提取特定类型的数据
    @return true表示转化成功
*/
template<typename T>
inline bool fromString(T &obj, const String& src)
{
    std::istringstream si(src);
    si >> obj;
    return /*si.eof() && */!si.fail(); // 注释掉eof, 即反序列化不要求完全匹配了
}
template<typename T>
inline bool fromString(T* &obj, const String& src)  // 特化处理指针
{
    std::istringstream si(src);
    si >> std::hex >> reinterpret_cast<size_t&>(obj);
    return /*si.eof() && */!si.fail();
}
inline bool fromString(String& obj, const String& src)  // 为了保证正确性.避免被空格或换行隔断
{
    obj = src;
    return true;
}
inline bool fromString(char& obj, const String& src) // 因为只读取一个字符, 模板的默认实现中eof不会返回真
{
    if (src.size() == 1) 
    {
        obj = src[0]; return true;
    }
    return false;
}
inline bool fromString(bool &b, const String& src)  // 使用true/false
{
    b = src == "true";
    return true;
}

/**
    @brief 一个本地化locale
    在它的生命期中, locale会变为ansi
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
    @brief 对std::type_info的包装
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
        Scan::messageBox("文件(行): " SCAN_PP_FILE_LINE "\n函数    :" __FUNCTION__ "\n表达式  :" # exp, "断言失败");    \
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
    @brief 便利的插入, 很好用; qt的做法
    如函数fun(const vector<int>& v), 可以这样调用fun(vector<int>() << 3 << 5) !
 */
template<typename T, typename A>
std::vector<T, A>& operator << (std::vector<T, A>& v, typename std::vector<T, A>::value_type const& t)
{
    v.push_back(t); return v;
}

}