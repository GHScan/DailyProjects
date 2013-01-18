#pragma once

#include <boost/function.hpp>

#include "Types.h"

namespace Scan
{
    class OutStream
    {
    public:
        OStringStream& getStream();
        OutStream& format(const char *fmt, ...);

    private:
        // OutStream(const OutStream&);
        // OutStream& operator = (const OutStream&);

    private:
        OStringStream   m_os;
    };

    template<typename T>
    inline void safe_delete(T* &p)
    {
        delete p;
        p = NULL;
    }

    template<typename T>
    inline void safe_release(T* &p)
    {
        if (p != NULL)
        {
            p->release();
            p = NULL;
        }
    }

    template<typename T>
    inline void safe_Release(T* &p)
    {
        if (p != NULL)
        {
            p->Release();
            p = NULL;
        }
    }

        /**
        @brief 编译期断言
    */
#define SCAN_STATIC_ASSERT(b)    { char a[(b) ? 1 : 0] = {0}; }

    /**
        @brief 强制类型转换
        相当于(Type)v, 但是不会有warning.
        只有在来源和目标类型具有相同尺寸的时候才可用转化.
    */
    template<typename DesType, typename SrcType>
    inline DesType force_cast(SrcType src)
    {
        SCAN_STATIC_ASSERT(sizeof(SrcType) == sizeof(DesType));
        union { SrcType src; DesType des; }temp = {src};
        return temp.des;
    }

    /**
        @brief 返回某类型的静态名称
    */
    template<typename T>
    inline const char * getStaticClassName()
    {
        static const char *ls_className = typeid(T).name();
        return ls_className;
    }

    /**
        @brief 返回某类型的动态名称
    */
    template<typename T>
    inline const char * getDynamicClassName(T& v)
    {
        return typeid(v).name();
    }

    /**
        @brief 返回数组的元素个数
    */
    template<typename T, uint32 len>
    inline uint32 countOf(T(&)[len])
    {
        return len;
    }

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
    template<typename Type>
    inline const String toString(const Type &i)
    {
        return static_cast<OStringStream&>(OStringStream() << i).str();
    }

    /**
        @brief 准特化String, 提高效率
    */
    inline const String& toString(const String& i)
    {
        return i;
    }

    /**
        @brief 准特化char        
        由于ms的ostringstream存在bug, 所以对char特殊处理
    */
    inline const String toString(char c)
    {
        return String(1, c);
    }

    /**
        @brief 准特化bool
        使用true/false
    */
    inline const String toString(bool b)
    {
        return static_cast<OStringStream&>(
            OStringStream() << std::boolalpha << b).str();
    }

    /**
        @brief 从字符串中提取特定类型的数据
        @return true表示转化成功
    */
    template<typename Type>
    inline bool fromString(Type &obj, const String& szSrc)
    {
        IStringStream si(szSrc);
        si >> obj;
        return si.eof() && !si.fail();
    }

    /**
        @brief 特化处理指针
    */
    template<typename Type>
    inline bool fromString(Type* &obj, const String& szSrc)
    {
        IStringStream si(szSrc);
        si >> std::hex >> reinterpret_cast<uint32&>(obj);
        return si.eof() && !si.fail();
    }

    /**
        @brief 针对String类型准特化fromString函数
        为了保证正确性.避免被空格或换行隔断
    */
    inline bool fromString(String& obj, const String& szSrc)
    {
        obj = szSrc;
        return true;
    }

    /**
        @brief 准特化bool
        使用true/false
    */
    inline bool fromString(bool &b, const String& szSrc)
    {
        IStringStream si(szSrc);
        si >> std::boolalpha >> b;
        return si.eof() && !si.fail();
    }

    /**
        @brief 将任意类型变量置0
    */
    template<typename T>
    inline void toZero(T &v)
    {
        memset(&v, 0, sizeof(v));
    }

    /**
        @brief 资源管理类, raii
    */
    class ResGuard
    {
    public:
        /**
            @brief 构造函数
            @param f 要调用的函数; 可以是类成员函数
            @param p 调用函数的参数; 可以是类指针
        */
        template<typename T, typename FunType>
        ResGuard(FunType f, T *p):
        m_f(boost::bind(f, p)){}
        ~ResGuard()
        {
            m_f();
        }

    private:
        /**
            @brief 禁止拷贝
        */
        ResGuard(const ResGuard&);
        ResGuard& operator = (const ResGuard&);

    private:
        boost::function<void(void)> m_f;
    };

    /**
        @brief 类型分配器
    */
    template<typename T>
    struct ClassAllocator
    {
        static T* alloc()
        {
            return new T;
        }
        static void unalloc(T *p)
        {
            delete p;
        }
    };

    /**
        @brife 极小的浮点数
    */
    extern const float FLOAT_EPSILON;

    /**
        @brief 极大的浮点数
    */
    extern const float FLOAT_INFINITY;

    /**
        @brief 一个本地化locale
        在它的生命期中, locale会变为ansi
    */
    class NativeLocale
    {
    public:
        NativeLocale();
        ~NativeLocale();

    private:
        NativeLocale(const NativeLocale&);
        NativeLocale& operator = (const NativeLocale&);

    private:
        char *m_lastLocale;
    };

    /**
        @brief 对std::type_info的包装
    */
    class TypeInfoWrapper
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
}