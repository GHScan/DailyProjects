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
        @brief �����ڶ���
    */
#define SCAN_STATIC_ASSERT(b)    { char a[(b) ? 1 : 0] = {0}; }

    /**
        @brief ǿ������ת��
        �൱��(Type)v, ���ǲ�����warning.
        ֻ������Դ��Ŀ�����;�����ͬ�ߴ��ʱ��ſ���ת��.
    */
    template<typename DesType, typename SrcType>
    inline DesType force_cast(SrcType src)
    {
        SCAN_STATIC_ASSERT(sizeof(SrcType) == sizeof(DesType));
        union { SrcType src; DesType des; }temp = {src};
        return temp.des;
    }

    /**
        @brief ����ĳ���͵ľ�̬����
    */
    template<typename T>
    inline const char * getStaticClassName()
    {
        static const char *ls_className = typeid(T).name();
        return ls_className;
    }

    /**
        @brief ����ĳ���͵Ķ�̬����
    */
    template<typename T>
    inline const char * getDynamicClassName(T& v)
    {
        return typeid(v).name();
    }

    /**
        @brief ���������Ԫ�ظ���
    */
    template<typename T, uint32 len>
    inline uint32 countOf(T(&)[len])
    {
        return len;
    }

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
    template<typename Type>
    inline const String toString(const Type &i)
    {
        return static_cast<OStringStream&>(OStringStream() << i).str();
    }

    /**
        @brief ׼�ػ�String, ���Ч��
    */
    inline const String& toString(const String& i)
    {
        return i;
    }

    /**
        @brief ׼�ػ�char        
        ����ms��ostringstream����bug, ���Զ�char���⴦��
    */
    inline const String toString(char c)
    {
        return String(1, c);
    }

    /**
        @brief ׼�ػ�bool
        ʹ��true/false
    */
    inline const String toString(bool b)
    {
        return static_cast<OStringStream&>(
            OStringStream() << std::boolalpha << b).str();
    }

    /**
        @brief ���ַ�������ȡ�ض����͵�����
        @return true��ʾת���ɹ�
    */
    template<typename Type>
    inline bool fromString(Type &obj, const String& szSrc)
    {
        IStringStream si(szSrc);
        si >> obj;
        return si.eof() && !si.fail();
    }

    /**
        @brief �ػ�����ָ��
    */
    template<typename Type>
    inline bool fromString(Type* &obj, const String& szSrc)
    {
        IStringStream si(szSrc);
        si >> std::hex >> reinterpret_cast<uint32&>(obj);
        return si.eof() && !si.fail();
    }

    /**
        @brief ���String����׼�ػ�fromString����
        Ϊ�˱�֤��ȷ��.���ⱻ�ո���и���
    */
    inline bool fromString(String& obj, const String& szSrc)
    {
        obj = szSrc;
        return true;
    }

    /**
        @brief ׼�ػ�bool
        ʹ��true/false
    */
    inline bool fromString(bool &b, const String& szSrc)
    {
        IStringStream si(szSrc);
        si >> std::boolalpha >> b;
        return si.eof() && !si.fail();
    }

    /**
        @brief ���������ͱ�����0
    */
    template<typename T>
    inline void toZero(T &v)
    {
        memset(&v, 0, sizeof(v));
    }

    /**
        @brief ��Դ������, raii
    */
    class ResGuard
    {
    public:
        /**
            @brief ���캯��
            @param f Ҫ���õĺ���; ���������Ա����
            @param p ���ú����Ĳ���; ��������ָ��
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
            @brief ��ֹ����
        */
        ResGuard(const ResGuard&);
        ResGuard& operator = (const ResGuard&);

    private:
        boost::function<void(void)> m_f;
    };

    /**
        @brief ���ͷ�����
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
        @brife ��С�ĸ�����
    */
    extern const float FLOAT_EPSILON;

    /**
        @brief ����ĸ�����
    */
    extern const float FLOAT_INFINITY;

    /**
        @brief һ�����ػ�locale
        ��������������, locale���Ϊansi
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
        @brief ��std::type_info�İ�װ
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