#pragma once

#include <boost/any.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/mpl/if.hpp>
#include <boost/type_traits.hpp>

#include "Types.h"
#include "Tools.h"
#include "MemoryAllocation.h"
#include "Preprocessor.h"

namespace Scan
{
    /**
        @brief ������Ϊ�����������������
        �൱����void*֮��������ǿ���͵ļ��, ��֧����ʽת��
    */
    class Any
    {
    public:
        Any(){}

        template<typename T>
        explicit Any(const T &p)
        {
            set(p);
        }

        /**
            @brief ��ȡ�ڲ�������
        */
        template<typename T>
        const T& get() const
        {
            return boost::any_cast<const T&>(m_data);
        }

        /**
            @brief ��ȡ�ڲ�����; �����﷨
        */
        template<typename T>
        void get(T& p) const
        {
            p = get<T>();
        }

        /**
            @brief �����ڲ�����
        */
        template<typename T>
        void set(const T &p)
        {
            m_data = p;
        }

    private:
        boost::any  m_data;
    };

    /**
        @brief �ɽ��ڲ�����string����any
        @remarks ����ָ��, �����������ʹ��toString, fromString, ����ʹ��toStringDeRef,
        fromStringDeRef
    */
    class PrintableAny
    {
    public:
        PrintableAny(){}

        template<typename T>
        explicit PrintableAny(const T &p)
        {
            set(p);
        }

        template<typename T>
        const T& get() const
        {
            return m_any.get<T>();
        }

        template<typename T>
        void get(T& p) const
        {
            m_any.get(p);
        }

        template<typename T>
        void set(const T &p)
        {
            m_any.set(p);
            m_method.reset(new PrintMethod<T>);
        }

        /**
            @brief ���ڲ�����ת����String
            @remarks ָ��ᱻ������������; �����Ҫ�õ�ָ��ָ���ֵ, ʹ��toStringDeRef
        */
        const String toString() const
        {
            return m_method->toString(m_any);
        }

        /**
            @brief ���ڲ�������String��
            ָ��ᱻ������, ��ָ�����toString��Ч
        */
        const String toStringDeRef() const
        {
            return m_method->toStringDeRef(m_any);
        }

        /**
            @brief ����String, �����ڲ�����
            @remarks ָ�뱻��������; ��Ҫ����������fromStringDeRef
            @remarks �ڵ����������֮ǰ, ��ȷ���ڲ���������, �����ù�set
        */
        bool fromString(const String& s)
        {
            return m_method->fromString(m_any, s);
        }

        /**
            @brief ����String, �����ڲ�����
            ָ��ᱻ������, ��ָ���ЧfromString
            @remarks ȷ��֮ǰ���ٵ��ù�һ��set
        */
        bool fromStringDeRef(const String& s)
        {
            return m_method->fromStringDeRef(m_any, s);
        }

    private:
        /**
            @brief Stringize����
        */
        struct IPrintMethod
        {
            virtual ~IPrintMethod() = 0 {}
            virtual const String toString(const Any &a) const = 0;
            virtual const String toStringDeRef(const Any &a) const = 0;
            virtual bool fromString(Any &a, const String &s) const = 0;
            virtual bool fromStringDeRef(Any &a, const String &s) const = 0;
        };

        /**
            @brief ���ָ��Ľ����÷���
        */
        template<typename T>
        struct StringizeImpl_DeRef
        {
            static const String toString(const Any &a)
            {
                return Scan::toString(*a.get<T>());
            }
            static bool fromString(Any &a, const String &s)
            {
                return Scan::fromString(*a.get<T>(), s);
            }
        };

        /**
            @brief �������÷���
        */
        template<typename T>
        struct StringizeImpl_NoDeRef
        {
            static const String toString(const Any &a)
            {
                return Scan::toString(a.get<T>());
            }
            static bool fromString(Any &a, const String &s)
            {
                T p;
                bool ret = Scan::fromString(p, s);
                a.set(p);
                return ret;
            }
        };

        /**
            @brief �����Stringize����
        */
        template<typename T>
        class PrintMethod:
            public IPrintMethod,
            public MemoryAllocation::SizefixedMemAllocObject<T>
        {
            typedef StringizeImpl_NoDeRef<T>      PrintImpl_NoDeRef;

            typedef typename 
                boost::mpl::if_<
                boost::is_pointer<T>, 
                StringizeImpl_DeRef<T>, 
                StringizeImpl_NoDeRef<T>>::type   PrintImpl_DeRef;

            virtual const String toString(const Any& a) const
            {
                return PrintImpl_NoDeRef::toString(a);
            }
            virtual const String toStringDeRef(const Any& a) const
            {              
                return PrintImpl_DeRef::toString(a);
            }
            virtual bool fromString(Any &a, const String &s) const
            {
                return PrintImpl_NoDeRef::fromString(a, s);
            }
            virtual bool fromStringDeRef(Any &a, const String &s) const
            {
                return PrintImpl_DeRef::fromString(a, s);
            }
        };

    private:
        typedef boost::shared_ptr<IPrintMethod>  PrintMethodPtr;

    private:
        PrintMethodPtr  m_method;
        Any             m_any;
    };

    typedef std::vector<Any>    AnyList;

    /**
        @brief ��������makeAnyList, ��������Any����
    */
#define _SCAN_PUSH_ANY_LIST(n)   list.push_back(Any(SCAN_PP_ENUM_PARAM(n)))
#define _SCAN_MAKE_ANY_LIST_N(n) \
    template<SCAN_PP_REPEAT_TYPENAME_TYPE_COMMA(n)> \
    inline AnyList makeAnyList(SCAN_PP_REPEAT_TYPE_PARAM_COMMA(n)) \
    { \
        AnyList list; \
        SCAN_PP_REPEAT_DATA_COMMA(n, _SCAN_PUSH_ANY_LIST); \
        return list; \
    }

    SCAN_PP_REPEAT_LIMITED_DATA_EMPTY(1, 32, _SCAN_MAKE_ANY_LIST_N);

#undef _SCAN_MAKE_ANY_LIST_N
#undef _SCAN_PUSH_ANY_LIST
}