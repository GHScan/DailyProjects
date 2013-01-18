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
        @brief 可以作为任意类型载体的类型
        相当于在void*之上增加了强类型的检查, 不支持隐式转换
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
            @brief 提取内部的数据
        */
        template<typename T>
        const T& get() const
        {
            return boost::any_cast<const T&>(m_data);
        }

        /**
            @brief 提取内部数据; 便利语法
        */
        template<typename T>
        void get(T& p) const
        {
            p = get<T>();
        }

        /**
            @brief 设置内部数据
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
        @brief 可将内部数据string化的any
        @remarks 对于指针, 不想解引用则使用toString, fromString, 否则使用toStringDeRef,
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
            @brief 由内部数据转化到String
            @remarks 指针会被当做整数处理; 如果需要得到指针指向的值, 使用toStringDeRef
        */
        const String toString() const
        {
            return m_method->toString(m_any);
        }

        /**
            @brief 将内部的数据String化
            指针会被解引用, 非指针则和toString等效
        */
        const String toStringDeRef() const
        {
            return m_method->toStringDeRef(m_any);
        }

        /**
            @brief 根据String, 重置内部数据
            @remarks 指针被当做整数; 需要解引用请用fromStringDeRef
            @remarks 在调用这个函数之前, 需确保内部存在数据, 即调用过set
        */
        bool fromString(const String& s)
        {
            return m_method->fromString(m_any, s);
        }

        /**
            @brief 根据String, 重置内部数据
            指针会被解引用, 非指针等效fromString
            @remarks 确保之前至少掉用过一次set
        */
        bool fromStringDeRef(const String& s)
        {
            return m_method->fromStringDeRef(m_any, s);
        }

    private:
        /**
            @brief Stringize方法
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
            @brief 针对指针的解引用方法
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
            @brief 不解引用方法
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
            @brief 具体的Stringize方法
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
        @brief 便利函数makeAnyList, 方便生成Any数组
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