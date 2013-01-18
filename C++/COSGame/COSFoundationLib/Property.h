#pragma once

#include <vector>
#include <map>
#include <sstream>

#include <boost/any.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/signal.hpp>
#include <boost/mpl/if.hpp>
#include <boost/type_traits.hpp>

#include "TypeDefine.h"
#include "Utility.h"

/**
    @brief 特点:
        1. any的特性
        2. 对象采用共享存储
        3. set/define/undefine 会触发valueChanged的信号
        4. getPtr, getRawPtr得到指针后, 修改值不会触发信号
        5. 序列化支持(默认对pod能够进行二进制序列化, 对支持iostream的<<,>>对象能进行字符串化)
 */

namespace Scan
{

class Property:
    private Copyable<true>
{
public:
    // 用户可以选择修改这个值来自己定义序列化方法
    template<typename T>
    struct BinarySerializeMode_UseDefault
    {
        // 默认pod已经实现
        enum { value = boost::is_pod<T>::value };
    };
    // PropertyBinarySerializeMode_UseDefault::value为0时, 用户自己实现二进制序列化方法
    template<typename T>
    struct BinarySerializeHelper_NotDefault
    {
        // 返回-1表示失败, 否则返回拷贝的字节数
        static size_t toBytes(const T& src, void *buffer, size_t bufferSize)
        {
            return -1;
        }
        static bool fromBytes(T& dest, const void *buffer, size_t bufferSize)
        {
            return false;
        }
    };

    // 如果把下面这个开启, 必须保证实现了>>和<<
    template<typename T>
    struct StringSerializeMode_UseDefault
    {
        // 默认基本类型已经实现
        enum { value = boost::is_fundamental<T>::value };
    };

    // 用户自定义实现
    template<typename T>
    struct StringSerializeHelper_NotDefault
    {
        static const String toString(const T& val)
        {
            return String();
        }
        static bool fromString(T& val, const String& s)
        {
            return false;
        }
    };

public:
    Property(): m_serializer(NULL){}

    template<typename T>
    explicit Property(const T& val): m_serializer(NULL){ set(val); }

    ~Property() { m_serializer = NULL; }

    template<typename T>
    boost::shared_ptr<T> getPtr()
    {
        define<T>();
        return boost::any_cast<boost::shared_ptr<T>&>(m_propImpl);
    }

    template<typename T>
    boost::shared_ptr<const T> getPtr() const
    {
        ASSERT(!isNull());
        return boost::any_cast<const boost::shared_ptr<T>&>(m_propImpl);
    }

    template<typename T>
    T* getRawPtr(T** pp = NULL)
    {
        define<T>();
        if (pp != NULL)
        {
            *pp = getPtr<T>().get();
            return *pp;
        }
        return getPtr<T>().get();
    }

    template<typename T>
    const T* getRawPtr() const
    {
        ASSERT(!isNull());
        return getPtr<T>().get();
    }

    template<typename T>
    const T get(T* p = NULL) const
    {
        ASSERT(!isNull());
        if (p != NULL)
        {
            *p = *getPtr<T>();
            return *p;
        }
        return *getPtr<T>();
    }

    template<typename T>
    void set(const T& newVal)
    {
        define<T>();
        if (get<T>() != newVal)
        {
            *getPtr<T>() = newVal;
            if (m_valueChangedSig != NULL) (*m_valueChangedSig)(*this);
        }
    }

    boost::signal<void(Property&)>& valueChanged()
    {
        if (m_valueChangedSig == NULL) m_valueChangedSig.reset(new boost::signal<void(Property&)>);
        return *m_valueChangedSig;
    }

    template<typename T>
    void define()
    {
        if (m_propImpl.empty())
        {
            m_propImpl = boost::shared_ptr<T>(new T);
        
            ASSERT(m_serializer == NULL);
            m_serializer = getSerializer<T>();

            if (m_valueChangedSig != NULL) (*m_valueChangedSig)(*this);
        }
        else ASSERT(m_propImpl.type() == typeid(boost::shared_ptr<T>));
    }

    void undefine()
    {
        if (!m_propImpl.empty())
        {
            m_propImpl = boost::any();

            ASSERT(m_serializer != NULL);
            m_serializer = NULL;

            if (m_valueChangedSig != NULL) (*m_valueChangedSig)(*this);
        }
    }

    bool isNull() const
    {
        return m_propImpl.empty();
    }

    template<typename T>
    bool isTypeOf() const
    {
        ASSERT(!isNull());
        return m_propImpl.type() == typeid(boost::shared_ptr<T>);
    }

    const String toString() const
    {
        ASSERT(!isNull());
        return m_serializer->toString(m_propImpl);
    }

    bool fromString(const String& s)
    {
        ASSERT(!isNull());
        return m_serializer->fromString(m_propImpl, s);
    }

    // 返回-1表示失败, 否则返回拷贝的字节数
    size_t toBytes(void *buffer, size_t bufferSize) const
    {
        ASSERT(!isNull());
        return m_serializer->toBytes(m_propImpl, buffer, bufferSize);
    }

    bool fromBytes(const void *buffer, size_t bufferSize)
    {
        ASSERT(!isNull());
        return m_serializer->fromBytes(m_propImpl, buffer, bufferSize);
    }

private:
    class ISerializer
    {
    public:
        virtual const String toString(const boost::any& a) const = 0;
        virtual bool fromString(boost::any &a, const String& s) const = 0;
        virtual size_t toBytes(const boost::any& a, void *buffer, size_t bufferSize) const = 0;
        virtual bool fromBytes(boost::any &a, const void *buffer, size_t bufferSize) const = 0;
        virtual ~ISerializer(){}
    };

    template<typename T>
    struct BinarySerializeHelper_Default
    {
        static size_t toBytes(const T& src, void *buffer, size_t bufferSize)
        {
            if (sizeof(src) == 0) return 0;
            if (sizeof(src) > bufferSize) return -1;
            memcpy(buffer, &src, sizeof(src));
            return sizeof(src);
        }
        static bool fromBytes(T& dest, const void *buffer, size_t bufferSize)
        {
            if (sizeof(dest) == 0) return true;
            if (sizeof(dest) > bufferSize) return false;
            memcpy(&dest, buffer, sizeof(dest));
            return true;
        }
    };

    template<typename T>
    struct StringSerializeHelper_Default
    {
        static const String toString(const T& val)
        {
            return Scan::toString(val);
        }
        static bool fromString(T& val, const String& s)
        {
            return Scan::fromString(val, s);
        }
    };

    template<typename T>
    class Serializer:
        public ISerializer
    {
    public:
        typedef typename boost::mpl::if_<
            boost::mpl::bool_<BinarySerializeMode_UseDefault<T>::value>, 
            BinarySerializeHelper_Default<T>, 
            BinarySerializeHelper_NotDefault<T>>::type   BinrarySerializeHelper;

        typedef typename boost::mpl::if_<
            boost::mpl::bool_<StringSerializeMode_UseDefault<T>::value>, 
            StringSerializeHelper_Default<T>, 
            StringSerializeHelper_NotDefault<T>>::type   StringSerializeHelper;

    public:
        virtual const String toString(const boost::any& a) const 
        {
            return StringSerializeHelper::toString(*boost::any_cast<const boost::shared_ptr<T>&>(a));
        }
        virtual bool fromString(boost::any &a, const String& s) const 
        {
            return StringSerializeHelper::fromString(*boost::any_cast<boost::shared_ptr<T>&>(a), s);
        }

        virtual size_t toBytes(const boost::any& a, void *buffer, size_t bufferSize) const
        {
            return BinrarySerializeHelper::toBytes(*boost::any_cast<const boost::shared_ptr<T>&>(a), buffer, bufferSize);
        }

        virtual bool fromBytes(boost::any &a, const void *buffer, size_t bufferSize) const 
        {
            return BinrarySerializeHelper::fromBytes(*boost::any_cast<boost::shared_ptr<T>&>(a), buffer, bufferSize);
        }
    };

    template<typename T>
    static ISerializer* getSerializer()
    {
        typedef std::map<TypeInfoWrapper, boost::shared_ptr<ISerializer>>  SerializerMap;
        static SerializerMap  ls_mp;

        TypeInfoWrapper info(typeid(T));
        SerializerMap::iterator iter = ls_mp.lower_bound(info);
        if (iter == ls_mp.end() || iter->first != info)
        {
            return ls_mp.insert(iter, SerializerMap::value_type(info, boost::shared_ptr<ISerializer>(new Serializer<T>)))->second.get();
        }
        else return iter->second.get();
    }

private:
    boost::any      m_propImpl;
    ISerializer    *m_serializer;
    boost::shared_ptr<boost::signal<void(Property&)> >   
                    m_valueChangedSig;
};

}