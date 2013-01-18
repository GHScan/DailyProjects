#pragma once

#include <boost/mpl/if.hpp>
#include <boost/type_traits.hpp>

#include "Utility.h"
#include "Preprocessor.h"

namespace Scan
{
    namespace Serialize
    {
        /**
            @brief 指出一个对象不支持字符串串行化
            对于一些不支持std::ostream<<的结构, 需要这样才能编译通过
        */
#define SCAN_DISABLE_STRING_SERIALIZE(...)          \
        template<>                                  \
        struct EnableStringSerialize<__VA_ARGS__>   \
        {                                           \
            static const bool value = false;        \
        };

        /**
            @brief 指出一个对象不支持二进制串行化
            对于二进制位并不是实际内容的类型, 可能会想要声明它不支持二进制
            串行化
        */
#define SCAN_DISABLE_BINARY_SERIALIZE(...)          \
        template<>                                  \
        struct EnableBinarySerialize<__VA_ARGS__>   \
        {                                           \
            static const bool value = false;        \
        };

        /**
            @brief 字符串串行化方法
            对于特别的简单类, 可能会想特化这个方法来输出格式字符串
        */
        template<typename T>
        struct StringSerializeMethod
        {
            static void saveToString(String&s, const T& o)
            {
                s = toString(o);
            }
            static void loadFromString(const String& s, T& o)
            {
                fromString(o, s);
            }
        };

        /**
            @brief 二进制串行化方法
            对于特殊的类型, 因为它的二进制位并不是实际内容, 所以可能需要特化
            这个类; 如String
        */
        template<typename T>
        struct BinarySerializeMethod
        {
            static uint32 getSize(const T& o)
            { 
                return sizeof(o);
            }
            static void saveToBytes(void* buf, const T& o)
            {
                memcpy(buf, &o, sizeof(o));
            }
            static void loadFromBytes(const void* buf, T& o)
            {
                memcpy(&o, buf, sizeof(o));
            }
        };

        /**
            @brief 针对String, 特化正确的二进制串行化方法
        */
        template<>
        struct BinarySerializeMethod<String>
        {
            static uint32 getSize(const String& o)
            { 
                return (uint32)o.size() + 1;
            }
            static void saveToBytes(void* buf, const String& o)
            {
                strcpy_s((char*)buf, o.size() + 1, o.c_str());
            }
            static void loadFromBytes(const void* buf, String& o)
            {
                char *p = (char*)buf;
                o.resize(strlen(p));
                strcpy_s((char*)o.c_str(), o.size() + 1, p);
            }
        };

        /**
            @brief 一个类型是否允许字符串串行化
        */
        template<typename T>
        struct EnableStringSerialize
        {
            static const bool value = true;
        };

        /**
            @brief 一个类型是否允许二进制串行化
        */
        template<typename T>
        struct EnableBinarySerialize
        {
            static const bool value = true;
        };

        /**
            @brief 不支持字符串输出的替代方法
        */
        template<typename T>
        struct StringSerializeNotSupport
        {
            static void saveToString(String&s, const T& o)
            {
                s = "not support";
            }
            static void loadFromString(const String& s, T& o)
            {
                ;
            }
        };

        /**
            @brief 不支持二进制输出的替代
        */
        template<typename T>
        struct BinarySerializeNotSupport
        {
            static uint32 getSize(const T& o)
            { 
                return 1;
            }
            static void saveToBytes(void* buf, const T& o)
            {
                
            }
            static void loadFromBytes(const void* buf, T& o)
            {
                
            }
        };

        /**
            @brief 简单类的串行化接口
        */
        struct ISimpleObject
        {
            virtual void saveToString(String& s){}
            virtual void loadFromString(const String& s){}

            virtual uint32 getSize(){ return 0; }
            virtual void saveToBytes(void* buf){}
            virtual void loadFromBytes(const void* buf){}

            virtual ~ISimpleObject() = 0 {}
        };

        /**
            @brief 具体类型的串行化包装
        */
        template<typename T>
        class SimpleObject:
            public ISimpleObject,
            public MemoryAllocation::SizefixedMemAllocObject<SimpleObject<T>>
        {
        public:
            SimpleObject(T& v): m_v(v){}

            typedef typename boost::remove_cv<T>::type RawType;

            typedef typename boost::mpl::if_<
                EnableStringSerialize<RawType>, 
                StringSerializeMethod<RawType>, 
                StringSerializeNotSupport<RawType>>::type 
                StringSerializeMethod;

            typedef typename boost::mpl::if_<
                EnableBinarySerialize<RawType>, 
                BinarySerializeMethod<RawType>, 
                BinarySerializeNotSupport<RawType>>::type 
                BinarySerializeMeThod;


            virtual void saveToString(String& s)
            {
                StringSerializeMethod::saveToString(s, m_v);
            }

            virtual void loadFromString(const String& s)
            {
                StringSerializeMethod::loadFromString(s, m_v);
            }

            virtual uint32 getSize()
            {
                return BinarySerializeMeThod::getSize(m_v);
            }

            virtual void saveToBytes(void* buf)
            {
                BinarySerializeMeThod::saveToBytes(buf, m_v);
            }

            virtual void loadFromBytes(const void* buf)
            {
                BinarySerializeMeThod::loadFromBytes(buf, m_v);
            }

        private:
            T&      m_v;
        };
    }
}