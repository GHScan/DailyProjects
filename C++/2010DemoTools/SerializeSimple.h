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
            @brief ָ��һ������֧���ַ������л�
            ����һЩ��֧��std::ostream<<�Ľṹ, ��Ҫ�������ܱ���ͨ��
        */
#define SCAN_DISABLE_STRING_SERIALIZE(...)          \
        template<>                                  \
        struct EnableStringSerialize<__VA_ARGS__>   \
        {                                           \
            static const bool value = false;        \
        };

        /**
            @brief ָ��һ������֧�ֶ����ƴ��л�
            ���ڶ�����λ������ʵ�����ݵ�����, ���ܻ���Ҫ��������֧�ֶ�����
            ���л�
        */
#define SCAN_DISABLE_BINARY_SERIALIZE(...)          \
        template<>                                  \
        struct EnableBinarySerialize<__VA_ARGS__>   \
        {                                           \
            static const bool value = false;        \
        };

        /**
            @brief �ַ������л�����
            �����ر�ļ���, ���ܻ����ػ���������������ʽ�ַ���
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
            @brief �����ƴ��л�����
            �������������, ��Ϊ���Ķ�����λ������ʵ������, ���Կ�����Ҫ�ػ�
            �����; ��String
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
            @brief ���String, �ػ���ȷ�Ķ����ƴ��л�����
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
            @brief һ�������Ƿ������ַ������л�
        */
        template<typename T>
        struct EnableStringSerialize
        {
            static const bool value = true;
        };

        /**
            @brief һ�������Ƿ���������ƴ��л�
        */
        template<typename T>
        struct EnableBinarySerialize
        {
            static const bool value = true;
        };

        /**
            @brief ��֧���ַ���������������
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
            @brief ��֧�ֶ�������������
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
            @brief ����Ĵ��л��ӿ�
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
            @brief �������͵Ĵ��л���װ
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