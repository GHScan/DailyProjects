#pragma once

#include <cassert>

#include <boost/type_traits.hpp>
#include <boost/shared_ptr.hpp>

#include "MemoryAllocation.h"
#include "Types.h"
#include "Preprocessor.h"
#include "SerializeNameValuePair.h"
#include "SerializeSimple.h"
#include "SerializeContainer.h"
#include "SerializeComposite.h"
#include "SerializeSerializeable.h"


namespace Scan
{
    namespace Serialize
    {   
        /**
            @brief �����������ļ�
            ��Factory::createObject��������, ����open����һ�������ļ�
        */
        class SerializeBinaryFileIn; 

        /**
            @brief ����������ļ�
            ��Factory::createObject��������, ����open����һ������ļ�
        */
        class SerializeBinaryFileOut; 

        /**
            @brief xml�����ļ�
            ��Factory::createObject��������, ����open����һ�������ļ�
        */
        class SerializeXmlFileIn; 

        /**
            @brief xml����ļ�
            ��Factory::createObject��������, ����open����һ������ļ�
        */
        class SerializeXmlFileOut;

        /**
            @brief �������ڴ�����
            ��std::vector<char>��Ϊ����Դ, ��openʱ����vector��ַ���ַ���
        */
        class SerializeBinaryDataIn; 

        /**
            @brief �������ڴ����
            ��std::vector<char>��Ϊ����Ŀ��, ��openʱ����vector��ַ���ַ���
        */
        class SerializeBinaryDataOut; 

        /**
            @brief xml�ַ�������
            ��xml��ʽ��String��Ϊ����Դ, ��openʱ����String
        */
        class SerializeXmlStringIn; 

        /**
            @brief xml�ַ������
            ��xml��ʽ���ַ��������ָ��String������, ��openʱ����String��ΪĿ��
        */
        class SerializeXmlStringOut;

        typedef boost::shared_ptr<ISerializeFile>   SerializeFilePtr;

        template<typename T>
        inline void serializeContentHook(ISerializeFile &f, T &v, const char *name)
        {
            f.serializeImpl(v, name);
        }

        /**
            @brief ���л��ļ��ӿ�
        */
        struct ISerializeFile
        {
        public:        
            virtual ~ISerializeFile() = 0 {}

            /** 
                @brief �Ƿ���һ�������ļ�
            */
            virtual bool isInputFile() const = 0;

            /**
                @brief �����ļ���ʶ
                ������ļ�, ���صľ����ļ���
            */
            virtual const String& getName() const = 0;

            /**
                @brief ��һ���ļ�
            */
            virtual bool open(const String& name) = 0;
            virtual bool isOpened() const = 0;
            virtual void close() = 0;

            template<typename T>
            ISerializeFile& serialize(T& v, const char *name,
                typename boost::disable_if<boost::is_const<T>>::type* = 0)
            {       
                beginObject(name);
                serializeContentHook(*this, v, name);
                endObject();
                return *this;
            }

            template<typename T>
            friend inline void serializeContentHook(ISerializeFile &f, T &v, const char *name);

        private:
            /**
                @brief ���л�һ��ָ�����
            */
            template<typename T>
            void serializeImpl(T& v, const char *name,
                typename boost::enable_if<boost::is_pointer<T>>::type* = 0)
            {
                serializePtrCast(v, name);
            }

            /**
                @brief ���л�һ����ָ�����
            */
            template<typename T>
            void serializeImpl(T& v, const char *name,
                typename boost::disable_if<boost::is_pointer<T>>::type* = 0)
            {
                serializeNotPtrCast(v, name);
            }

            template<typename T>
            void serializeNotPtrCast(T& v, const char *name,
                typename boost::enable_if<boost::is_base_of<ISerializeable, T>>::type* = 0)
            {
                // ���ڶ��ؼ̳�, ����������ȷ��λƫ��
                ISerializeable& r = v;
                serializeNotPtr(r, name);
            }
            template<typename T>
            void serializeNotPtrCast(T& v, const char *name,
                typename boost::disable_if<boost::is_base_of<ISerializeable, T>>::type* = 0)
            {
                serializeNotPtr(v, name);
            }

            template<typename T>
            void serializePtrCast(T& v, const char *name,
                typename boost::enable_if<boost::is_base_of<ISerializeable, 
                typename boost::remove_pointer<T>::type>>::type* = 0)
            {
                // ���ڶ��ؼ̳�, ����������ȷ��λƫ��
                ISerializeable* p = v;
                serializePtr(p, name);
                v = static_cast<T>(p);
            }
            template<typename T>
            void serializePtrCast(T& v, const char *name,
                typename boost::disable_if<boost::is_base_of<ISerializeable, 
                typename boost::remove_pointer<T>::type>>::type* = 0)
            {
                serializePtr(v, name);
            }

        private:
            /**
                @brief ���л�һ���򵥶���
                ����ISerializeable������ķ�������
            */
            template<typename T> 
            void serializeNotPtr(T& v, const char *name,
                typename boost::disable_if<IsContainer<T>>::type* = 0)
            {
                ISimpleObject *p = new SimpleObject<T>(v);
                handleSimpleObject(p);
                delete p;
            }

            /**
                @brief ���л������ָ��
            */
            template<typename T> 
            void serializePtr(T*& v, const char *name,
                typename boost::disable_if<IsContainer<T>>::type* = 0)
            {
                // ֻ����Ϊ�����ļ���ʱ��������ָ��
                assert(v != NULL || isInputFile());
                // ��ָ��Ļ�, newһ������
                if (v == NULL)
                {
                    v = ClassAllocator<T>::alloc();
                }
                serializeNotPtr(*v, name);
            }   

            /**
                @brief ���л�һ��ISerializeable����
            */
            void serializeNotPtr(ISerializeable& v, const char *name)
            {
                ISerializeable *p = &v;
                serializePtr(p, name);
            }

            /**
                @brief ���л�һ��ISerializeableָ��
            */
            void serializePtr(ISerializeable*& v,  const char *name)
            {
                // ֻ�������ļ��ſ��Դ����ָ��
                assert(v != NULL || isInputFile());
                // ���ڿ�ָ��, �����ļ���new��ȷ������
                handleSerializeable(v);
            }
            
            /**
                @brief ���л�һ������
            */
            template<typename ContainerType>
            void serializeNotPtr(ContainerType& v, const char *name, 
                typename boost::enable_if<IsContainer<ContainerType>>::type* = 0)
            {
                typedef ContainerAdapter<ContainerType> Adapter;
                Adapter* list = new Adapter(v);
                uint32 itemCount = list->getSize();

                // �ȴ��л�����ĸ���
                serialize(itemCount, "item_count");

                // Ȼ��ݹ鴮�л�����
                for (uint32 i = 0; i < itemCount; ++i)
                {
                    // �����ļ��ᴴ������
                    if (isInputFile())
                    {
                        list->createNew();
                    }
                    list->moveNext();

                    String itemName = String(name) + "_item_" + toString(i);
                    serialize(list->getCurrentItem(), itemName.c_str());
                }

                delete list;
            }

            /**
                @brief ���л�����ָ��
            */
            template<typename ContainerType>
            void serializePtr(ContainerType*& p, const char *name,
                typename boost::enable_if<IsContainer<ContainerType>>::type* = 0,
                typename boost::disable_if<boost::is_array<ContainerType>>::type* = 0)
            {
                assert(p != NULL || isInputFile());
                // ������ָ��, �����ļ���newһ������
                if (p == NULL)
                {
                    p = ClassAllocator<ContainerType>::alloc();
                }
                serializeNotPtr(*p, name);
            }

        protected:
            /**
                @brief ��ʼ���л�һ������
            */
            virtual void beginObject(const char *name) = 0;

            /**
                @brief ����򵥶���
            */
            virtual void handleSimpleObject(ISimpleObject *&p) = 0;
            
            /**
                @brief ����һ�����϶���
                ����������ļ�, pΪ�յĻ��ᴴ��һ������, ��Ҳ��Ϊʲôp��ָ���
                ���õ�ԭ��
            */
            virtual void handleSerializeable(ISerializeable *&p) = 0;

            /**
                @brief ���л�����
            */
            virtual void endObject() = 0;
        };        
    
        template<typename T>
        ISerializeFile& operator << (ISerializeFile& file, const T& v)
        {   
            assert(!file.isInputFile());
            file.serialize(const_cast<T&>(v), "");
            return file;
        }
        template<typename T>
        ISerializeFile& operator << (ISerializeFile& file, const NVPair<const T>& p)
        {
            assert(!file.isInputFile());
            file.serialize(const_cast<T&>(p.obj), p.name);
            return file;
        }
        template<typename T>
        ISerializeFile& operator << (ISerializeFile& file, const NVPair<T>& p)
        {
            assert(!file.isInputFile());
            file.serialize(p.obj, p.name);
            return file;
        }
        template<typename T>
        ISerializeFile& operator >> (ISerializeFile& file, T& v)
        {
            assert(file.isInputFile());
            file.serialize(v, "");
            return file;
        }
        template<typename T>
        ISerializeFile& operator >> (ISerializeFile& file, const NVPair<T>& p)
        {
            assert(file.isInputFile());
            file.serialize(p.obj, p.name);
            return file;
        }
        template<typename T>
        ISerializeFile& operator & (ISerializeFile& file, T& v)
        {
            return file.isInputFile() ? file >> v : file << v;
        }
        template<typename T>
        ISerializeFile& operator & (ISerializeFile& file, const NVPair<T>& p)
        {
            return file.isInputFile() ? file >> p : file << p;
        }
    }
}
