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
            @brief 二进制输入文件
            用Factory::createObject创建对象, 后用open来打开一个输入文件
        */
        class SerializeBinaryFileIn; 

        /**
            @brief 二进制输出文件
            用Factory::createObject创建对象, 后用open来打开一个输出文件
        */
        class SerializeBinaryFileOut; 

        /**
            @brief xml输入文件
            用Factory::createObject创建对象, 后用open来打开一个输入文件
        */
        class SerializeXmlFileIn; 

        /**
            @brief xml输出文件
            用Factory::createObject创建对象, 后用open来打开一个输出文件
        */
        class SerializeXmlFileOut;

        /**
            @brief 二进制内存输入
            以std::vector<char>作为数据源, 在open时传入vector地址的字符串
        */
        class SerializeBinaryDataIn; 

        /**
            @brief 二进制内存输出
            以std::vector<char>作为数据目标, 在open时传入vector地址的字符串
        */
        class SerializeBinaryDataOut; 

        /**
            @brief xml字符串输入
            以xml格式的String作为数据源, 在open时传入String
        */
        class SerializeXmlStringIn; 

        /**
            @brief xml字符串输出
            将xml格式的字符串输出到指定String对象中, 在open时传入String作为目标
        */
        class SerializeXmlStringOut;

        typedef boost::shared_ptr<ISerializeFile>   SerializeFilePtr;

        template<typename T>
        inline void serializeContentHook(ISerializeFile &f, T &v, const char *name)
        {
            f.serializeImpl(v, name);
        }

        /**
            @brief 串行化文件接口
        */
        struct ISerializeFile
        {
        public:        
            virtual ~ISerializeFile() = 0 {}

            /** 
                @brief 是否是一个输入文件
            */
            virtual bool isInputFile() const = 0;

            /**
                @brief 返回文件标识
                如果是文件, 返回的就是文件名
            */
            virtual const String& getName() const = 0;

            /**
                @brief 打开一个文件
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
                @brief 串行化一个指针对象
            */
            template<typename T>
            void serializeImpl(T& v, const char *name,
                typename boost::enable_if<boost::is_pointer<T>>::type* = 0)
            {
                serializePtrCast(v, name);
            }

            /**
                @brief 串行化一个非指针对象
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
                // 对于多重继承, 这样可以正确定位偏移
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
                // 对于多重继承, 这样可以正确定位偏移
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
                @brief 串行化一个简单对象
                即非ISerializeable派生类的非容器类
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
                @brief 串行化简单类的指针
            */
            template<typename T> 
            void serializePtr(T*& v, const char *name,
                typename boost::disable_if<IsContainer<T>>::type* = 0)
            {
                // 只有作为输入文件的时候才允许空指针
                assert(v != NULL || isInputFile());
                // 空指针的话, new一个对象
                if (v == NULL)
                {
                    v = ClassAllocator<T>::alloc();
                }
                serializeNotPtr(*v, name);
            }   

            /**
                @brief 串行化一个ISerializeable对象
            */
            void serializeNotPtr(ISerializeable& v, const char *name)
            {
                ISerializeable *p = &v;
                serializePtr(p, name);
            }

            /**
                @brief 串行化一个ISerializeable指针
            */
            void serializePtr(ISerializeable*& v,  const char *name)
            {
                // 只有输入文件才可以处理空指针
                assert(v != NULL || isInputFile());
                // 对于空指针, 输入文件会new正确的子类
                handleSerializeable(v);
            }
            
            /**
                @brief 串行化一个容器
            */
            template<typename ContainerType>
            void serializeNotPtr(ContainerType& v, const char *name, 
                typename boost::enable_if<IsContainer<ContainerType>>::type* = 0)
            {
                typedef ContainerAdapter<ContainerType> Adapter;
                Adapter* list = new Adapter(v);
                uint32 itemCount = list->getSize();

                // 先串行化对象的个数
                serialize(itemCount, "item_count");

                // 然后递归串行化对象
                for (uint32 i = 0; i < itemCount; ++i)
                {
                    // 输入文件会创建对象
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
                @brief 串行化容器指针
            */
            template<typename ContainerType>
            void serializePtr(ContainerType*& p, const char *name,
                typename boost::enable_if<IsContainer<ContainerType>>::type* = 0,
                typename boost::disable_if<boost::is_array<ContainerType>>::type* = 0)
            {
                assert(p != NULL || isInputFile());
                // 遇到空指针, 输入文件会new一个对象
                if (p == NULL)
                {
                    p = ClassAllocator<ContainerType>::alloc();
                }
                serializeNotPtr(*p, name);
            }

        protected:
            /**
                @brief 开始串行化一个对象
            */
            virtual void beginObject(const char *name) = 0;

            /**
                @brief 处理简单对象
            */
            virtual void handleSimpleObject(ISimpleObject *&p) = 0;
            
            /**
                @brief 处理一个复合对象
                如果是输入文件, p为空的话会创建一个对象, 这也是为什么p是指针的
                引用的原因
            */
            virtual void handleSerializeable(ISerializeable *&p) = 0;

            /**
                @brief 串行化结束
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
