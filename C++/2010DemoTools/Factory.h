#pragma once

#include <cassert>

#include <map>

#include <boost/function.hpp>
#include <boost/shared_ptr.hpp>

#include "Types.h"
#include "Utility.h"
#include "Singleton.h"
#include "Preprocessor.h"

namespace Scan
{
    namespace Factory
    {
        /**
            @brief 创建指定接口下的一个对象
            相当于createObject<InterfaceType>(getStaticName(DerivedType))
            @remarks DerivedType需要已经SCAN_FACTORY_REGISTER注册过
            @return 没有查找到指定类型返回NULL
        */
        template<typename InterfaceType, typename DerivedType>
        inline InterfaceType* createObject();
        template<typename InterfaceType, typename DerivedType>
        inline boost::shared_ptr<InterfaceType> createObjectSptr();

        /**
            @brief 创建指定接口下的对象; 语法简化
        */
        template<typename DerivedType, typename InterfaceType>
        inline void createObject(InterfaceType* &p);
        template<typename DerivedType, typename InterfaceType>
        inline void createObjectSptr(boost::shared_ptr<InterfaceType> &p);

        /**
            @brief 创建指定接口下的一个对象
            @remarks objName 描述的类型应该已经被SCAN_FACTORY_REGISTER注册
        */
        template<typename InterfaceType>
        inline InterfaceType* createObject(const String& objName);
        template<typename InterfaceType>
        inline boost::shared_ptr<InterfaceType> createObjectSptr(const String& objName);

        /**
            @brief 创建指定接口下的一个对象; 语法简化
        */
        template<typename InterfaceType>
        inline void createObject(InterfaceType* &p, const String& objName);
        template<typename InterfaceType>
        inline void createObjectSptr(boost::shared_ptr<InterfaceType> &p, const String& objName);

        /**
            @brief 查询某对象的动态类型
            @return 如果该动态类型并没有注册过, 返回空串
        */
        template<typename InterfaceType>
        inline const String getValidDynamicClassName(const InterfaceType *p);

        /** 
            @brief 将某接口指针转化为另一种接口指针
            @remarks 两种接口都需要SCAN_FACTORY_REGISTER注册
            @return 如果实际对象不是多重继承这两种接口, 返回NULL; 否则会返回偏移正确的指针
        */
        template<typename DestInterface, typename SrcInterface>
        inline DestInterface* & queryInterface(DestInterface* &destPtr, SrcInterface* srcPtr);

        /**
            @brief 将一种类型注册到某个接口下
            应该在全局空间使用这个宏, 接口和实际类应该有派生关系
        */
#define SCAN_FACTORY_REGISTER(baseType, derivedType)   \
    namespace Scan  \
        { \
            namespace Factory \
            { \
                namespace TypeRegisterSpace \
                { \
                    TypeRegister<baseType, derivedType> SCAN_PP_CAT(g_typeRegister, __LINE__); \
                } \
            } \
        }

        /**
            @brief 静态工厂管理器
        */
        template<typename InterfaceType>
        class ObjectFactory:
            public Singleton<ObjectFactory<InterfaceType>>
        {
        public:
            /**
                @brief 创建一个实体类的对象
            */
            InterfaceType* createObject(const String& name) const
            {
                ClassMap_CIter iter = m_classes.find(name);
                if (iter != m_classes.end())
                {
                    return iter->second.creator();
                }
                return NULL;
            }

            /**
                @brief 查询一个实体类的动态类型
            */
            const String getObjectName(const InterfaceType* p) const
            {
                String name = getDynamicClassName(*p);
                return m_classes.count(name) == 0 ? "" : name;
            }

            /**
                @brief 将一个实体类对象的接口指针转化为实际的对象指针
                @return 如果该对象没注册, 返回NULL; 否则指针会正确偏移
            */
            const void* castToObjectPtr(const InterfaceType* p, const String& name = "") const
            {
                String objName = name.empty() ? getDynamicClassName(*p) : name;
                ClassMap_CIter iter = m_classes.find(objName);
                if (iter != m_classes.end())
                {   
                    return iter->second.toCastor(p);
                }
                return NULL;
            }

            /**
                @brief 从一个实体对象的指针返回正确偏移后的接口指针
                @return 如果对象没注册返回NULL
            */
            const InterfaceType* castFromObjectPtr(const void* p, const String& name) const
            {
                ClassMap_CIter iter = m_classes.find(name);
                if (iter != m_classes.end())
                {
                    return iter->second.fromCastor(p);
                }
                return NULL;
            }

            /**
                @brief 注册一个派生类型
            */
            template<typename DerivedType>
            void registerType()
            {
                String name = getStaticClassName<DerivedType>();
                assert(m_classes.count(name) == 0);

                typedef ObjectTraitsImpl<DerivedType> ObjectTraitsImpl;
                ObjectTraits objTraits;
                objTraits.creator       = &ObjectTraitsImpl::create;
                objTraits.toCastor      = &ObjectTraitsImpl::toCast;
                objTraits.fromCastor    = &ObjectTraitsImpl::fromCast;

                m_classes[name] = objTraits;
            }

        private:
            typedef boost::function<InterfaceType*()>                   Creator;
            typedef boost::function<const void*(const InterfaceType*)>  ToCastor;
            typedef boost::function<const InterfaceType*(const void*)>  FromCastor;

            struct ObjectTraits
            {
                Creator     creator;
                ToCastor    toCastor;
                FromCastor  fromCastor;
            };

            typedef std::map<String, ObjectTraits>      ClassMap;
            typedef typename ClassMap::iterator         ClassMap_Iter;
            typedef typename ClassMap::const_iterator   ClassMap_CIter;
            typedef typename ClassMap::value_type       ClassMap_Value;

            /**
                @brief 提供正确的转化方法
            */
            template<typename DerivedType>
            struct ObjectTraitsImpl
            {
                static InterfaceType* create()
                {
                    return ClassAllocator<DerivedType>::alloc();
                }

                static const void* toCast(const InterfaceType* p)
                {
                    return static_cast<const DerivedType*>(p);
                }

                static const InterfaceType* fromCast(const void* p)
                {
                    return static_cast<const DerivedType*>(p);
                }
            };

        private:
            ClassMap        m_classes;
        };

        /**
            @brief 类型注册器
        */
        template<typename InterfaceType, typename DerivedType>
        class TypeRegister
        {
        public:
            TypeRegister()
            {
                // 因为对Singleton的调用时全局的, 所以参数为true
                ObjectFactory<InterfaceType>::getSingletonPtr(true)->registerType<DerivedType>();
            }
        };

        template<typename InterfaceType, typename DerivedType>
        inline InterfaceType* createObject()
        {
            return createObject<InterfaceType>(getStaticClassName<DerivedType>());
        }

        template<typename InterfaceType>
        inline InterfaceType* createObject(const String& objName)
        {
            return ObjectFactory<InterfaceType>::getSingletonPtr()->createObject(objName);
        }

        template<typename InterfaceType, typename DerivedType>
        inline boost::shared_ptr<InterfaceType> createObjectSptr()
        {
            return boost::shared_ptr<InterfaceType>(createObject<InterfaceType, DerivedType>());
        }

        template<typename InterfaceType>
        inline boost::shared_ptr<InterfaceType> createObjectSptr(const String& objName)
        {
            return boost::shared_ptr<InterfaceType>(createObject<InterfaceType>(objName));
        }

        template<typename DestInterface, typename SrcInterface>
        inline DestInterface* & queryInterface(DestInterface* &destPtr, SrcInterface* srcPtr)
        {
            ObjectFactory<SrcInterface> *srcFactory = ObjectFactory<SrcInterface>::getSingletonPtr();
            ObjectFactory<DestInterface> *destFactory = ObjectFactory<DestInterface>::getSingletonPtr();

            destPtr = NULL;
            
            String objectName = srcFactory->getObjectName(srcPtr);
            if (!objectName.empty())
            {
                const void* objecPtr = srcFactory->castToObjectPtr(srcPtr, objectName);
                destPtr = const_cast<DestInterface*>(destFactory->castFromObjectPtr(objecPtr, objectName));
            }

            return destPtr;
        }

        template<typename InterfaceType>
        inline const String getValidDynamicClassName(const InterfaceType *p)
        {
            return ObjectFactory<InterfaceType>::getSingletonPtr()->getObjectName(p);
        }

        template<typename DerivedType, typename InterfaceType>
        inline void createObject(InterfaceType* &p)
        {
            p = createObject<InterfaceType, DerivedType>();
        }

        template<typename DerivedType, typename InterfaceType>
        inline void createObjectSptr(boost::shared_ptr<InterfaceType> &p)
        {
            p = createObjectSptr<InterfaceType, DerivedType>();
        }

        template<typename InterfaceType>
        inline void createObject(InterfaceType* &p, const String& objName)
        {
            p = createObject<InterfaceType>(objName);
        }

        template<typename InterfaceType>
        inline void createObjectSptr(boost::shared_ptr<InterfaceType> &p, const String& objName)
        {
            p = createObjectSptr<InterfaceType>(objName);
        }
    }
}