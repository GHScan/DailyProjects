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
            @brief ����ָ���ӿ��µ�һ������
            �൱��createObject<InterfaceType>(getStaticName(DerivedType))
            @remarks DerivedType��Ҫ�Ѿ�SCAN_FACTORY_REGISTERע���
            @return û�в��ҵ�ָ�����ͷ���NULL
        */
        template<typename InterfaceType, typename DerivedType>
        inline InterfaceType* createObject();
        template<typename InterfaceType, typename DerivedType>
        inline boost::shared_ptr<InterfaceType> createObjectSptr();

        /**
            @brief ����ָ���ӿ��µĶ���; �﷨��
        */
        template<typename DerivedType, typename InterfaceType>
        inline void createObject(InterfaceType* &p);
        template<typename DerivedType, typename InterfaceType>
        inline void createObjectSptr(boost::shared_ptr<InterfaceType> &p);

        /**
            @brief ����ָ���ӿ��µ�һ������
            @remarks objName ����������Ӧ���Ѿ���SCAN_FACTORY_REGISTERע��
        */
        template<typename InterfaceType>
        inline InterfaceType* createObject(const String& objName);
        template<typename InterfaceType>
        inline boost::shared_ptr<InterfaceType> createObjectSptr(const String& objName);

        /**
            @brief ����ָ���ӿ��µ�һ������; �﷨��
        */
        template<typename InterfaceType>
        inline void createObject(InterfaceType* &p, const String& objName);
        template<typename InterfaceType>
        inline void createObjectSptr(boost::shared_ptr<InterfaceType> &p, const String& objName);

        /**
            @brief ��ѯĳ����Ķ�̬����
            @return ����ö�̬���Ͳ�û��ע���, ���ؿմ�
        */
        template<typename InterfaceType>
        inline const String getValidDynamicClassName(const InterfaceType *p);

        /** 
            @brief ��ĳ�ӿ�ָ��ת��Ϊ��һ�ֽӿ�ָ��
            @remarks ���ֽӿڶ���ҪSCAN_FACTORY_REGISTERע��
            @return ���ʵ�ʶ����Ƕ��ؼ̳������ֽӿ�, ����NULL; ����᷵��ƫ����ȷ��ָ��
        */
        template<typename DestInterface, typename SrcInterface>
        inline DestInterface* & queryInterface(DestInterface* &destPtr, SrcInterface* srcPtr);

        /**
            @brief ��һ������ע�ᵽĳ���ӿ���
            Ӧ����ȫ�ֿռ�ʹ�������, �ӿں�ʵ����Ӧ����������ϵ
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
            @brief ��̬����������
        */
        template<typename InterfaceType>
        class ObjectFactory:
            public Singleton<ObjectFactory<InterfaceType>>
        {
        public:
            /**
                @brief ����һ��ʵ����Ķ���
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
                @brief ��ѯһ��ʵ����Ķ�̬����
            */
            const String getObjectName(const InterfaceType* p) const
            {
                String name = getDynamicClassName(*p);
                return m_classes.count(name) == 0 ? "" : name;
            }

            /**
                @brief ��һ��ʵ�������Ľӿ�ָ��ת��Ϊʵ�ʵĶ���ָ��
                @return ����ö���ûע��, ����NULL; ����ָ�����ȷƫ��
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
                @brief ��һ��ʵ������ָ�뷵����ȷƫ�ƺ�Ľӿ�ָ��
                @return �������ûע�᷵��NULL
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
                @brief ע��һ����������
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
                @brief �ṩ��ȷ��ת������
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
            @brief ����ע����
        */
        template<typename InterfaceType, typename DerivedType>
        class TypeRegister
        {
        public:
            TypeRegister()
            {
                // ��Ϊ��Singleton�ĵ���ʱȫ�ֵ�, ���Բ���Ϊtrue
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