#pragma once

#include <cassert>

#include <map>

#include "Any.h"
#include "Types.h"
#include "PlatformDepends.h"

namespace Scan
{
    namespace Property
    {
        /**
            @brief ���Լ��ϵķ�����
        */
        struct IPropertySetVisitor
        {
            virtual ~IPropertySetVisitor() = 0 {}

            /**
                @brief ���ʼ����е�һ������
                @param propertyName ������
                @param data ��������
            */
            virtual void visit(const String& propertyName, const Any& data) = 0;
        };

        /**
            @brief ���Լ���
            ����<String, Any>�Եļ���
        */
        template<
            typename MutexType = EmptyMutex,
            typename MapType = std::map<String, Any>>
        class PropertySet
        {
        public:
            /**
                @brief ���һ������
            */
            template<typename PropertyType>
            void addProperty(const String& propertyName, const PropertyType& v)
            {
                SingleLocker locker(m_mutex);
                assert(m_propertys.count(propertyName) == 0);
                m_propertys[propertyName] = Any(v);
            }

            /**
                @brief �Ƴ�һ������
            */
            void removeProperty(const String& propertyName)
            {
                SingleLocker locker(m_mutex);
                m_propertys.erase(propertyName);
            }

            /**
                @brief �޸�һ�����Ե�ֵ
            */
            template<typename PropertyType>
            bool setProperty(const String& propertyName, const PropertyType& v)
            {
                SingleLocker locker(m_mutex);
                NamePropertyMap_Iter iter = m_propertys.find(propertyName);
                if (iter != m_propertys.end())
                {
                    iter->second.set(v);
                    return true;
                }
                return false;
            }

            /**
                @brief �������Ե�ֵ
            */
            template<typename PropertyType>
            bool getProperty(const String& propertyName, PropertyType& v)
            {
                SingleLocker locker(m_mutex);
                NamePropertyMap_CIter iter = m_propertys.find(propertyName);
                if (iter != m_propertys.end())
                {
                    iter->second.get(v);
                    return true;
                }
                return false;
            }

            /**
                @brief ���ܷ����߷���
            */
            void accept(IPropertySetVisitor *p) const
            {
                NamePropertyMap cloneMap;

                {
                    SingleLocker locker(m_mutex);
                    cloneMap = m_propertys;
                }

                for (NamePropertyMap_CIter iter = cloneMap.begin();
                    iter != cloneMap.end();
                    ++iter)
                {
                    p->visit(iter->first, iter->second);
                }
            }

            bool isEmpty() const
            {
                SingleLocker locker(m_mutex);
                return m_propertys.empty();
            }

            uint32 getSize() const
            {
                SingleLocker locker(m_mutex);
                return m_propertys.size();
            }

            void clear()
            {
                SingleLocker locker(m_mutex);
                m_propertys.clear();
            }

        private:
            typedef MapType                                     NamePropertyMap;
            typedef typename NamePropertyMap::iterator          NamePropertyMap_Iter;
            typedef typename NamePropertyMap::const_iterator    NamePropertyMap_CIter;

        private:
            MutexType           m_mutex;
            NamePropertyMap     m_propertys;
        };
    }
}