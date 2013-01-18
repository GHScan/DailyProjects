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
            @brief 属性集合的访问者
        */
        struct IPropertySetVisitor
        {
            virtual ~IPropertySetVisitor() = 0 {}

            /**
                @brief 访问集合中的一个属性
                @param propertyName 属性名
                @param data 属性数据
            */
            virtual void visit(const String& propertyName, const Any& data) = 0;
        };

        /**
            @brief 属性集合
            就是<String, Any>对的集合
        */
        template<
            typename MutexType = EmptyMutex,
            typename MapType = std::map<String, Any>>
        class PropertySet
        {
        public:
            /**
                @brief 添加一个属性
            */
            template<typename PropertyType>
            void addProperty(const String& propertyName, const PropertyType& v)
            {
                SingleLocker locker(m_mutex);
                assert(m_propertys.count(propertyName) == 0);
                m_propertys[propertyName] = Any(v);
            }

            /**
                @brief 移除一个属性
            */
            void removeProperty(const String& propertyName)
            {
                SingleLocker locker(m_mutex);
                m_propertys.erase(propertyName);
            }

            /**
                @brief 修改一个属性的值
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
                @brief 返回属性的值
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
                @brief 接受访问者访问
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