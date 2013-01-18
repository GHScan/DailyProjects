#pragma once

#include <cassert>
#include <ctime>

#include <set>
#include <map>
#include <queue>
#include <functional>

#include <boost/weak_ptr.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/random.hpp>
#include <boost/bind.hpp>
#include <boost/function.hpp>

#include "PlatformDepends.h"
#include "MetaProgram.h"
#include "Delegate.h"

namespace Scan
{
    /**
            @brief 迭代器接口
        */
        template<typename ValueType>
        struct IIterator
        {
            virtual bool isExistMore() const = 0;
            virtual ValueType& getNext() = 0;
            virtual ValueType& peekNext() const = 0;
            virtual IIterator& move(int step = 1) = 0;

            virtual IIterator* copy() const = 0;
            virtual ~IIterator() = 0 {}
        };

        /**
            @brief 遍历指定类型容器或数组的迭代器
            要求容器支持stl标准
        */
        template<typename ContainerType, bool reverse>
        class ContainerIterator:
            public IIterator<typename MPL::ContainerTraits<ContainerType, reverse>::ValueType>
        {
        public:
            typedef MPL::ContainerTraits<ContainerType, reverse> HelperType;
            typedef typename HelperType::IterType   IterType;
            typedef typename HelperType::ValueType  ValueType;

        public:
            ContainerIterator(ContainerType& c): m_con(c)
            {
                m_iter = HelperType::begin(m_con);
                m_end  = HelperType::end(m_con);
            }

            virtual bool isExistMore() const
            {
                return m_iter != m_end;
            }

            virtual ValueType& getNext() 
            {
                ValueType &r = *m_iter;
                HelperType::move(m_iter);
                return r;
            }

            virtual ValueType& peekNext() const
            {
                ValueType &r = *m_iter;
                return r;
            }

            virtual IIterator* copy() const
            {
                return new ContainerIterator(*this);
            }

            virtual IIterator& move(int step = 1)
            {
                HelperType::move(m_iter, step);
                return *this;
            }

            template<typename NewValueType>
            IIterator<NewValueType>* castTo()
            {
                SCAN_ASSERT_TYPECAST(NewValueType, ValueType);
                return reinterpret_cast<IIterator<NewValueType>*>(this);
            }

        private:
            ContainerType      &m_con;
            IterType            m_iter;
            IterType            m_end;
        };

        /**
            @brief 能够遍历各种类型stl容器和数组的迭代器
        */
        template<typename ValueType, bool reverse = false>
        class Iterator:
            IIterator<ValueType>
        {
        private:
            typedef IIterator<ValueType>*        IterPtr;

        public:
            template<typename ContainerType>
            explicit Iterator(ContainerType& c)
            {
                constructDispatch(c);
            }    

            Iterator()
            {
                m_iter = NULL;
            }

            ~Iterator()
            {
                safe_delete(m_iter);
            }

            Iterator(const Iterator& o)
            {
                copyConstruct(o);
            }

            Iterator& operator = (const Iterator& o)
            {
                if (this != &o)
                {
                    safe_delete(m_iter);

                    if (o.m_iter != NULL)
                    {
                        m_iter = o.m_iter->copy();
                    }
                }

                return *this;
            }

            /**
                @brief 是否存在更多的元素
            */
            virtual bool isExistMore() const
            {
                return m_iter->isExistMore();
            }

            /**
                @brief 获取下一个元素并移动迭代器
            */
            virtual ValueType& getNext()
            {
                return m_iter->getNext();
            }

            /**
                @brief 获取下一个元素但不移动迭代器
            */
            virtual ValueType& peekNext() const
            {
                return m_iter->peekNext();
            }

            /**
                @brief 拷贝迭代器
            */
            virtual IIterator *copy() const
            {
                return new Iterator(*this);
            }

            /**
                @brief 移动迭代器
            */
            virtual IIterator& move(int step = 1)
            {
                m_iter->move(step);
                return *this;
            }

            /**
                @brief 将迭代器重新绑定到容器
            */
            template<typename ContainerType>
            void rebind(ContainerType& c)
            {
                m_iter = 
                    (new ContainerIterator<ContainerType, reverse>(c))->castTo<ValueType>();
            }

            operator Iterator<const ValueType> ()
            {
                return *reinterpret_cast<Iterator<const ValueType>*>(this);
            }

        private:
            template<typename T>
            void constructDispatch(T& v)
            {
                rebind(v);
            }

            typedef typename MPL::ToNoConst<ValueType>::Type RawValueType;

            template<>
            void constructDispatch<Iterator<RawValueType>>(Iterator<RawValueType>& v)
            {
                copyConstruct(v);
            }

            template<>
            void constructDispatch<Iterator<const RawValueType>>(Iterator<const RawValueType>& v)
            {
                copyConstruct(v);
            }

            void copyConstruct(const Iterator& o)
            {
                m_iter = NULL;
                *this = o;
            }

        private:
            IterPtr         m_iter;
        };

        /**
            @brief 观察者管理器
        */
        template<
            typename ListenerType, 
            typename MutexType = EmptyMutex,
            typename SetType = std::set<ListenerType*>>
        class ListenerManager
        {
        public:
            typedef DelegateN<boost::function<void(ListenerType*)>> Delegate;

        public:
            void addListener(ListenerType *p)
            {
                SingleLocker locker(m_mutex);
                m_set.insert(p);
            }

            void removeListener(ListenerType *p)
            {
                SingleLocker locker(m_mutex);
                m_set.erase(p);
            }

            bool isEmpty() const
            {
                SingleLocker locker(m_mutex);
                return m_set.empty();
            }

            void clear()
            {
                SingleLocker locker(m_mutex);
                m_set.clear();
            }

            /**
                @brief 调用所有观察者的某个函数
            */
            void foreach(const Delegate& d) const
            {
                SetType tempSet;
                {
                    SingleLocker locker(m_mutex);
                    tempSet = m_set;
                }

                typename SetType::const_iterator iter = tempSet.begin();
                while (iter != tempSet.end())
                {
                    d(*iter);
                    ++iter;
                }
            }

        private:
            mutable MutexType   m_mutex;
            SetType             m_set;
        };
}