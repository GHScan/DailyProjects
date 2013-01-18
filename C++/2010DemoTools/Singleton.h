#pragma once

#include <boost/shared_ptr.hpp>

#include "PlatformDepends.h"
#include "Utility.h"

namespace Scan
{
    /**
        @brief 单例模式
        使用它有两个步骤:
        1. 继承; class T: public Singleton<T>{}
        2. 私有化构造函数, 并添加友元; friend struct classAllocator<T>;
    */
    template<typename T, typename MutexType = Windows::CriticalSection>
    class Singleton
    {
    public:
        /**
            @brief 获得一个单例引用
            @param globalAcess 如果是在全局对象的构造中调用, 将它置为true
        */
        static T& getSingleton(bool globalAcess = false)
        {
            return *getSingletonPtr(globalAcess);
        }

        /**
            @brief 获得一个单例指针
            @param globalAcess 如果是在全局对象的构造中调用, 将它置为true
        */
        static T* getSingletonPtr(bool globalAcess = false)
        {
            static boost::shared_ptr<T> cs_object;
            if (cs_object.get() == NULL)
            {
                SingleLocker locker(globalAcess ? getLocalMutex() : getGlobalMutex());
                if (cs_object.get() == NULL)
                {
                    cs_object = 
                        boost::shared_ptr<T>(
                        ClassAllocator<T>::alloc(), 
                        &ClassAllocator<T>::unalloc);
                }
            }
            return cs_object.get();
        }

    protected:
        Singleton(){}
        ~Singleton(){}

    private:
        /**
            @brief 禁止拷贝
        */
        Singleton(const Singleton&);
        Singleton& operator = (const Singleton&);

    private:
        static ISyncObject& getGlobalMutex()
        {
            return cs_synObject;
        }
        static ISyncObject& getLocalMutex()
        {
            static EmptyMutex ls_synObject;
            return ls_synObject;
        }

    private:
        static MutexType cs_synObject;
    };

    template<typename T, typename MutexType>
    MutexType Singleton<T, MutexType>::cs_synObject;
}