#pragma once

#include <boost/shared_ptr.hpp>

#include "PlatformDepends.h"
#include "Utility.h"

namespace Scan
{
    /**
        @brief ����ģʽ
        ʹ��������������:
        1. �̳�; class T: public Singleton<T>{}
        2. ˽�л����캯��, �������Ԫ; friend struct classAllocator<T>;
    */
    template<typename T, typename MutexType = Windows::CriticalSection>
    class Singleton
    {
    public:
        /**
            @brief ���һ����������
            @param globalAcess �������ȫ�ֶ���Ĺ����е���, ������Ϊtrue
        */
        static T& getSingleton(bool globalAcess = false)
        {
            return *getSingletonPtr(globalAcess);
        }

        /**
            @brief ���һ������ָ��
            @param globalAcess �������ȫ�ֶ���Ĺ����е���, ������Ϊtrue
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
            @brief ��ֹ����
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