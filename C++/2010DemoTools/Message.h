#pragma once

#include <cassert>

#include <map>

#include "Any.h"
#include "Types.h"
#include "PlatformDepends.h"

namespace Scan
{
    /**
        @brief ��Ϣ
        ����Ϣ��Ӧ�úͽ�����Ϣ���Դ��ݵĲ�����Ĭ��
    */
    class Message
    {
    public:
        Message(){}

        explicit Message(const AnyList &params)
        {
            m_params = params;
        }

        template<typename T>
        void pushParam(T t)
        {
            m_params.push_back(Any(t));
        }

        void popParam()
        {
            m_params.pop_back();
        }

        template<typename T>
        T getParam(uint32 i) const
        {
            return m_params[i].get<T>();
        }

        template<typename T>
        void getParam(uint32 i, T& p) const
        {
            p = getParam<T>(i);
        }

        bool isEmpty() const
        {
            return m_params.empty();
        }

    private:
        AnyList     m_params;
    };

#define makeMessage(...)    Message(makeAnyList(__VA_ARGS__))

    /**
        @brief ��Ϣ������
    */
    struct IMessageReceiver
    {
        virtual ~IMessageReceiver() = 0 {}

        /**
            @brief �����߱�־
            ��Ϊ��Ϣ�����ߵ�Ŀ��
        */
        virtual const char* getReceiverID() const = 0;

        /**
            @brief ��Ϣ����
            �����������ﴦ�������Ϣ
        */
        virtual void onMessage(const Message& msg) = 0;
    };

    /**
        @brief ��Ϣ������ 
        ά����һ����Ϣ����
    */
    template<
        typename MutexType = EmptyMutex, 
        typename MapType = std::map<String, IMessageReceiver*>>
    class MessageDispatcher
    {
    public:
        /**
            @brief ��һ����Ϣ��������ӽ���Ϣ����
        */
        void addReceiver(IMessageReceiver *p)
        {
            SingleLocker locker(m_mutex);
            assert(m_map.count(p->getReceiverID()) == 0);
            m_map[p->getReceiverID()] = p;
        }

        void removeReceiver(const String& receiveID)
        {
            SingleLocker locker(m_mutex);
            m_map.erase(receiveID);
        }

        void removeReceiver(IMessageReceiver *p)
        {
            SingleLocker locker(m_mutex);
            m_map.erase(p->getReceiverID());
        }

        /**
            @brief ��һ����Ϣ������Ͷ��һ����Ϣ
        */
        bool sendMessage(const String& receiveID, const Message& msg) const
        {
            IMessageReceiver *p = NULL;

            {
                SingleLocker locker(m_mutex);
                MapType::const_iterator iter =  m_map.find(receiveID);
                if (iter != m_map.end())
                {
                    p = iter->second;
                }
            }

            if (p != NULL)
            {
                p->onMessage(msg);
            }

            return p != NULL;
        }

        void clear()
        {
            SingleLocker locker(m_mutex);
            m_map.clear();
        }

    private:
        mutable MutexType   m_mutex;
        MapType     m_map;
    };
}