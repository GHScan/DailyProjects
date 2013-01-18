#pragma once

#include <cassert>

#include <map>

#include "Any.h"
#include "Types.h"
#include "PlatformDepends.h"

namespace Scan
{
    /**
        @brief 消息
        发消息方应该和接受消息方对传递的参数有默契
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
        @brief 消息接受者
    */
    struct IMessageReceiver
    {
        virtual ~IMessageReceiver() = 0 {}

        /**
            @brief 接受者标志
            作为消息发送者的目标
        */
        virtual const char* getReceiverID() const = 0;

        /**
            @brief 消息处理
            接受者在这里处理各种消息
        */
        virtual void onMessage(const Message& msg) = 0;
    };

    /**
        @brief 消息传递者 
        维护了一个消息网络
    */
    template<
        typename MutexType = EmptyMutex, 
        typename MapType = std::map<String, IMessageReceiver*>>
    class MessageDispatcher
    {
    public:
        /**
            @brief 将一个消息接受者添加进消息网络
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
            @brief 向一个消息接受者投送一条消息
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