#pragma once

#include <map>

#include "Types.h"
#include "Delegate.h"

namespace Scan
{
    /**
        @brief ����ǵ�ί�ж���

        ʹ������:
        �û�������Ӻܶ�ί�е�������, ����ӵ�ʱ��ί�д���δ���״̬; �û�������
        �κ�ʱ��������ί��; ���ͨ��callDelegates�������ڶ���ͷ�����������Ԫ��
    */
    class MarkedDelegateQueue
    {
    public:
        typedef Delegate0<> Delegate;
        typedef uint32      DelegateID;

    public:
        MarkedDelegateQueue(void);
        ~MarkedDelegateQueue(void);

        /**
            @brief ���δ��ǵ�ί��; Ȼ���û�����ͨ��DelegateID����Ƕ�Ӧ��ί��
            @return ���ڱ��ί�е�ID
        */
        DelegateID addUnmarkedDelegate(Delegate d);

        /**
            @brief ��������
            @param p ��ClassType�Ķ���
            @param f ��ClassType�ķ���MethodType
        */
        template<typename ClassType, typename MethodType>
        DelegateID addUnmarkedDelegate(ClassType *p, MethodType f)
        {
            return addUnmarkedDelegate(Delegate(f, p));
        }
        
        /**
            @brief ���һ��ί��
        */
        void setDelegateMarked(DelegateID id);

        /**
            @brief ���Ե���ί��
            @return ���ر����õ�ί������
            λ�ڶ���ͷ�����������ί�лᱻִ��
        */
        uint32 callDelegates();

        /**
            @brief ģ�⺯����
        */
        uint32 operator() ()
        {
            return callDelegates();
        }

        /**
            @brief ���ó�ʱʱ��
            ��ʱ��ί�н������ٱ�����
        */
        void setDelegateExpireTime(uint32 milliSeconds)
        {
            m_expireMillSeconds = milliSeconds;
        }

    private:
        MarkedDelegateQueue(const MarkedDelegateQueue&);
        MarkedDelegateQueue& operator = (const MarkedDelegateQueue&);

    private:
        struct DelegateQueueItem
        {
            DelegateQueueItem(Delegate d);
            DelegateQueueItem();

            Delegate    d;
            bool        isMarked;
            clock_t     enterTime;
        };

        typedef std::map<DelegateID, DelegateQueueItem>   DelegateQueue;

    private:
        uint32                  m_expireMillSeconds;
        DelegateID              m_delegateIDSeed;
        DelegateQueue           m_delegates;
    };

}