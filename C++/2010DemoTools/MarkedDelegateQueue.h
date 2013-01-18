#pragma once

#include <map>

#include "Types.h"
#include "Delegate.h"

namespace Scan
{
    /**
        @brief 代标记的委托队列

        使用如下:
        用户可以添加很多委托到队列中, 刚添加的时候委托处于未标记状态; 用户可以在
        任何时候标记任意委托; 最后通过callDelegates来调用在队列头的连续被标记元素
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
            @brief 添加未标记的委托; 然后用户可以通过DelegateID来标记对应的委托
            @return 用于标记委托的ID
        */
        DelegateID addUnmarkedDelegate(Delegate d);

        /**
            @brief 便利函数
            @param p 类ClassType的对象
            @param f 类ClassType的方法MethodType
        */
        template<typename ClassType, typename MethodType>
        DelegateID addUnmarkedDelegate(ClassType *p, MethodType f)
        {
            return addUnmarkedDelegate(Delegate(f, p));
        }
        
        /**
            @brief 标记一个委托
        */
        void setDelegateMarked(DelegateID id);

        /**
            @brief 尝试调用委托
            @return 返回被调用的委托数量
            位于队列头的连续被标记委托会被执行
        */
        uint32 callDelegates();

        /**
            @brief 模拟函数子
        */
        uint32 operator() ()
        {
            return callDelegates();
        }

        /**
            @brief 设置超时时间
            超时的委托将不会再被调用
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