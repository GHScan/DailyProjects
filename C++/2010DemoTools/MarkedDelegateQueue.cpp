#include "StdAfx.h"

#include <ctime>

#include "MarkedDelegateQueue.h"

namespace Scan
{
    MarkedDelegateQueue::DelegateQueueItem::DelegateQueueItem(Delegate _d):
    d(_d), isMarked(false), enterTime(clock())
    {
    }

    MarkedDelegateQueue::DelegateQueueItem::DelegateQueueItem():
    isMarked(false), enterTime(clock())
    {}

    MarkedDelegateQueue::MarkedDelegateQueue(void):
    m_delegateIDSeed(1), m_expireMillSeconds(1000 * 60 * 60 * 24 * 1)
    {
    }

    MarkedDelegateQueue::~MarkedDelegateQueue(void)
    {
    }

    MarkedDelegateQueue::DelegateID MarkedDelegateQueue::addUnmarkedDelegate(Delegate d)
    {
        return 
            m_delegates.insert(DelegateQueue::value_type(m_delegateIDSeed++, DelegateQueueItem(d))).first->first;
    }

    void MarkedDelegateQueue::setDelegateMarked(DelegateID id)
    {
        DelegateQueue::iterator iter = m_delegates.find(id);
        if (iter != m_delegates.end())
        {
            iter->second.isMarked = true;
        }
    }

    uint32 MarkedDelegateQueue::callDelegates()
    {
        clock_t nowTime = clock();
        uint32 num = 0;

        for (DelegateQueue::iterator iter = m_delegates.begin();
            iter != m_delegates.end();
            )
        {
            DelegateQueueItem &item = iter->second;
            if (iter->second.isMarked || nowTime - item.enterTime > clock_t(m_expireMillSeconds))
            {
                if (iter->second.isMarked)
                {
                    iter->second.d();
                    ++num;
                }

                iter = m_delegates.erase(iter);
            }
            else
            {
                break;
            }
        }

        return num;
    }
}