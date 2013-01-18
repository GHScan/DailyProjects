#include "StdAfx.h"

#include <cassert>

#include <queue>
#include <set>

#include <boost/thread.hpp>

#include <Windows.h>

#include "Delegate.h"
#include "PlatformDepends.h"

#include "MemoryCheck.h"

namespace Scan
{
    typedef Delegate0<> Delegate;

    /**
        @brief 委托事件类集合
        集合的排序顺序为priority作为主键, 添加时间作为次键; priority越大时间越小,
        越靠前
    */
    class DelegateList
    {
    private:
        struct DelegateItem
        {
            bool operator < (const DelegateItem& o) const
            {
                if (priority == o.priority)
                {
                    return callTime > o.callTime;
                }
                else
                {
                    return priority < o.priority;
                }
            }

            DelegateItem(uint32 _priority, void* _id, const Delegate& _delegat, uint64 _callTime):
            priority(_priority), id(_id), delegt(_delegat), callTime(_callTime)
            {
            }

            uint32          priority;
            void*           id;
            uint64          callTime;
            Delegate  delegt;
        };

        typedef std::priority_queue<DelegateItem>   MaxHeap;

    public:
        DelegateList(){}

        void pushDelegate(const Delegate& d, void* id, uint32 priority)
        {
            SingleLocker locker(m_syncObj);

            m_heap.push(DelegateItem(priority, id, d, getInstructionCount()));
        }

        void popDelegate(Delegate& d, void* &id)
        {
            SingleLocker locker(m_syncObj);

            const DelegateItem& item = m_heap.top();
            id = item.id;
            d = item.delegt;

            m_heap.pop();
        }

        bool empty() const
        {
            SingleLocker locker(m_syncObj);

            return m_heap.empty();
        }

    private:
        DelegateList(const DelegateList&);
        DelegateList& operator = (const DelegateList&);

    private:
        MaxHeap                             m_heap;
        mutable Windows::CriticalSection    m_syncObj;
    };

    class DelegateThreadImpl
    {
    public:
        DelegateThreadImpl();
        ~DelegateThreadImpl();
        void setParent(DelegateThread *parent);
        // 以下3个函数能访问到的资源只有delegatelist, 这是一个闭合资源, 安全
        void addCall(const Delegate& d, void* id, uint32 priority);
        void addListener(IDelegateThreadListener *p);
        void removeListener(IDelegateThreadListener *p);

    private:
        void threadProc();

    private:
        // 以下4个直接访问listenerlist的函数被串行到本线程上了, 并且无外层锁, 开放资源, 但安全
        void _addListener(IDelegateThreadListener *p);
        void _removeListener(IDelegateThreadListener *p);
        void notifyDelegateState(void* id, bool isBegin);
        void notifyThreadState(bool isSleep);

    private:
        DelegateThreadImpl(const DelegateThreadImpl&);
        DelegateThreadImpl& operator = (const DelegateThreadImpl&);

    private:
        typedef boost::thread   Thread;
        typedef std::set<IDelegateThreadListener*>  ListenerList;
        typedef ListenerList::iterator              ListenerList_Iter;

    private:
        DelegateThread*         m_parent;

        Thread                  m_thread;
        Windows::Event          m_event;
        bool                    m_isOver;
        DelegateList            m_list;     
        ListenerList            m_listeners;
    };

    DelegateThreadImpl::DelegateThreadImpl():
    m_event("", false, false),
        m_isOver(false),
        m_parent(NULL)
    {
        boost::swap(m_thread, Thread(&DelegateThreadImpl::threadProc, this));
    }

    DelegateThreadImpl::~DelegateThreadImpl()
    {
        m_isOver = true;
        m_event.setEvent();
        m_thread.join();
    }

    void DelegateThreadImpl::addListener(IDelegateThreadListener *p)
    {
        addCall(Delegate(&DelegateThreadImpl::_addListener, this, p), this, -1);
    }

    void DelegateThreadImpl::removeListener(IDelegateThreadListener *p)
    {
        addCall(Delegate(&DelegateThreadImpl::_removeListener, this, p), this, -1);
    }

    void DelegateThreadImpl::setParent(DelegateThread *parent)
    {
        m_parent = parent;
    }

    void DelegateThreadImpl::addCall(const Delegate& d, void* id, uint32 priority)
    {
        m_list.pushDelegate(d, id, priority);
        m_event.setEvent();
    }

    void DelegateThreadImpl::_addListener(IDelegateThreadListener *p)
    {
        m_listeners.insert(p);
    }

    void DelegateThreadImpl::_removeListener(IDelegateThreadListener *p)
    {
        m_listeners.erase(p);
    }

    void DelegateThreadImpl::notifyDelegateState(void* id, bool isBegin)
    {
        for (ListenerList_Iter iter = m_listeners.begin();
            iter != m_listeners.end();
            ++iter)
        {
            IDelegateThreadListener* p = *iter;
            if (isBegin)
            {
                p->onBeginCall(m_parent, id);
            }
            else
            {
                p->onEndCall(m_parent, id);
            }
        }
    }

    void DelegateThreadImpl::notifyThreadState(bool isSleep)
    {
        for (ListenerList_Iter iter = m_listeners.begin();
            iter != m_listeners.end();
            ++iter)
        {
            IDelegateThreadListener *p = *iter;
            if (isSleep)
            {
                p->onThreadSleep(m_parent);
            }
            else
            {
                p->onThreadWakeup(m_parent);
            }
        }
    }

    void DelegateThreadImpl::threadProc()
    {
        for (;;)
        {
            if (!m_event.lock(0))
            {
                notifyThreadState(true);
                m_event.lock();
                notifyThreadState(false);   
            }

            while (!m_isOver && !m_list.empty())
            {
                Delegate d; 
                void* id;

                m_list.popDelegate(d, id);

                notifyDelegateState(id, true);

                d();

                notifyDelegateState(id, false);
            }

            if (m_isOver)
            {
                break;
            }
        }
    }



    DelegateThread::DelegateThread():
    m_impl(new DelegateThreadImpl())
    {
        m_impl->setParent(this);
    }

    DelegateThread::~DelegateThread()
    {
    }

    void DelegateThread::addCall(const Delegate& d, void* id, uint32 priority)
    {
        m_impl->addCall(d, id, priority);
    }

    void DelegateThread::addListener(IDelegateThreadListener *p)
    {
        m_impl->addListener(p);
    }

    void DelegateThread::removeListener(IDelegateThreadListener *p)
    {
        m_impl->removeListener(p);
    }



    class DelegateThreadList
    {
    public:
        DelegateThreadList();
        ~DelegateThreadList();

        void createNewThreads(uint32 count);
        DelegateThread* createNewThread();
        DelegateThread* switchIdleToBusy();
        void switchBusyToIdle(DelegateThread* p);

        void addListenerToAll(IDelegateThreadListener *p);
        void removeListenerToAll(IDelegateThreadListener *p);

    private:
        DelegateThreadList(const DelegateThreadList&);
        DelegateThreadList& operator = (const DelegateThreadList&);

    private:
        typedef std::set<DelegateThread*>   ThreadList;
        typedef ThreadList::iterator        ThreadList_Iter;

    private:
        Windows::CriticalSection    m_syncObj;
        ThreadList                  m_busyThreads;
        ThreadList                  m_idleThreads;
    };

    DelegateThreadList::DelegateThreadList()
    {

    }

    DelegateThreadList::~DelegateThreadList()
    {
        SingleLocker locker(m_syncObj);

        for (ThreadList_Iter iter = m_busyThreads.begin(); 
            iter != m_busyThreads.end(); 
            ++iter)
        {
            safe_delete(*iter);
        }
        for (ThreadList_Iter iter = m_idleThreads.begin(); 
            iter != m_idleThreads.end(); 
            ++iter)
        {
            safe_delete(*iter);
        }
    }

    void DelegateThreadList::createNewThreads(uint32 count)
    {
        SingleLocker locker(m_syncObj);

        for (uint32 i = 0; i < count; ++i)
        {
            m_idleThreads.insert(new DelegateThread);
        }
    }

    DelegateThread* DelegateThreadList::createNewThread()
    {
        SingleLocker locker(m_syncObj);
        DelegateThread *p = new DelegateThread;
        m_busyThreads.insert(p);
        return p;
    }

    DelegateThread* DelegateThreadList::switchIdleToBusy()
    {
        SingleLocker locker(m_syncObj);

        if (m_idleThreads.empty())
        {
            return NULL;
        }
        DelegateThread *p = *m_idleThreads.begin();
        m_idleThreads.erase(m_idleThreads.begin());
        m_busyThreads.insert(p);
        return p;
    }

    void DelegateThreadList::switchBusyToIdle(DelegateThread* p)
    {
        SingleLocker locker(m_syncObj);

        ThreadList_Iter iter = m_busyThreads.find(p);
        if (iter != m_busyThreads.end())
        {
            m_busyThreads.erase(iter);
            m_idleThreads.insert(p);
        }
    }

    void DelegateThreadList::addListenerToAll(IDelegateThreadListener *p)
    {
        SingleLocker locker(m_syncObj);

        for (ThreadList_Iter iter = m_busyThreads.begin(); 
            iter != m_busyThreads.end(); 
            ++iter)
        {
            (*iter)->addListener(p);
        }
        for (ThreadList_Iter iter = m_idleThreads.begin(); 
            iter != m_idleThreads.end(); 
            ++iter)
        {
            (*iter)->addListener(p);
        }
    }

    void DelegateThreadList::removeListenerToAll(IDelegateThreadListener *p)
    {
        SingleLocker locker(m_syncObj);

        for (ThreadList_Iter iter = m_busyThreads.begin(); 
            iter != m_busyThreads.end(); 
            ++iter)
        {
            (*iter)->removeListener(p);
        }
        for (ThreadList_Iter iter = m_idleThreads.begin(); 
            iter != m_idleThreads.end(); 
            ++iter)
        {
            (*iter)->removeListener(p);
        }
    }


    class DelegateThreadPoolImpl:
        public IDelegateThreadListener
    {
    public:
        DelegateThreadPoolImpl(DelegateThreadPool *p, uint32 autoExtend);
        ~DelegateThreadPoolImpl();

        void addCall(const Delegate& d, void* id, uint32 priority);
        void addListener(IDelegateThreadPoolListener *p);
        void removeListener(IDelegateThreadPoolListener *p);

    private:
        virtual void onThreadSleep(DelegateThread *p);
        virtual void onThreadWakeup(DelegateThread *p);
        virtual void onBeginCall(DelegateThread* td, void* delegateId);
        virtual void onEndCall(DelegateThread* td, void* delegateId);

    private:
        void notifyCallState(bool isBegin, void* id);
        void _addListener(IDelegateThreadPoolListener *p);
        void _removeListener(IDelegateThreadPoolListener *p);

    private:
        DelegateThreadPoolImpl(const DelegateThreadPoolImpl&);
        DelegateThreadPoolImpl& operator = (const DelegateThreadPoolImpl&);

    private:
        typedef std::set<IDelegateThreadPoolListener*> ListenerList;
        typedef ListenerList::iterator                 ListenerList_Iter;

    private:
        DelegateThreadList   m_threads;
        DelegateList         m_delegates;

        DelegateThreadPool  *m_parent;
        ListenerList         m_listeners;
        uint32               m_autoExtendCount;            
    };

    DelegateThreadPoolImpl::DelegateThreadPoolImpl(DelegateThreadPool *p, uint32 autoExtend):
    m_parent(p), m_autoExtendCount(autoExtend)
    {
        m_threads.createNewThreads(getProcessorCount() * 2);
        m_threads.addListenerToAll(this);
    }

    DelegateThreadPoolImpl::~DelegateThreadPoolImpl()
    {
        m_threads.removeListenerToAll(this);
    }

    void DelegateThreadPoolImpl::onThreadSleep(DelegateThread *p)
    {
        if (m_delegates.empty())
        {
            m_threads.switchBusyToIdle(p);
        }
        else
        {
            Delegate d;
            void* id;
            m_delegates.popDelegate(d, id);
            p->addCall(d, id, 0);
        }
    }

    void DelegateThreadPoolImpl::onThreadWakeup(DelegateThread *p)
    {

    }

    void DelegateThreadPoolImpl::notifyCallState(bool isBegin, void* id)
    {
        for (ListenerList_Iter iter = m_listeners.begin(); 
            iter != m_listeners.end();
            ++iter)
        {
            IDelegateThreadPoolListener *p = *iter;
            if (isBegin)
            {
                p->onBeginCall(m_parent, id);
            }
            else
            {
                p->onEndCall(m_parent, id);
            }
        }
    }

    void DelegateThreadPoolImpl::onBeginCall(DelegateThread* td, void* delegateId)
    {
        notifyCallState(true, delegateId);
    }   

    void DelegateThreadPoolImpl::onEndCall(DelegateThread* td, void* delegateId)
    {
        notifyCallState(false, delegateId);
    }

    void DelegateThreadPoolImpl::addCall(const Delegate& d, void* id, uint32 priority)
    {
        DelegateThread *td = m_threads.switchIdleToBusy();
        if (td == NULL && m_autoExtendCount > 0)
        {
            --m_autoExtendCount;
            td = m_threads.createNewThread();
            td->addListener(this);
        }
        if (td == NULL)
        {
            m_delegates.pushDelegate(d, id, priority);
        }
        else
        {
            td->addCall(d, id, priority);
        }
    }

    void DelegateThreadPoolImpl::_addListener(IDelegateThreadPoolListener *p)
    {
        m_listeners.insert(p);  
    }

    void DelegateThreadPoolImpl::_removeListener(IDelegateThreadPoolListener *p)
    {
        m_listeners.erase(p);
    }

    void DelegateThreadPoolImpl::addListener(IDelegateThreadPoolListener *p)
    {
        addCall(Delegate(&DelegateThreadPoolImpl::_addListener, this, p), this, -1);
    }

    void DelegateThreadPoolImpl::removeListener(IDelegateThreadPoolListener *p)
    {
        addCall(Delegate(&DelegateThreadPoolImpl::_removeListener, this, p), this, -1);
    }



    DelegateThreadPool::DelegateThreadPool(uint32 autoExtendCount):
    m_impl(NULL)
    {
        m_impl.reset(new DelegateThreadPoolImpl(this, autoExtendCount));
    }

    DelegateThreadPool::~DelegateThreadPool()
    {
    }

    void DelegateThreadPool::addCall(const Delegate& d, void* id, uint32 priority)
    {
        m_impl->addCall(d, id, priority);
    }

    void DelegateThreadPool::addListener(IDelegateThreadPoolListener *p)
    {
        m_impl->addListener(p);
    }

    void DelegateThreadPool::removeListener(IDelegateThreadPoolListener *p)
    {
        m_impl->removeListener(p);
    }
}