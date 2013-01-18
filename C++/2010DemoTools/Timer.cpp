#include "stdafx.h"

#include <cassert>

#include <map>

#include "Timer.h"
#include "Utility.h"
#include "PlatformDepends.h"

#include "MemoryCheck.h"

namespace Scan
{
    /**
        @brief 局部定时器的实现
    */
    class LocalTimerImpl
    {
    public:
        LocalTimerImpl(const String& name, bool isDestructPrint):
          m_name(name), m_isDestructPrint(isDestructPrint)
          {
              reset();
          }

        ~LocalTimerImpl()
        {
            if (m_isDestructPrint)
            {
                print();
            }
        }

        const String getFullDescribeString() const
        {
            return getName() + " : " + toString(getElapse()) + "\n";
        }

        void print() const
        {
            printDebug(getFullDescribeString().c_str());
        }

        void print(std::ostream& so) const
        {
            so << getFullDescribeString();
        }

        void reset()
        {
            m_beginInstructions = getInstructionCount();
        }

        float getElapse() const
        {
            double d = double(getInstructionCount() - m_beginInstructions);
            d *= INV_CPU_FREQUENCY;
            return float(d);
        }

        const String& getName() const
        {
            return m_name;
        }

    private:
        String              m_name;
        bool                m_isDestructPrint;
        uint64              m_beginInstructions;

        static const double INV_CPU_FREQUENCY;
    };

    const double LocalTimerImpl::INV_CPU_FREQUENCY = 1.0 / getCpuFrequency();

    LocalTimer::LocalTimer(const String& name, bool isDestructPrint):
    m_impl(new LocalTimerImpl(name, isDestructPrint))
    {

    }

    LocalTimer::~LocalTimer()
    {
    }

    float LocalTimer::getElapse() const
    {
        return m_impl->getElapse();
    }

    const String& LocalTimer::getName() const
    {
        return m_impl->getName();
    }

    void LocalTimer::print() const
    {
        m_impl->print();
    }

    void LocalTimer::print(std::ostream& so) const
    {
        m_impl->print(so);
    }

    void LocalTimer::reset()
    {
        m_impl->reset();
    }

    /**
        @brief 事件定时器的实现
    */
    class EventTimerImpl
    {
    public:
        typedef Delegate0<>   EventHandler;

    public:
        EventTimerImpl(bool isHanldeOncePerUpdate):
          m_isHandleOncePerUpdate(isHanldeOncePerUpdate){}

        void addHandler(const String& name, float interval, const EventHandler& handler)
        {
            HandlerItem item;
            item.elapse = 0;
            item.interval = interval;
            item.handler = handler;
            assert(m_handlers.count(name) == 0);
            m_handlers[name] = item;
        }

        void removeHandler(const String& name)
        {
            m_handlers.erase(name);
        }

        void update(float elapse)
        {
            for (HandlerMap_Iter iter = m_handlers.begin();
                iter != m_handlers.end();
                ++iter)
            {
                HandlerItem& item = iter->second;
                item.elapse += elapse;

                if (item.elapse >= item.interval)
                {
                    item.handler();

                    if (m_isHandleOncePerUpdate)
                    {
                        item.elapse = 0;
                    }
                    else
                    {
                        item.elapse -= item.interval;

                        while (item.elapse >= item.interval)
                        {
                            item.handler();
                            item.elapse -= item.interval;
                        }
                    }
                }
            }
        }

    private:
        struct HandlerItem
        {
            float           elapse;
            float           interval;
            EventHandler    handler;
        };

        typedef std::map<String, HandlerItem>   HandlerMap;
        typedef HandlerMap::iterator            HandlerMap_Iter;

    private:
        HandlerMap      m_handlers;
        bool            m_isHandleOncePerUpdate;
    };

    EventTimer::EventTimer(bool isHanldeOncePerUpdate):
    m_impl(new EventTimerImpl(isHanldeOncePerUpdate))
    {

    }

    EventTimer::~EventTimer()
    {
    }

    void EventTimer::addEventHandler(const String& name, float interval, const EventHandler& handler)
    {
        m_impl->addHandler(name, interval, handler);
    }

    void EventTimer::removeEventHandler(const String& name)
    {
        m_impl->removeHandler(name);
    }

    void EventTimer::update(float elapse)
    {
        m_impl->update(elapse);
    }
}