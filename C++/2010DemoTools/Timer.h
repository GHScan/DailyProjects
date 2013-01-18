#pragma once

#include <iostream>

#include <boost/scoped_ptr.hpp>

#include "Types.h"
#include "Delegate.h"

namespace Scan
{
    class LocalTimerImpl;

    /**
        @brief 局部定时器
        析构时可以输出时间等
    */
    class LocalTimer
    {
    public:
        LocalTimer(const String& name, bool isDestructPrint = true);
        ~LocalTimer();

        /**
            @brief 返回描述字符串
        */
        const String& getName() const;

        /**
            @brief 返回已经流逝的秒数
        */
        float getElapse() const;

        /**
            @brief 重置计时起点
        */
        void reset();

        /**
            @brief 打印时间开销
        */
        void print() const;
        void print(std::ostream& so) const;

    private:
        boost::scoped_ptr<LocalTimerImpl>   m_impl;
    };

    inline std::ostream& operator << (std::ostream& so, const LocalTimer& t)
    {
        t.print(so);
        return so;
    }

    class EventTimerImpl;
    /**
        @brief 事件定时器
        可以定时触发事件
    */
    class EventTimer
    {
    public:
        typedef Delegate0<>   EventHandler;

    public:
        EventTimer(bool isHanldeOncePerUpdate = false);
        ~EventTimer();

        /**
            @brief 添加一个事件响应
        */
        void addEventHandler(const String& name, float interval, const EventHandler& handler);
        void removeEventHandler(const String& name);
        
        void update(float elapse);

    private:
        boost::scoped_ptr<EventTimerImpl>   m_impl;
    };
}