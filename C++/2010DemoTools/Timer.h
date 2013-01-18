#pragma once

#include <iostream>

#include <boost/scoped_ptr.hpp>

#include "Types.h"
#include "Delegate.h"

namespace Scan
{
    class LocalTimerImpl;

    /**
        @brief �ֲ���ʱ��
        ����ʱ�������ʱ���
    */
    class LocalTimer
    {
    public:
        LocalTimer(const String& name, bool isDestructPrint = true);
        ~LocalTimer();

        /**
            @brief ���������ַ���
        */
        const String& getName() const;

        /**
            @brief �����Ѿ����ŵ�����
        */
        float getElapse() const;

        /**
            @brief ���ü�ʱ���
        */
        void reset();

        /**
            @brief ��ӡʱ�俪��
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
        @brief �¼���ʱ��
        ���Զ�ʱ�����¼�
    */
    class EventTimer
    {
    public:
        typedef Delegate0<>   EventHandler;

    public:
        EventTimer(bool isHanldeOncePerUpdate = false);
        ~EventTimer();

        /**
            @brief ���һ���¼���Ӧ
        */
        void addEventHandler(const String& name, float interval, const EventHandler& handler);
        void removeEventHandler(const String& name);
        
        void update(float elapse);

    private:
        boost::scoped_ptr<EventTimerImpl>   m_impl;
    };
}