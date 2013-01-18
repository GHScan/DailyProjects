#include "stdafx.h"

#include "MarshalSignal.h"

namespace Scan
{

class MarshalSignal::Connection::ConnectionImpl:
    private Copyable<false>
{
public:
    ConnectionImpl(boost::shared_ptr<std::list<boost::weak_ptr<IMarshalSlot> > > slots, 
        std::list<boost::weak_ptr<IMarshalSlot> >::iterator slotIter):
    m_slots(slots), m_slotIter(slotIter), m_slot(*slotIter), m_block(false){}

    // �������Ҫ�ܹ�����, Signal/Slot�Ѿ��������ط���ɾ������Ϣ
    bool connected() const
    {
        return !isDisconnectBySignal() && !isDisconnectBySlot();
    }

    void disconnect()
    {
        // �Ͽ�signal�Ǳ�
        if (boost::shared_ptr<std::list<boost::weak_ptr<IMarshalSlot> > > list = m_slots.lock())
        {
            list->erase(m_slotIter);
            m_slotIter = list->end();

            m_slots.reset();
        }

        m_slot.reset();

        m_block = false;
    }

    void block()
    {
        if (m_block) return;
        if (!connected()) return;
        m_slotIter->reset();
        m_block = true;
    }

    void unblock()
    {
        if (!m_block) return;
        if (!connected()) return;
        *m_slotIter = m_slot;
        m_block = false;
    }

    bool blocked() const
    {
        return m_block;
    }

private:
    bool isDisconnectBySignal() const
    {
        return m_slots.expired();
    }

    bool isDisconnectBySlot() const
    {
        return m_slot.expired();
    }

private:
    // signal������
    boost::weak_ptr<std::list<boost::weak_ptr<IMarshalSlot> > >  m_slots;   
    // slot������
    boost::weak_ptr<IMarshalSlot>                                m_slot;
    // slot��signal�е�λ��
    std::list<boost::weak_ptr<IMarshalSlot> >::iterator          m_slotIter;
    bool                                                             m_block;
};

void MarshalSignal::Connection::disconnect() { m_conImpl->disconnect(); }
bool MarshalSignal::Connection::connected() const { return m_conImpl->connected(); }
void MarshalSignal::Connection::block() { m_conImpl->block(); }
void MarshalSignal::Connection::unblock() { m_conImpl->unblock(); }
bool MarshalSignal::Connection::blocked() const { return m_conImpl->blocked(); }
MarshalSignal::Connection::Connection(
   boost::shared_ptr<std::list<boost::weak_ptr<IMarshalSlot> > > slots, 
   std::list<boost::weak_ptr<IMarshalSlot> >::iterator slotIter):
m_conImpl(new ConnectionImpl(slots, slotIter))
{
}

}