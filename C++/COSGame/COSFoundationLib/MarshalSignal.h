#pragma once

#include <list>

#include <boost/shared_ptr.hpp>
#include <boost/weak_ptr.hpp>
#include <boost/signal.hpp>
#include <boost/type_traits.hpp>

#include "TypeDefine.h"
#include "Preprocessor.h"
#include "Utility.h"
#include "MarshalHelp.h"

namespace Scan
{

class MarshalSignal:
    private Copyable<true>
{
public:
    struct IMarshalSlot
    {
        virtual ~IMarshalSlot(){}
        virtual const String onEmit(const StringVec& params) = 0;
    };

    // 因为其他的signal::connection, 有用boost的, 所以虽然signal接口可以不按boost, 但connection应该相同接口
    class Connection:
        private Copyable<true>
    {
    public:
        void disconnect();
        bool connected() const;

        void block();
        void unblock();
        bool blocked() const;

        bool operator == (const Connection& o) const { return m_conImpl == o.m_conImpl; }
        bool operator < (const Connection& o) const  { return m_conImpl < o.m_conImpl; }

    private:
        friend class MarshalSignal;

    private:
        Connection(
            boost::shared_ptr<std::list<boost::weak_ptr<IMarshalSlot> > > slots, 
            std::list<boost::weak_ptr<IMarshalSlot> >::iterator slotIter);

    private:
        class ConnectionImpl;
        // 用智能指针, 因为需要每个==的connection实例, 能够共享行为
        boost::shared_ptr<ConnectionImpl>  m_conImpl;   
    };
    
    enum ConnectionPos
    {
        CP_Back = 0,
        CP_Front = 1,
    };

private:
    class MarshalSlotWrapper:
        private Copyable<true>
    {
    public:
        MarshalSlotWrapper(IMarshalSlot* slot): m_slot(slot){}
        const String operator () (const StringVec& params) const
        {
            return m_slot->onEmit(params);
        }

    private:
        IMarshalSlot    *m_slot;
    };

private:
    struct VoidTag{};
    struct NotVoidTag{};
    template<typename T>
    struct GetVoidTag
    {
        typedef typename boost::mpl::if_<boost::is_void<T>, VoidTag, NotVoidTag>::type Type;
    };

public:
    template<typename R>
    R emit()
    {
        return emitImpl<R>(GetVoidTag<R>::Type()); 
    }

    // 后面的宏用于变参的 emit
    /**
    template<typename R, typename T0, typename T1, ..., typename Tn>
    R emit(T0 t0, T1 t1, ..., Tn tn)
    {
        return emitImpl<R, T0, T1, ..., Tn>(GetVoidTag<R>::Type(), t0, t1, ..., tn); 
    }
     */

#define ENUM_MARSHAL_SIGNAL_EMIT(nextD, curD, data) \
    template<typename R, BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_TYPENAME_T, 0)> \
    R emit(BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_TYPE_VAL, 0)) \
    { \
        return emitImpl<R, BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_TYPE, 0)>(GetVoidTag<R>::Type(), BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_VAL, 0)); \
    }
BOOST_PP_REPEAT_FROM_TO(1, BOOST_PP_INC(MARSHAL_MAX_PARAM), ENUM_MARSHAL_SIGNAL_EMIT, 0)
#undef ENUM_MARSHAL_SIGNAL_EMIT

    Connection connect(boost::shared_ptr<IMarshalSlot> slot, ConnectionPos pos = CP_Back)
    {
        if (m_slots == NULL) m_slots.reset(new std::list<boost::weak_ptr<IMarshalSlot> >);

        return Connection(m_slots, m_slots->insert(pos == CP_Back ? m_slots->end() : m_slots->begin(), slot));
    }

    void disconnectAll()
    {
        m_slots.reset();
    }

    bool isNull() const
    {
        return m_slots == NULL;
    }

    size_t getSlotCount() const
    {
        return m_slots == NULL ? 0 : m_slots->size();
    }

    void compact()
    {
        if (isNull()) return;

        for (std::list<boost::weak_ptr<IMarshalSlot> >::iterator iter = m_slots->begin();
            iter != m_slots->end();)
        {
            if (iter->expired()) iter = m_slots->erase(iter);
            else ++iter;
        }
        if (m_slots->empty()) m_slots.reset();
    }

private:
    template<typename R>
    R emitImpl(NotVoidTag)
    {
        if (isNull()) return R();

        R r = R();
        for (std::list<boost::weak_ptr<IMarshalSlot> >::iterator iter = m_slots->begin();
            iter != m_slots->end();
            ++iter)
        {
            if (boost::shared_ptr<IMarshalSlot> slot = iter->lock())
            {
                r = marshal_func<R>(MarshalSlotWrapper(slot.get()));
            }
        }

        return r;        
    }

    // 后面的宏用于变参的 emitImpl, 返回类型R
    /**
    template<typename R, typename T0, typename T1, ..., typename Tn>
    R emitImpl(NotVoidTag, T0 t0, T1 t1, ..., Tn tn)
    {
        if (isNull()) return R();
        R r = R();
        for (std::list<boost::weak_ptr<IMarshalableSlot> >::iterator iter = m_slots->begin();
            iter != m_slots->end();
            ++iter)
        {
            if (boost::shared_ptr<IMarshalableSlot> slot = iter->lock())
            {
                r = marshal_func<R>(MarshalableSlotWrapper(slot.get()), t0, t1, ..., tn);
            }
        }
        return r; 
    }
    */

#define ENUM_MARSHAL_SIGNAL_EMITIMPL(nextD, curD, data) \
    template<typename R, BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_TYPENAME_T, 0)> \
    R emitImpl(NotVoidTag, BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_TYPE_VAL, 0)) \
    { \
        if (isNull()) return R(); \
        R r = R(); \
        for (std::list<boost::weak_ptr<IMarshalSlot> >::iterator iter = m_slots->begin(); \
            iter != m_slots->end(); \
            ++iter) \
        { \
            if (boost::shared_ptr<IMarshalSlot> slot = iter->lock()) \
            { \
                r = marshal_func<R>(MarshalSlotWrapper(slot.get()), BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_VAL, 0)); \
            }\
        } \
        return r;  \
    }
    BOOST_PP_REPEAT_FROM_TO(1, BOOST_PP_INC(MARSHAL_MAX_PARAM), ENUM_MARSHAL_SIGNAL_EMITIMPL, 0)
#undef ENUM_MARSHAL_SIGNAL_EMITIMPL

    template<typename R>
    R emitImpl(VoidTag) // R 必须是void
    {
        if (isNull()) return;

        for (std::list<boost::weak_ptr<IMarshalSlot> >::iterator iter = m_slots->begin();
            iter != m_slots->end();
            ++iter)
        {
            if (boost::shared_ptr<IMarshalSlot> slot = iter->lock())
            {
                marshal_func<void>(MarshalSlotWrapper(slot.get()));
            }
        }
    }
   
    // 后面的宏用于变参的 emitImpl, 返回类型void
    /**
    template<typename R, typename T0, typename T1, ..., typename Tn>
    R emitImpl(VoidTag, T0 t0, T1 t1, ..., Tn tn) // R 必须是void
    {
        if (isNull()) return;
        for (std::list<boost::weak_ptr<IMarshalableSlot> >::iterator iter = m_slots->begin();
            iter != m_slots->end();
            ++iter)
        {
            if (boost::shared_ptr<IMarshalableSlot> slot = iter->lock())
            {
                marshal_func<void>(MarshalableSlotWrapper(slot.get()), t0, t1, ..., tn);
            }
        }
    }
     */

#define ENUM_MARSHAL_SIGNAL_EMITIMPL(nextD, curD, data) \
    template<typename R, BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_TYPENAME_T, 0)> \
    R emitImpl(VoidTag, BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_TYPE_VAL, 0)) \
    { \
        if (isNull()) return; \
        for (std::list<boost::weak_ptr<IMarshalSlot> >::iterator iter = m_slots->begin(); \
            iter != m_slots->end(); \
            ++iter) \
        { \
            if (boost::shared_ptr<IMarshalSlot> slot = iter->lock()) \
            { \
                marshal_func<void>(MarshalSlotWrapper(slot.get()), BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_VAL, 0)); \
            } \
        } \
    }
    BOOST_PP_REPEAT_FROM_TO(1, BOOST_PP_INC(MARSHAL_MAX_PARAM), ENUM_MARSHAL_SIGNAL_EMITIMPL, 0)
#undef ENUM_MARSHAL_SIGNAL_EMITIMPL

private:
    // 用智能指针, 因为需要Singal被销毁后, connection能够知道; 另外IMarshalableSlot被销毁后, 也能够不再调用
    boost::shared_ptr<std::list<boost::weak_ptr<IMarshalSlot> > > m_slots; 
};

class UnmarshSignal:
    public MarshalSignal::IMarshalSlot,
    private Copyable<true>
{
public:
    typedef boost::signals::connection  Connection;

    enum ConnectionPos
    {
        CP_Back = boost::signals::at_back,
        CP_Front = boost::signals::at_front,
    };

public:
    const String emit(const StringVec& params)
    {
        if (isNull()) return getEmptyString();
        return m_unmarshal->unmarshal(params);
    }

    // 调用顺序
    template<typename Signature>
    Connection connect(Signature *f, ConnectionPos cp = CP_Back)
    {
        return connect(boost::function<Signature>(f), cp);
    }

    template<typename Signature>
    Connection connect(boost::function<Signature> f, ConnectionPos cp = CP_Back)
    {
        define<Signature>();
        return boost::any_cast<boost::shared_ptr<boost::signal<Signature> > >(m_sigImpl)->connect(f, (boost::signals::connect_position)cp);
    }

    void disconnectAll()
    {
        undefine();
    }

    template<typename Signature>
    void define()
    {
        if (m_sigImpl.empty())
        {
            boost::shared_ptr<boost::signal<Signature> > sig(new boost::signal<Signature>());
            m_sigImpl = sig;

            ASSERT(m_unmarshal == NULL);
            m_unmarshal.reset(new UnmarshalHelper<Signature>(boost::ref(*sig)));
        }
        else ASSERT(m_sigImpl.type() == typeid(boost::shared_ptr<boost::signal<Signature> >));
    }

    void undefine()
    {
        if (m_sigImpl.empty()) return;
        ASSERT(m_unmarshal != NULL);

        m_sigImpl = boost::any();
        m_unmarshal.reset();
    }

    bool isNull() const
    {
        return m_sigImpl.empty();
    }

private:
    // 这个可以处理参数类型和个数不完全匹配的问题
    virtual const String onEmit(const StringVec& params){ return emit(params); }

private:
    struct IUnmarshalHelper
    {
        virtual ~IUnmarshalHelper() {}
        virtual const String unmarshal(const StringVec& params) = 0;
    };

    template<typename Signature>
    class UnmarshalHelper:
        public IUnmarshalHelper
    {
    public:
        UnmarshalHelper(boost::function<Signature> f): m_f(f){}
        virtual const String unmarshal(const StringVec& params)
        {
            return unmarshal_func(params, m_f);
        }

    private:
        boost::function<Signature>  m_f;
    };

private:
    boost::any                              m_sigImpl;
    boost::shared_ptr<IUnmarshalHelper>     m_unmarshal;
};

}