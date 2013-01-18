#pragma once

#include <map>
#include <string>

#include <boost/any.hpp>
#include <boost/signals.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/bind.hpp>
#include <boost/function.hpp>

#include "TypeDefine.h"
#include "Preprocessor.h"

namespace Scan
{

#define SIGNAL_MAX_PARAM        BOOST_PP_MIN(2, BOOST_PP_LIMIT_REPEAT)

/**
    @brief 特点:
        1. Signal类型的不同对象, 各自都可以连接一组签名不同的slots; (any的特点)
*/
class Signal:
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
        return boost::any_cast<boost::shared_ptr<boost::signal<Signature>>>(m_sigImpl)->connect(f, (boost::signals::connect_position)cp);
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
            m_sigImpl = boost::shared_ptr<boost::signal<Signature>>(new boost::signal<Signature>());
        }
        else ASSERT(m_sigImpl.type() == typeid(boost::shared_ptr<boost::signal<Signature>>));
    }

    void undefine()
    {
        m_sigImpl = boost::any();
    }

    bool isNull() const
    {
        return m_sigImpl.empty();
    }

    template<typename R>
    R emit() const
    {
        if (isNull()) return R();
        return (*boost::any_cast<boost::shared_ptr<boost::signal<R()> > >(m_sigImpl))();
    }

#define ENUM_EMIT(nextD, curD, data) \
    template<typename R, BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_TYPENAME_T, 0)> \
    R emit(BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_TYPE_VAL, 0)) const \
    { \
    if (isNull()) return R(); \
    return (*boost::any_cast<boost::shared_ptr<boost::signal<R(BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_TYPE, 0))> > >(m_sigImpl))(BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_VAL, 0)); \
    }

    BOOST_PP_REPEAT_FROM_TO(1, BOOST_PP_INC(SIGNAL_MAX_PARAM), ENUM_EMIT, 0)

#undef ENUM_EMIT

private:
    boost::any  m_sigImpl;
};

#undef SIGNAL_MAX_PARAM

}