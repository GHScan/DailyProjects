#pragma once

#include <boost/type_traits.hpp>
#include <boost/function.hpp>

#include "TypeDefine.h"
#include "Preprocessor.h"
#include "Utility.h"

namespace Scan
{

#define MARSHAL_MAX_PARAM       BOOST_PP_MIN(3, BOOST_PP_LIMIT_REPEAT)

template<typename T>
struct ResultUnmarshal
{
    static T unmarshal(const String& s)
    {
        T def = T();
        fromString(def, s);
        return def;
    }
};

template<>
struct ResultUnmarshal<void>
{
    static void unmarshal(const String& s)
    {
        return;
    }
};

template<typename T>
struct ValueType
{
    typedef typename boost::remove_cv<typename boost::remove_reference<T>::type >::type Type;
};

template<typename R, typename F>
inline R marshal_func(F f)
{
    StringVec params;
    return ResultUnmarshal<R>::unmarshal(f(params));
}

// 后面的宏用于变参的 marshal_func
/**
    template<typename R, typename F, typename T0, typename T1, ..., typename Tn>
    inline R marshal_func(F f, T0 t0, T1 t1, ... Tn tn)
    {
        StringVec params;
        params.push_back(toString(t0));
        params.push_back(toString(t1));
        ...
        params.push_back(toString(tn));
        return ResultUnmarshal<R>::unmarshal(f(params));
    }
 */

#define ENUM_MARSHAL_FUNC_PUSH_PARAM(nextD, curD, data) \
params.push_back(toString(BOOST_PP_CAT(t, curD)));
#define ENUM_MARSHAL_FUNC(nextD, curD, data) \
template<typename R, typename F, BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_TYPENAME_T, 0)> \
inline R marshal_func(F f, BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_TYPE_VAL, 0)) \
{ \
    StringVec params; \
    BOOST_PP_REPEAT(curD, ENUM_MARSHAL_FUNC_PUSH_PARAM, 0); \
    return ResultUnmarshal<R>::unmarshal(f(params)); \
}
BOOST_PP_REPEAT_FROM_TO(1, BOOST_PP_INC(MARSHAL_MAX_PARAM), ENUM_MARSHAL_FUNC, 0)
#undef ENUM_MARSHAL_FUNC
#undef ENUM_MARSHAL_FUNC_PUSH_PARAM



template<typename R>
inline const String unmarshal_func(
                                        const StringVec& params,
                                        boost::function<R()> f)
{
    return toString(f());
}

// 后面的宏生成变参的 unmarshal_func
/**
    template<typename R, typename T0, typename T1, ..., typename Tn>
    inline const String unmarshal_func(const StringVec& params,
    boost::function<R(T0, T1, ..., Tn)> f)
    {
        do 
        {
            if (params.size() < n) break;

            typename ValueType<T0>::Type t0; if (!fromString(t0, params[0])) break;
            typename ValueType<T1>::Type t1; if (!fromString(t1, params[1])) break;
            ...
            typename ValueType<Tn>::Type tn; if (!fromString(tn, params[n])) break;

            return toString(f(t0, t1, ..., tn));
        } while(0);
        return getEmptyString();
    }
*/

#define ENUM_UNMARSHAL_FUNC_FROM_PARAM(nextD, curD, data) \
typename ValueType<BOOST_PP_CAT(T, curD)>::Type BOOST_PP_CAT(t, curD); if (!fromString(BOOST_PP_CAT(t, curD), params[curD])) break;
#define ENUM_UNMARSHAL_FUNC(nextD, curD, data) \
template<typename R, BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_TYPENAME_T, 0)> \
inline const String unmarshal_func(const StringVec& params, \
                                   boost::function<R(BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_TYPE, 0))> f) \
{ \
    do { \
    if (params.size() < curD) break; \
    BOOST_PP_REPEAT(curD, ENUM_UNMARSHAL_FUNC_FROM_PARAM, 0); \
    return toString(f(BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_VAL, 0))); \
    } while(0); \
    return getEmptyString(); \
}
BOOST_PP_REPEAT_FROM_TO(1, BOOST_PP_INC(MARSHAL_MAX_PARAM), ENUM_UNMARSHAL_FUNC, 0)
#undef ENUM_UNMARSHAL_FUNC
#undef ENUM_UNMARSHAL_FUNC_FROM_PARAM


inline const String unmarshal_func(
                                        const StringVec& params,
                                        boost::function<void()> f)
{
    f(); return getEmptyString();
}

// 后面的宏生成变参的 unmarshal_func, 这个函数返回void
/**
    template<typename T0, typename T1, ..., typename Tn>
    inline const String unmarshal_func(const StringVec& params,
    boost::function<void(T0, T1, ..., Tn)> f)
    {
        do
        {
            if (params.size() < n) break;

            typename ValueType<T0>::Type t0; if (!fromString(t0, params[0])) break;
            typename ValueType<T1>::Type t1; if (!fromString(t1, params[1])) break;
            ...
            typename ValueType<Tn>::Type tn; if (!fromString(tn, params[n])) break;

            f(t0, t1, ..., tn);
        } while (0);
        return getEmptyString();
    }
 */
#define ENUM_UNMARSHAL_FUNC_FROM_PARAM(nextD, curD, data) \
typename ValueType<BOOST_PP_CAT(T, curD)>::Type BOOST_PP_CAT(t, curD); if (!fromString(BOOST_PP_CAT(t, curD), params[curD])) break;
#define ENUM_UNMARSHAL_FUNC(nextD, curD, data) \
template<BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_TYPENAME_T, 0)> \
inline const String unmarshal_func(const StringVec& params, \
                                   boost::function<void(BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_TYPE, 0))> f) \
{ \
    do { \
    if (params.size() < curD) break; \
    BOOST_PP_REPEAT(curD, ENUM_UNMARSHAL_FUNC_FROM_PARAM, 0); \
    f(BOOST_PP_REPEAT(curD, SCAN_PP_ENUM_VAL, 0)); \
    } while(0); \
    return getEmptyString(); \
}
BOOST_PP_REPEAT_FROM_TO(1, BOOST_PP_INC(MARSHAL_MAX_PARAM), ENUM_UNMARSHAL_FUNC, 0)
#undef ENUM_UNMARSHAL_FUNC
#undef ENUM_UNMARSHAL_FUNC_FROM_PARAM

}