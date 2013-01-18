#pragma once

#include <boost/preprocessor.hpp>

#define _SCAN_TO_STRING(p)      #p
/**
    @brief 将符号转化为字符串
*/
#define SCAN_PP_TO_STRING(p)       _SCAN_TO_STRING(p)

#define _SCAN_CAT(a, b)         a##b
/**
    @brief 连接两个符号
*/
#define SCAN_PP_CAT(a, b)          _SCAN_CAT(a, b)

/**
    @brief <符号字符串, 符号>的值对
*/
#define SCAN_PP_NV(obj)            SCAN_PP_TO_STRING(obj), obj
#define SCAN_PP_VN(obj)            obj, SCAN_PP_TO_STRING(obj)

#define SCAN_PP_CONNECT_COMMA(z, n, data)   BOOST_PP_COMMA_IF(n) data(n)
#define SCAN_PP_CONNECT_EMPTY(z, n, data)   data(n)
#define SCAN_PP_ENUM_TYPENAME_TYPE(n)       SCAN_PP_CAT(typename Type, n)
#define SCAN_PP_ENUM_TYPE(n)                SCAN_PP_CAT(Type, n)
#define SCAN_PP_ENUM_TYPE_PARAM(n)          SCAN_PP_CAT(Type, n) SCAN_PP_CAT(p, n)
#define SCAN_PP_ENUM_PARAM(n)               SCAN_PP_CAT(p, n)

/**
    @brief 相当于: data(from), data(from + 1), ...data(to)
*/
#define SCAN_PP_REPEAT_LIMITED_DATA_COMMA(from, to, data)     BOOST_PP_REPEAT_FROM_TO(from, BOOST_PP_INC(to), SCAN_PP_CONNECT_COMMA, data)

/**
    @brief 相当于: data(from) data(from + 1)... data(to)
*/
#define SCAN_PP_REPEAT_LIMITED_DATA_EMPTY(from, to, data)     BOOST_PP_REPEAT_FROM_TO(from, BOOST_PP_INC(to), SCAN_PP_CONNECT_EMPTY, data)

/**
    @brief 相当于: data(0), data(1), ...data(n)
*/
#define SCAN_PP_REPEAT_DATA_COMMA(n, data)     SCAN_PP_REPEAT_LIMITED_DATA_COMMA(0, BOOST_PP_DEC(n), data)

/**
    @brief 相当于: data(0) data(1)... data(n)
*/
#define SCAN_PP_REPEAT_DATA_EMPTY(n, data)     SCAN_PP_REPEAT_LIMITED_DATA_EMPTY(0, BOOST_PP_DEC(n), data)

/**
    @brief 相当于: typename Type0, typename Type1, ...typename Typen
*/
#define SCAN_PP_REPEAT_TYPENAME_TYPE_COMMA(n)  SCAN_PP_REPEAT_DATA_COMMA(n, SCAN_PP_ENUM_TYPENAME_TYPE)

/**
    @brief 相当于: Type0, Type1, ...Typen
*/
#define SCAN_PP_REPEAT_TYPE_COMMA(n)           SCAN_PP_REPEAT_DATA_COMMA(n, SCAN_PP_ENUM_TYPE)

/**
    @brief 相当于: Type0 p0, Type1 p1, ... Typen pn
*/
#define SCAN_PP_REPEAT_TYPE_PARAM_COMMA(n)     SCAN_PP_REPEAT_DATA_COMMA(n, SCAN_PP_ENUM_TYPE_PARAM)

/**
    @brief 相当于: p0, p1, ... pn
*/
#define SCAN_PP_REPEAT_PARAM_COMMA(n)          SCAN_PP_REPEAT_DATA_COMMA(n, SCAN_PP_ENUM_PARAM)

/**
    重复次数n的最大值
*/
#define SCAN_PP_REPEAT_MAX                     BOOST_PP_LIMIT_REPEAT