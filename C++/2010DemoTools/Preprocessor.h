#pragma once

#include <boost/preprocessor.hpp>

#define _SCAN_TO_STRING(p)      #p
/**
    @brief ������ת��Ϊ�ַ���
*/
#define SCAN_PP_TO_STRING(p)       _SCAN_TO_STRING(p)

#define _SCAN_CAT(a, b)         a##b
/**
    @brief ������������
*/
#define SCAN_PP_CAT(a, b)          _SCAN_CAT(a, b)

/**
    @brief <�����ַ���, ����>��ֵ��
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
    @brief �൱��: data(from), data(from + 1), ...data(to)
*/
#define SCAN_PP_REPEAT_LIMITED_DATA_COMMA(from, to, data)     BOOST_PP_REPEAT_FROM_TO(from, BOOST_PP_INC(to), SCAN_PP_CONNECT_COMMA, data)

/**
    @brief �൱��: data(from) data(from + 1)... data(to)
*/
#define SCAN_PP_REPEAT_LIMITED_DATA_EMPTY(from, to, data)     BOOST_PP_REPEAT_FROM_TO(from, BOOST_PP_INC(to), SCAN_PP_CONNECT_EMPTY, data)

/**
    @brief �൱��: data(0), data(1), ...data(n)
*/
#define SCAN_PP_REPEAT_DATA_COMMA(n, data)     SCAN_PP_REPEAT_LIMITED_DATA_COMMA(0, BOOST_PP_DEC(n), data)

/**
    @brief �൱��: data(0) data(1)... data(n)
*/
#define SCAN_PP_REPEAT_DATA_EMPTY(n, data)     SCAN_PP_REPEAT_LIMITED_DATA_EMPTY(0, BOOST_PP_DEC(n), data)

/**
    @brief �൱��: typename Type0, typename Type1, ...typename Typen
*/
#define SCAN_PP_REPEAT_TYPENAME_TYPE_COMMA(n)  SCAN_PP_REPEAT_DATA_COMMA(n, SCAN_PP_ENUM_TYPENAME_TYPE)

/**
    @brief �൱��: Type0, Type1, ...Typen
*/
#define SCAN_PP_REPEAT_TYPE_COMMA(n)           SCAN_PP_REPEAT_DATA_COMMA(n, SCAN_PP_ENUM_TYPE)

/**
    @brief �൱��: Type0 p0, Type1 p1, ... Typen pn
*/
#define SCAN_PP_REPEAT_TYPE_PARAM_COMMA(n)     SCAN_PP_REPEAT_DATA_COMMA(n, SCAN_PP_ENUM_TYPE_PARAM)

/**
    @brief �൱��: p0, p1, ... pn
*/
#define SCAN_PP_REPEAT_PARAM_COMMA(n)          SCAN_PP_REPEAT_DATA_COMMA(n, SCAN_PP_ENUM_PARAM)

/**
    �ظ�����n�����ֵ
*/
#define SCAN_PP_REPEAT_MAX                     BOOST_PP_LIMIT_REPEAT