#pragma once

#include <boost/preprocessor.hpp>

#define SCAN_PP_ENUM_TYPENAME_T(nextD, curD, data) \
    BOOST_PP_COMMA_IF(curD) typename BOOST_PP_CAT(T, curD)
#define SCAN_PP_ENUM_TYPE(nextD, curD, data) \
    BOOST_PP_COMMA_IF(curD) BOOST_PP_CAT(T, curD)
#define SCAN_PP_ENUM_VAL(nextD, curD, data) \
    BOOST_PP_COMMA_IF(curD) BOOST_PP_CAT(t, curD)
#define SCAN_PP_ENUM_TYPE_VAL(nextD, curD, data)   \
    BOOST_PP_COMMA_IF(curD) BOOST_PP_CAT(T, curD) BOOST_PP_CAT(t, curD)

#define SCAN_PP__TO_STRING(s)   # s
#define SCAN_PP_TO_STRING(s)    SCAN_PP__TO_STRING(s)

#define SCAN_PP_FILE_LINE   __FILE__ "(" SCAN_PP_TO_STRING(__LINE__) ")"