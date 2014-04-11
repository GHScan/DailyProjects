#ifndef UTILITY_H
#define UTILITY_H

#include "Config.h"

#define by_ARRAY_SIZE(a)    (sizeof(a) / sizeof((a)[0]))

#define by_TO_STRING_(e)    #e
#define by_TO_STRING(e)     by_TO_STRING_(e)

#define by_CONCAT_(a, b)    a##b
#define by_CONCAT(a, b)     by_CONCAT_(a, b)

#define by_UNIQ_VAR_NAME(name)  by_CONCAT(name, __LINE__)

#endif
