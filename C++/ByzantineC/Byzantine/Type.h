#ifndef TYPE_H
#define TYPE_H

#include "Config.h"

typedef char                    byInt8;
typedef short                   byInt16;
typedef int                     byInt32;
typedef long long               byInt64;
typedef unsigned char           byUint8;
typedef unsigned short          byUint16;
typedef unsigned int            byUint32;
typedef unsigned long long      byUint64;

#if defined(by_64BIT)
typedef byInt64                 byInt;
#else
typedef byInt32                 byInt;
#endif

typedef byUint8                 byByte;

typedef enum byColor {
    byColor_Red,
    byColor_Green,
    byColor_Blue,
    byColor_Black,
    byColor_White,
} byColor;

#endif
