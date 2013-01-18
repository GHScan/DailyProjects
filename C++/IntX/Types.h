#ifndef TYPES_H
#define TYPES_H

#include "Mpl.h"

template<int bitW>
struct Int
{
    typedef typename Mpl::If<
        sizeof(long long) == bitW / 8,
        long long,
        typename Mpl::If<
            sizeof(long) == bitW / 8,
        long,
        typename Mpl::If<
            sizeof(int) == bitW / 8,
        int,
        typename Mpl::If<
            sizeof(short) == bitW / 8,
        short,
            char>::Type
            >::Type
            >::Type
        >::Type
        Type;
};
typedef Int<8>::Type i8;
typedef Int<16>::Type i16;
typedef Int<32>::Type i32;
typedef Int<64>::Type i64;

template<int bitW>
struct Uint
{
    typedef typename Mpl::If<
        sizeof(unsigned long long) == bitW / 8,
        unsigned long long,
        typename Mpl::If<
            sizeof(unsigned long) == bitW / 8,
        unsigned long,
        typename Mpl::If<
            sizeof(unsigned int) == bitW / 8,
        unsigned int,
        typename Mpl::If<
            sizeof(unsigned short) == bitW / 8,
        unsigned short,
            unsigned char>::Type
            >::Type
            >::Type
        >::Type
        Type;
};
typedef Uint<8>::Type u8;
typedef Uint<16>::Type u16;
typedef Uint<32>::Type u32;
typedef Uint<64>::Type u64;

#endif
