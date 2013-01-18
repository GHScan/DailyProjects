// vim:fileencoding=gbk

#include "pch.h"

#include "Types.h"

int main()
{
    Mpl::static_assert<sizeof(i32) == 4>();
    Mpl::static_assert<sizeof(i64) == 8>();
    Mpl::static_assert<sizeof(i8) == 1>();
    Mpl::static_assert<sizeof(i16) == 2>();
}
