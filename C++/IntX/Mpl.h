#ifndef MPL_H
#define MPL_H

namespace Mpl {

template<bool b, typename T, typename T2>
struct If
{
    typedef T Type;
};

template<typename T, typename T2>
struct If<false, T, T2>
{
    typedef T2 Type;
};

template<bool b>
void static_assert()
{
    char a[b] = {0};
}

}

#endif
