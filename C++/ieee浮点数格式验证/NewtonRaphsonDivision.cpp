#include "stdafx.h"

#include <cstdint>
#include <cstdio>

#include <limits>
#include <type_traits>

template<typename TDest, typename TSrc>
inline TDest UnsafeCast(TSrc v, std::enable_if_t<sizeof(TDest) == sizeof(TSrc)>* = nullptr) {
    union {
        TSrc src;
        TDest dest;
    } u{ v };
    return u.dest;
}

template<typename TInt>
constexpr size_t Log2(TInt n, std::enable_if_t<std::is_integral_v<TInt>>* = nullptr) {
    return n == 0 ? 0 : 1 + Log2(n >> 1);
}

template<typename TFloat>
static auto Divide(TFloat n, TFloat d, std::enable_if<std::is_floating_point_v<TFloat>>* = nullptr) {
    using TUint = std::conditional_t<sizeof(TFloat) == sizeof(uint32_t), uint32_t, uint64_t>;
    static_assert(sizeof(TUint) == sizeof(TFloat), "Unsupported floating type detected!");


    auto constexpr kBits = sizeof(n) << 3;
    auto constexpr kSignificandBits = std::numeric_limits<TFloat>::digits - 1;
    auto constexpr kExponentBits = kBits - kSignificandBits - 1;
    auto constexpr kSignMask = TUint(1) << (kBits - 1);
    auto constexpr kExponentMask = ((TUint(1) << kExponentBits) - 1) << kSignificandBits;
    auto constexpr kSignificandMask = (TUint(1) << kSignificandBits) - 1;
    auto constexpr kExponentMinusOne = ((TUint(1) << (kExponentBits - 1)) - 2) << kSignificandBits;
    auto constexpr kIterations = Log2((kSignificandBits + Log2(17)) / Log2(17));


    auto ud = UnsafeCast<TUint>(d);
    auto un = UnsafeCast<TUint>(n);
    auto sd = ud & kSignMask;
    auto ed = ud & kExponentMask;
    auto en = un & kExponentMask;
    auto eShift = kExponentMinusOne - ed;
    auto en2 = en + eShift;

    TUint un2;
    if (en2 & kSignMask) {
        un2 = eShift & kSignMask
            ? un & kSignMask ^ sd                       // underflow
            : (un & kSignMask | kExponentMask) ^ sd;    // overflow
    }
    else {
        un2 = (un & ~kExponentMask | en2) ^ sd;
    }
    auto n2 = UnsafeCast<TFloat>(un2);
    auto d2 = UnsafeCast<TFloat>(ud & kSignificandMask | kExponentMinusOne);


    TFloat x = 48 / 17.0 - 32 / 17.0 * d2;
    for (auto _ = 0; _ < kIterations; ++_) {
        x = x + x * (1 - d2 * x);
    }


    return n2 * x;
}

int main() {
    float numbers[] = { -3e-22, 2e-10, 0.1, 0.6, 1, 7, 200, 300000, 5e8, -7e20 };
    for (auto a : numbers) {
        for (auto b : numbers) {
            auto c = Divide(a, b);
            auto d = a / b;
            if (c != d) {
                printf("%f/%f->%f (%f)\n", a, b, c, d);
            }
        }
    }
}
