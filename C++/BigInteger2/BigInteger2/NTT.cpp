

#include "NTT.h"


namespace RingArithmetic_NTT {

    constexpr uint64_t kPrime = 4179340454199820289ULL;
    constexpr uint64_t kHalfPrime = kPrime >> 1;
    constexpr uint64_t kPrimitiveRoot = 3;

    static uint64_t Add(uint64_t a, uint64_t b) {
        ASSERT(a < kPrime && b < kPrime);
        auto s = a + b;
        return s >= kPrime ? s - kPrime : s;
    }

    static uint64_t Substract(uint64_t a, uint64_t b) {
        ASSERT(a < kPrime && b < kPrime);
        return a < b ? kPrime - b + a : a - b;
    }

#ifdef _MSC_VER
    uint64_t Multiply(uint64_t a, uint64_t b) {
        ASSERT(a < kPrime && b < kPrime);
        uint64_t r = 0;
        for (auto _ = 0; _ < 64; ++_) {
            r = r > kHalfPrime ? (r << 1) - kPrime : r << 1;
            r = a & 0x8000000000000000ULL ? r + b : r;
            r = r > kPrime ? r - kPrime : r;
            a <<= 1;
        }
        return r;
    }
#else
    uint64_t Multiply(uint64_t a, uint64_t b)
    {
        ASSERT(a < kPrime && b < kPrime);
        ASSERT(std::numeric_limits<long double>::digits >= 64);
        long double fa = a;
        auto q = static_cast<uint64_t>(fa * b / kPrime);
        auto r = static_cast<int64_t>(a * b - q * kPrime) % static_cast<int64_t>(kPrime);
        return r < 0 ? r + kPrime : r;
    }
#endif

    static uint64_t Power(uint64_t a, int64_t b) {
        uint64_t r = 1;
        for (; b > 0; b >>= 1) {
            if (b & 1)
                r = Multiply(r, a);
            a = Multiply(a, a);
        }
        return r;
    }
}


using namespace RingArithmetic_NTT;


static void NTT(
    uint64_t *dest,
    uint64_t const *src,
    size_t size,
    uint64_t w,
    uint64_t scale) {

    std::vector<uint64_t> ws(size / 2);
    {
        uint64_t wi = 1;
        for (auto &v : ws) {
            v = wi;
            wi = Multiply(wi, w);
        }
    }

    if (scale == 1) {
        for (size_t i = 0, bc = static_cast<size_t>(log2(size)); i < size; ++i)
            dest[ReverseBits(i, bc)] = src[i];
    } else {
        for (size_t i = 0, bc = static_cast<size_t>(log2(size)); i < size; ++i)
            dest[ReverseBits(i, bc)] = Multiply(src[i], scale);
    }

    for (size_t s = 2; s <= size; s <<= 1) {
        size_t halfS = s / 2;
        size_t dw = size / s;
        for (size_t i = 0; i < size; i += s) {
            for (size_t j = 0, wi = 0; j < halfS; ++j, wi += dw) {
                auto c0 = dest[i + j], c1 = dest[i + j + halfS];
                auto wc1 = Multiply(c1, ws[wi]);
                dest[i + j] = Add(c0, wc1);
                dest[i + j + halfS] = Substract(c0, wc1);
            }
        }
    }
}


extern void NumberTheoreticTransform(
    uint64_t *dest,
    uint64_t const *src,
    size_t size) {
    
    ASSERT(IsPowerOf2(size));

    auto w = Power(kPrimitiveRoot, (kPrime - 1) / size);
    NTT(dest, src, size, w, 1);
}

extern void InverseNumberTheoreticTransform(
    uint64_t *dest,
    uint64_t const *src,
    size_t size) {
 
    ASSERT(IsPowerOf2(size));

    auto w = Power(kPrimitiveRoot, kPrime - 1 - (kPrime - 1) / size);
    auto scale = Power(size, kPrime - 2);
    NTT(dest, src, size, w, scale);
}