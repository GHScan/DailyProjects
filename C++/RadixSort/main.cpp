#include <cstdio>
#include <cassert>

#include <chrono>
#include <limits>
#include <algorithm>
#include <numeric>
#include <vector>


template<typename TFunc>
inline double Time(TFunc const &f, size_t loop = 3) {
    using namespace std::chrono;

    auto t = std::numeric_limits<double>::max();
    for (size_t i = 0; i < loop; ++i) {
        auto start = high_resolution_clock::now();
        f(i);
        auto end = high_resolution_clock::now();
        t = std::min(t, duration<double>(end - start).count());
    }
    return t;
}

template<typename T>
static bool IsAscentSequence(T const *begin, T const *end) {
    for (auto it = begin; it + 1 != end; ++it) {
        if (!(it[0] < it[1])) return false;
    }
    return true;
}

template<typename T, std::enable_if_t<std::is_unsigned<T>::value, int> = 0>
void RadixSort(T *begin, T* end) {
    size_t size = end - begin;

    thread_local std::vector<T> gBuf;
    gBuf.resize(size);

    auto src = begin, dst = gBuf.data();
    for (size_t radix = 0; radix < sizeof(T); ++radix) {
        size_t const shift = radix * 8;

        size_t counters[256] = {0};
        for (size_t i = 0; i < size; ++i)
            ++counters[static_cast<uint8_t>(src[i] >> shift)];

        bool ordered = false;
        size_t offs[256];
        offs[0] = 0;
        for (size_t i = 1; i < 256; ++i) {
            size_t c = counters[i - 1];
            if (c == size) {
                ordered = true;
                break;
            }
            offs[i] = offs[i - 1] + c;
        }
        if (ordered)
            continue;

        for (size_t i = 0; i < size; ++i) {
            auto value = src[i];
            dst[offs[static_cast<uint8_t>(value >> shift)]++] = value;
        }

        std::swap(src, dst);
    }

    if (src != begin)
        std::memcpy(begin, src, size * sizeof(T));
}

int main() {
    using UInt = uint32_t;

    for (size_t len : {1 << 10, 1 << 12, 1 << 14, 1 << 16, 1 << 18, 1 << 20, 1 << 22,}) {

        std::vector<UInt> ints(len);
        iota(ints.begin(), ints.end(), 0);
        random_shuffle(ints.begin(), ints.end());

        size_t repeat = 10;

        // warm up
        std::vector<std::vector<UInt>> tmps(2, ints);
        std::sort(&tmps[0][0], &tmps[0][0] + len);
        RadixSort(&tmps[0][0], &tmps[0][0] + len);

        tmps.assign(repeat, ints);
        auto stlSortTime = Time([&](auto i) {
            auto &tmp = tmps[i];
            std::sort(&tmp[0], &tmp[0] + len);
        }, repeat);
        for (auto &tmp : tmps)
            assert(IsAscentSequence(&tmp[0], &tmp[0] + tmp.size()));

        tmps.assign(repeat, ints);
        auto radixSortTime = Time([&](auto i) {
            auto &tmp = tmps[i];
            RadixSort(&tmp[0], &tmp[0] + len);
        }, repeat);
        for (auto &tmp : tmps)
            assert(IsAscentSequence(&tmp[0], &tmp[0] + tmp.size()));

        printf("%zu K :\n\tSTLSort=%.3f MKey/s(%.3f ms)\n\tRadixSort=%.3f MKey/s(%.3f ms)\n", 
            len / 1024, 
            len * 1e-6 / stlSortTime, stlSortTime * 1e3, 
            len * 1e-6 / radixSortTime, radixSortTime * 1e3);
    }
}
