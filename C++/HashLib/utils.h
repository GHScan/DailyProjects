
#ifndef UTILS_H
#define UTILS_H

#include <stdint.h>
#include <assert.h>

#include <string>

inline uint32_t reverseUint(uint32_t i) {
    auto p = (const uint8_t*)&i;
    return (p[0] << 24u) | (p[1] << 16u) | (p[2] << 8u) | (p[3] << 0u);
}
inline uint64_t reverseUint(uint64_t i) {
    return (uint64_t(reverseUint(uint32_t(i))) << 32) | uint64_t(reverseUint(uint32_t(i >> 32)));
}

inline bool isLittleEndian(int i = 1) {
    return 1 == (char&)i;
}

template<typename UintT>
inline UintT readUint_littleEndian(const void *p) {
    auto r = *(const UintT*)p;
    return isLittleEndian() ? r : reverseUint(r);
}
template<typename UintT>
inline void writeUint_littleEndian(void *p, UintT i) {
    if (!isLittleEndian()) i = reverseUint(i);
    *(UintT*)p = i;
}

template<typename UintT>
inline UintT readUint_bigEndian(const void *p) {
    auto r = *(const UintT*)p;
    return isLittleEndian() ? reverseUint(r) : r;
}
template<typename UintT>
inline void writeUint_bigEndian(void *p, UintT i) {
    if (isLittleEndian()) i = reverseUint(i);
    *(UintT*)p = i;
}

inline uint32_t leftRotate(uint32_t a, uint32_t n) {
    return (a << n) | (a >> (32 - n));
}
inline uint32_t rightRotate(uint32_t a, uint32_t n) {
    return (a >> n) | (a << (32 - n));
}

inline string binary2string(const void *_p, int n) {
    const uint8_t *p = (const uint8_t*)_p;
    assert(p != nullptr && n > 0);

    const char *HEX = "0123456789abcdef";

    string s(n * 2, 0);
    for (int i = 0; i < n; ++i) {
        s[i * 2 + 0] = HEX[p[i] >> 4];
        s[i * 2 + 1] = HEX[p[i] & 0xf];
    }
    return s;
}

#define ASIZE(a) (sizeof(a) / sizeof((a)[0]))

#endif
