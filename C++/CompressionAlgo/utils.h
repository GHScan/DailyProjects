#ifndef UTILS_H
#define UTILS_H

#include <stdint.h>
#include <math.h>

#include <type_traits>

#define ASIZE(a) (sizeof(a) / sizeof((a)[0]))

inline bool isLittleEndian(int i = 1) {
    return 1 == (char&)i;
}

inline uint8_t reverseInt(uint8_t i) {
    return i;
}
inline uint16_t reverseInt(uint16_t i) {
    return uint16_t(i << 8) | uint16_t(i >> 8);
}
inline uint32_t reverseInt(uint32_t i) {
    return (uint32_t(reverseInt(uint16_t(i))) << 16) | reverseInt(uint16_t(i >> 16));
}
inline uint64_t reverseInt(uint64_t i) {
    return (uint64_t(reverseInt(uint32_t(i))) << 32) | reverseInt(uint32_t(i >> 32));
}

template<typename T>
inline T fromLittleEndian(T v) {
    return isLittleEndian() ? v : reverseInt(v);
}
template<typename T>
inline T toLittleEndian(T v) {
    return isLittleEndian() ? v : reverseInt(v);
}

inline int pow2Roundup(int size) {
    return 1 << ((int)ceil(log(size) / log(2)));
}

int primeRounddown(int size);

#endif
