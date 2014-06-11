#ifndef UTILS_H
#define UTILS_H

typedef long PtrValue;
static_assert(sizeof(PtrValue) == sizeof(void*), "PtrValue should be big engough to store a pointer!");

template<typename IterT>
inline uint32_t hashRange(IterT begin, IterT end) {
    auto h = std::hash<typename iterator_traits<IterT>::value_type>();

    uint32_t seed = 0;
    for (; begin != end; ++begin) {
        seed ^= h(*begin) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    }
    return seed;
}

#endif
