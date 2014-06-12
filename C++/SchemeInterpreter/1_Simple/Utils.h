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

template<typename T>
inline void FREE(T*& p, typename is_pod<T>::type* =0) {
    ::free(p);
    p = nullptr;
}

template<typename T>
inline void DELETE(T*& p) {
    delete p;
    p = nullptr;
}

template<typename T>
inline void DELETE_ARRAY(T*& p) {
    delete[] p;
    p = nullptr;
}

template<typename T, int N>
inline int ARRAY_SIZE(T (&a)[N]) {
    return N;
}

template<typename DerivedT, typename BaseT>
DerivedT* polymorphic_cast(BaseT *p) {
#ifdef _DEBUG
    return dynamic_cast<DerivedT*>(p);
#else
    return static_cast<DerivedT*>(p);
#endif
}

#endif
