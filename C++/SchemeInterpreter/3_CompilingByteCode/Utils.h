#ifndef UTILS_H
#define UTILS_H

typedef long PtrValue;
static_assert(sizeof(PtrValue) == sizeof(void*), "PtrValue should be big engough to store a pointer!");
static const int PTR_ALIGNMENT = 8;
static const int LOG_PTR_ALIGNMENT = 3;

string format(const char *fmt, ...);
string escapeString(const string &s, const char *specialChars);
string unescapeString(const string &s);

class AssertFailedException: public exception {
public:
    explicit AssertFailedException(const string &what): mWhat(what) {
    }

    const char* what() const noexcept {
        return mWhat.c_str();
    }

private:
    string mWhat;
};

#ifdef _DEBUG
#define ASSERT      assert
#else
#define ASSERT(b)   if (b); else throw AssertFailedException(#b)
#endif

inline uint32_t hashMerge(uint32_t seed, uint32_t h) {
    return seed ^ (h + 0x9e3779b9  + (seed << 6) + (seed >> 2));
}

template<typename IterT>
inline uint32_t hashRange(IterT begin, IterT end) {
    hash<typename iterator_traits<IterT>::value_type> hashf;

    uint32_t h = hashMerge(0, 0);
    for (; begin != end; ++begin) {
        h = hashMerge(h, hashf(*begin));
    }
    return h;
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

template<int ALIGNMENT, typename T>
inline T roundUp(T v) {
    return (v + ALIGNMENT - 1) / ALIGNMENT * ALIGNMENT;
}

template<typename IntT>
inline void checkedAssign(IntT *p, long long v) {
    ASSERT(v >= numeric_limits<IntT>::min() && v <= numeric_limits<IntT>::max());
    *p = (IntT)v;
}

template<typename T>
inline T signBit(T v) {
    return v & (T(1) << (sizeof(T) * 8 - 1));
}

#endif
