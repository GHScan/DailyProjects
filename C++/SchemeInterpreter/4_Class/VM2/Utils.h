#ifndef UTILS_H
#define UTILS_H

#ifdef BIT_64
typedef int64_t PtrValue;
#else
typedef int32_t PtrValue;
#endif

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

    const char* what() const throw() {
        return mWhat.c_str();
    }

private:
    string mWhat;
};

#define _TO_STRING(e) #e
#define TO_STRING(e) _TO_STRING(e)
#define FILE_LINE __FILE__ "(" TO_STRING(__LINE__) "): "

#ifndef NDEBUG
#define ASSERT      assert
#else
#ifdef DEBUG_ONLY_ASSERT
#define ASSERT(b)   
#else
#define ASSERT(b)   if (b); else throw AssertFailedException(FILE_LINE #b)
#endif
#endif

inline uint32_t hashMerge(uint32_t seed, uint32_t h) {
    return seed ^ (h + 0x9e3779b9  + (seed << 6) + (seed >> 2));
}

template<typename IterT>
inline uint32_t hashRange(IterT begin, IterT end) {
    hash<typename iterator_traits<IterT>::value_type> hashf;

    uint32_t h = hashMerge(0, 0);
    for (; begin != end; ++begin) {
        h = hashMerge(h, uint32_t(hashf(*begin)));
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

template<typename DestT, typename SrcT>
inline DestT force_cast(SrcT v) {
    static_assert(sizeof(DestT) == sizeof(SrcT), "");

    union { SrcT src; DestT dest;} o = {v};
    return o.dest;
}

template<typename DestT, typename SrcT>
inline DestT narrowing_cast(SrcT v) {
    static_assert(sizeof(DestT) < sizeof(SrcT), "");

    union { SrcT src; DestT dest;} o = {v};
    return o.dest;
}

template<typename DestT, typename SrcT>
inline DestT widening_cast(SrcT v) {
    static_assert(sizeof(DestT) > sizeof(SrcT), "");

    union { SrcT src; DestT dest;} o = {v};
    return o.dest;
}

template<typename IntT>
inline void checkedAssign(IntT *p, long long v) {
    ASSERT(v >= numeric_limits<IntT>::min() && v <= numeric_limits<IntT>::max());
    *p = (IntT)v;
}

#endif
