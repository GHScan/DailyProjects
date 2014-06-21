#ifndef UTILS_H
#define UTILS_H

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

template<typename T>
inline T signBit(T v) {
    return v & (T(1) << (sizeof(T) * 8 - 1));
}

#endif
