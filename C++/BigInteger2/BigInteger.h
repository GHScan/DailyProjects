#ifndef BIG_INTEGER_H
#define BIG_INTEGER_H


#include <algorithm>


#include "Utility.h"
#include "MultiplePrecisionOp.h"


size_t constexpr kDisplayBaseDecimals = 9;
uint64_t constexpr kDisplayBase = 1000000000;


class BigInteger final {
public:
    BigInteger(uint64_t n) : mDigits(n < kInternalBase ? 1 : 2, static_cast<uint32_t>(n)) {
        if (n >= kInternalBase)
            mDigits[1] = static_cast<uint32_t>(n >> kInternalBaseBits);
    }

    BigInteger(char const *str, size_t size);

    BigInteger(std::string const &str) : BigInteger(str.c_str(), str.size()) {
    }

    explicit BigInteger(std::vector<uint32_t> digits) : mDigits(move(digits)) {
        ASSERT(mDigits.size() == 1 || mDigits.back() > 0);
    }

    BigInteger(BigInteger const&) = default;
    BigInteger(BigInteger &&) = default;
    BigInteger& operator = (BigInteger const&) = default;
    BigInteger& operator = (BigInteger &&) = default;

    ~BigInteger() noexcept = default;

    void Swap(BigInteger &other) noexcept {
        swap(mDigits, other.mDigits);
    }

    std::string ToString() const;

    uint64_t ToInt64() const {
        ASSERT(mDigits.size() <= 2);

        uint64_t v = mDigits[0];
        if (mDigits.size() == 2)
            v |= uint64_t(mDigits[1]) << kInternalBaseBits;
        return v;
    }

    size_t Size() const {
        return mDigits.size();
    }

    size_t BitCount() const {
        return Size() * kInternalBaseBits;
    }

    std::vector<uint32_t> const& Digits() const {
        return mDigits;
    }

public:
    static BigInteger kOne;

private:
    std::vector<uint32_t> mDigits;
};


inline BigInteger operator + (BigInteger const &lhs, BigInteger const &rhs) {
    return BigInteger(MultiplePrecisionOp::Add(lhs.Digits(), rhs.Digits()));
}

inline BigInteger operator - (BigInteger const &lhs, BigInteger const &rhs) {
    return BigInteger(MultiplePrecisionOp::Substract(lhs.Digits(), rhs.Digits()));
}

inline BigInteger operator * (BigInteger const &lhs, BigInteger const &rhs) {
    return BigInteger(MultiplePrecisionOp::FastMultiply(lhs.Digits(), rhs.Digits()));
}

BigInteger operator / (BigInteger const &lhs, BigInteger const &rhs) = delete;
BigInteger operator % (BigInteger const &lhs, BigInteger const &rhs) = delete;

namespace std {
    template<>
    inline void swap<BigInteger>(BigInteger &lhs, BigInteger &rhs) noexcept {
        lhs.Swap(rhs);
    }
}

inline std::ostream& operator << (std::ostream &so, BigInteger const &i) {
    return so << i.ToString();
}

inline std::istream& operator >> (std::istream&si, BigInteger &i) {
    std::string s;
    si >> s;
    i = BigInteger(s);
    return si;
}

inline BigInteger Factorial(int32_t n, int32_t d = 1) {
    if (n <= d) return BigInteger(std::max(1, n));
    return Factorial(n, d * 2) * Factorial(n - d, d * 2);
}

template<typename T>
struct Matrix22 {
public:
    Matrix22(T v00, T v01, T v10, T v11) : mValues{ { v00, v01 },{ v10, v11 } } {
    }

    T const& operator () (size_t i, size_t j) const {
        ASSERT(i >= 0 && i < 2);
        ASSERT(j >= 0 && j < 2);
        return mValues[i][j];
    }

    Matrix22 operator * (Matrix22 const &other) const {
        return Matrix22(
            (*this)(0, 0) * other(0, 0) + (*this)(0, 1) * other(1, 0),
            (*this)(0, 0) * other(0, 1) + (*this)(0, 1) * other(1, 1),
            (*this)(1, 0) * other(0, 0) + (*this)(1, 1) * other(1, 0),
            (*this)(1, 0) * other(0, 1) + (*this)(1, 1) * other(1, 1)
        );
    }

public:
    static Matrix22 kOne;

private:
    T mValues[2][2];
};

template<typename T>
Matrix22<T> Matrix22<T>::kOne(1, 0, 0, 1);

template<typename T>
inline T Power(T a, uint32_t b) {
    T r(T::kOne);
    for (; b > 0; b >>= 1) {
        if (b & 1) 
            r = r * a;
        a = a * a;
    }
    return r;
}

inline BigInteger Fibonacci(uint32_t i) {
    return Power(Matrix22<BigInteger>(1, 1, 1, 0), i)(0, 0);
}

#endif
