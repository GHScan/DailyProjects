#ifndef BIG_INTEGER_H
#define BIG_INTEGER_H


#include <cstdint>


#include "FFT.h"
#include "NTT.h"


size_t constexpr kInternalBaseBits = 32;
uint64_t constexpr kInternalBase = 1ULL << kInternalBaseBits;
size_t constexpr kDisplayBaseDecimals = 9;
uint64_t constexpr kDisplayBase = 1000000000;
size_t constexpr kKaratsubaThreashold = 80;
size_t constexpr kFFTThreashold = 5000;
size_t constexpr kNTTThreashold = 5000;


namespace MultiplePrecisionOp {

    inline uint32_t DecimalsToDisplayDigit(char const *decimals, size_t size) {
        uint32_t digit = 0;
        for (size_t i = 0; i < size; ++i) {
            ASSERT(decimals[i] >= '0' && decimals[i] <= '9');
            digit = digit * 10 + (decimals[i] - '0');
        }
        return digit;
    }

    inline size_t DisplayDigitToDecimals(uint32_t digit, char *decimals, size_t size, bool fillZero) {
        auto space = size;
        do {
            decimals[--space] = '0' + digit % 10;
            digit /= 10;
        } while (digit > 0);
        if (fillZero) {
            while (space > 0) decimals[--space] = '0';
        }
        else {
            copy(decimals + space, decimals + size, decimals);
        }
        return size - space;
    }

    inline std::vector<uint32_t> ChangeBase(std::vector<uint32_t> srcDigits, uint64_t srcBase, uint64_t destBase) {

        std::vector<uint32_t> destDigits;
        destDigits.reserve(static_cast<size_t>(ceil(log(double(destBase)) / log(double(srcBase)) * srcDigits.size())));

        do {
            uint64_t remainder = 0;
            for (auto i = srcDigits.size(); i > 0; --i) {
                remainder = remainder * srcBase + srcDigits[i - 1];
                srcDigits[i - 1] = static_cast<uint32_t>(remainder / destBase);
                remainder = remainder % destBase;
            }

            destDigits.push_back(static_cast<uint32_t>(remainder));

            for (; srcDigits.size() > 1 && srcDigits.back() == 0; srcDigits.pop_back());
            if (srcDigits.size() == 1 && srcDigits.back() == 0) break;

        } while (true);

        return destDigits;
    }

    inline std::vector<uint32_t> LShift(std::vector<uint32_t> const &digits, size_t n) {
        std::vector<uint32_t> result(digits.size() + n);
        copy(digits.begin(), digits.end(), result.begin() + n);
        return result;
    }

    inline std::vector<uint32_t> Add(std::vector<uint32_t> const &lhs, std::vector<uint32_t> const &rhs) {
        if (lhs.size() < rhs.size()) return Add(rhs, lhs);

        if (rhs.size() == 1) {
            if (rhs.front() == 0) return lhs;
        }

        auto result(lhs);
        auto resi = result.begin(), resie = result.end();

        uint64_t carry = 0;
        for (auto ri = rhs.begin(), rie = rhs.end(); ri != rie; ++ri, ++resi) {
            carry = carry + *resi + *ri;
            *resi = static_cast<uint32_t>(carry);
            carry >>= kInternalBaseBits;
        }
        for (; carry != 0 && resi != resie; ++resi) {
            carry += *resi;
            *resi = static_cast<uint32_t>(carry);
            carry >>= kInternalBaseBits;
        }

        if (carry > 0)
            result.push_back(static_cast<uint32_t>(carry));

        return result;
    }

    inline std::vector<uint32_t> Substract(std::vector<uint32_t> const &lhs, std::vector<uint32_t> const &rhs) {
        ASSERT(lhs.size() >= rhs.size());

        if (rhs.size() == 1) {
            if (rhs.front() == 0) return lhs;
        }

        auto result(lhs);
        auto resi = result.begin(), resie = result.end();

        uint64_t carry = kInternalBase;
        for (auto ri = rhs.begin(), rie = rhs.end(); ri != rie; ++ri, ++resi) {
            carry = carry + *resi - *ri;
            *resi = static_cast<uint32_t>(carry);
            carry = (carry >> kInternalBaseBits) + kInternalBase - 1;
        }
        for (; carry != kInternalBase && resi != resie; ++resi) {
            carry += *resi;
            *resi = static_cast<uint32_t>(carry);
            carry = (carry >> kInternalBaseBits) + kInternalBase - 1;
        }
        ASSERT(carry == kInternalBase);

        for (; result.size() > 1 && result.back() == 0; result.pop_back());

        return result;
    }


    inline uint64_t AccumulativeMultiply4(uint32_t const a[4], uint32_t b, uint32_t result[4], uint64_t carry) {
        uint64_t am[] = {
            uint64_t(a[0]) * b + result[0],
            uint64_t(a[1]) * b + result[1],
            uint64_t(a[2]) * b + result[2],
            uint64_t(a[3]) * b + result[3],
        };

        carry += am[0];
        result[0] = static_cast<uint32_t>(carry);
        carry >>= kInternalBaseBits;

        carry += am[1];
        result[1] = static_cast<uint32_t>(carry);
        carry >>= kInternalBaseBits;

        carry += am[2];
        result[2] = static_cast<uint32_t>(carry);
        carry >>= kInternalBaseBits;

        carry += am[3];
        result[3] = static_cast<uint32_t>(carry);
        carry >>= kInternalBaseBits;

        return carry;
    }

    inline std::vector<uint32_t> Multiply(std::vector<uint32_t> const &lhs, std::vector<uint32_t> const &rhs) {
        if (lhs.size() < rhs.size()) return Multiply(rhs, lhs);

        if (rhs.size() == 1) {
            if (rhs.front() == 0) return rhs;
            if (rhs.front() == 1) return lhs;
        }

        std::vector<uint32_t> result(lhs.size() + rhs.size());
        for (auto ri = rhs.begin(), rie = rhs.end(); ri != rie; ++ri) {
            auto resi = result.begin() + (ri - rhs.begin()), resie = result.end();

            uint64_t carry = 0;
            auto li = lhs.begin(), lie = lhs.end();
            for (; distance(li, lie) >= 4; li += 4, resi += 4) {
                carry = AccumulativeMultiply4(&*li, *ri, &*resi, carry);
            }
            for (; li != lie; ++li, ++resi) {
                carry += uint64_t(*li) * *ri + *resi;
                *resi = static_cast<uint32_t>(carry);
                carry >>= kInternalBaseBits;
            }
            for (; carry > 0 && resi != resie; ++resi) {
                carry += *resi;
                *resi = static_cast<uint32_t>(carry);
                carry >>= kInternalBaseBits;
            }
            ASSERT(carry == 0);
        }

        for (; result.size() > 1 && result.back() == 0; result.pop_back());

        return result;
    }

    inline std::vector<uint32_t> Multiply_Karatsuba(std::vector<uint32_t> const &lhs, std::vector<uint32_t> const &rhs) {
        if (lhs.size() < rhs.size()) return Multiply_Karatsuba(rhs, lhs);
        if (rhs.size() < kKaratsubaThreashold) {
            return Multiply(lhs, rhs);
        }

        auto half = (lhs.size() + 1) / 2;

        std::vector<uint32_t> llow(lhs.begin(), lhs.begin() + half);
        std::vector<uint32_t> lhi(lhs.begin() + half, lhs.end());
        std::vector<uint32_t> rlow(rhs.begin(), rhs.begin() + std::min(half, rhs.size()));
        std::vector<uint32_t> rhi;
        if (rhs.size() > half) rhi.assign(rhs.begin() + half, rhs.end());
        else rhi.resize(1);

        auto r0 = Multiply_Karatsuba(llow, rlow);
        auto r2 = Multiply_Karatsuba(lhi, rhi);
        auto r1 = Substract(Multiply_Karatsuba(Add(llow, lhi), Add(rlow, rhi)), Add(r0, r2));
        return Add(LShift(Add(LShift(r2, half), r1), half), r0);
    }

    inline std::vector<uint32_t> Multiply_FFT16(std::vector<uint32_t> const &lhs, std::vector<uint32_t> const &rhs) {
        std::vector<uint64_t> convolveResult((lhs.size() + rhs.size()) * 2);

        Convolve(
            reinterpret_cast<uint16_t const*>(&lhs[0]), lhs.size() * 2,
            reinterpret_cast<uint16_t const*>(&rhs[0]), rhs.size() * 2,
            &convolveResult[0], convolveResult.size(),
            [](uint16_t v) { return v; },
            [](std::complex<double> &v)
        {
            auto re = llround(v.real());
            ASSERT(re >= 0 && llround(v.imag()) == 0);
            return re;
        });

        std::vector<uint32_t> result;
        result.reserve(convolveResult.size() / 2);
        uint64_t carry = 0;
        for (size_t i = 0; i < convolveResult.size(); i += 2) {
            carry += (convolveResult[i] & 0xffffffff) + ((convolveResult[i + 1] & 0xffff) << 16);
            result.push_back(static_cast<uint32_t>(carry));
            carry >>= kInternalBaseBits;
            carry += (convolveResult[i] >> 32) + (convolveResult[i + 1] >> 16);
        }
        if (carry > 0)
            result.push_back(static_cast<uint32_t>(carry));
        ASSERT(carry < kInternalBase);

        for (; result.size() > 1 && result.back() == 0; result.pop_back());

        return result;
    }

    inline std::vector<uint32_t> Multiply_FFT8(std::vector<uint32_t> const &lhs, std::vector<uint32_t> const &rhs) {
        std::vector<uint64_t> convolveResult((lhs.size() + rhs.size()) * 4);

        Convolve(
            reinterpret_cast<uint8_t const*>(&lhs[0]), lhs.size() * 4,
            reinterpret_cast<uint8_t const*>(&rhs[0]), rhs.size() * 4,
            &convolveResult[0], convolveResult.size(),
            [](uint8_t v) { return v; },
            [](std::complex<double> &v)
        {
            auto re = llround(v.real());
            ASSERT(re >= 0 && llround(v.imag()) == 0);
            return re;
        });


        std::vector<uint32_t> result;
        result.reserve(convolveResult.size() / 4);
        uint64_t carry = 0;
        for (size_t i = 0; i < convolveResult.size(); i += 4) {
            carry += (convolveResult[i + 0] & 0xffffffff) << 0;
            carry += (convolveResult[i + 1] & 0x00ffffff) << 8;
            carry += (convolveResult[i + 2] & 0x0000ffff) << 16;
            carry += (convolveResult[i + 3] & 0x000000ff) << 24;
            result.push_back(static_cast<uint32_t>(carry));
            carry >>= kInternalBaseBits;
            carry += (convolveResult[i + 0] >> 32);
            carry += (convolveResult[i + 1] >> 24);
            carry += (convolveResult[i + 2] >> 16);
            carry += (convolveResult[i + 3] >> 8);
        }
        if (carry > 0)
            result.push_back(static_cast<uint32_t>(carry));
        ASSERT(carry < kInternalBase);

        for (; result.size() > 1 && result.back() == 0; result.pop_back());

        return result;
    }

    inline std::vector<uint32_t> Multiply_FFT(std::vector<uint32_t> const &lhs, std::vector<uint32_t> const &rhs) {

        if (CanConvolveIntegersAccurately(
            reinterpret_cast<uint16_t const*>(&lhs[0]), lhs.size() * 2,
            reinterpret_cast<uint16_t const*>(&rhs[0]), rhs.size() * 2)) {
            return Multiply_FFT16(lhs, rhs);
        }

        ASSERT(CanConvolveIntegersAccurately(
            reinterpret_cast<uint8_t const*>(&lhs[0]), lhs.size() * 4,
            reinterpret_cast<uint8_t const*>(&rhs[0]), rhs.size() * 4));
        return Multiply_FFT8(lhs, rhs);
    }

    inline std::vector<uint32_t> Multiply_NTT(std::vector<uint32_t> const &lhs, std::vector<uint32_t> const &rhs) {
        std::vector<uint64_t> convolveResult((lhs.size() + rhs.size()) * 2);
        
        NTConvolve(
            reinterpret_cast<uint16_t const*>(&lhs[0]), lhs.size() * 2,
            reinterpret_cast<uint16_t const*>(&rhs[0]), rhs.size() * 2,
            &convolveResult[0], convolveResult.size(),
            [](uint16_t v) { return v; },
            [](uint64_t &v) { return v; });

        std::vector<uint32_t> result;
        result.reserve(convolveResult.size() / 2);
        uint64_t carry = 0;
        for (size_t i = 0; i < convolveResult.size(); i += 2) {
            carry += (convolveResult[i] & 0xffffffff) + ((convolveResult[i + 1] & 0xffff) << 16);
            result.push_back(static_cast<uint32_t>(carry));
            carry >>= kInternalBaseBits;
            carry += (convolveResult[i] >> 32) + (convolveResult[i + 1] >> 16);
        }
        if (carry > 0)
            result.push_back(static_cast<uint32_t>(carry));
        ASSERT(carry < kInternalBase);

        for (; result.size() > 1 && result.back() == 0; result.pop_back());

        return result;

    }
}

class BigInteger final {
public:
    BigInteger(uint64_t n) : mDigits(n < kInternalBase ? 1 : 2, static_cast<uint32_t>(n)) {
        if (n >= kInternalBase)
            mDigits[1] = static_cast<uint32_t>(n >> kInternalBaseBits);
    }

    BigInteger(char const *str, size_t size) {

        std::vector<uint32_t> displayDigits;
        displayDigits.reserve((size + kDisplayBaseDecimals - 1) / kDisplayBaseDecimals);

        for (auto p = str + size; p >= str; p -= kDisplayBaseDecimals) {
            auto s = std::max(p - kDisplayBaseDecimals, str);
            displayDigits.push_back(MultiplePrecisionOp::DecimalsToDisplayDigit(s, p - s));
        }

        mDigits = MultiplePrecisionOp::ChangeBase(move(displayDigits), kDisplayBase, kInternalBase);
    }

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

    std::string ToString() const {

        std::vector<uint32_t> displayDigits = MultiplePrecisionOp::ChangeBase(mDigits, kInternalBase, kDisplayBase);

        std::string str(displayDigits.size() * kDisplayBaseDecimals, ' ');
        auto i = MultiplePrecisionOp::DisplayDigitToDecimals(displayDigits.back(), &str[0], kDisplayBaseDecimals, false);
        str.resize(str.size() - (kDisplayBaseDecimals - i));

        for (auto j = displayDigits.size() - 2; i < str.size(); i += kDisplayBaseDecimals, --j) {
            MultiplePrecisionOp::DisplayDigitToDecimals(displayDigits[j], &str[i], kDisplayBaseDecimals, true);
        }

        return str;
    }

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
BigInteger BigInteger::kOne(1);

inline BigInteger operator + (BigInteger const &lhs, BigInteger const &rhs) {
    return BigInteger(MultiplePrecisionOp::Add(lhs.Digits(), rhs.Digits()));
}

inline BigInteger operator - (BigInteger const &lhs, BigInteger const &rhs) {
    return BigInteger(MultiplePrecisionOp::Substract(lhs.Digits(), rhs.Digits()));
}

inline BigInteger operator * (BigInteger const &lhs, BigInteger const &rhs) {
    if (lhs.Size() < kKaratsubaThreashold || rhs.Size() < kKaratsubaThreashold) {
        return BigInteger(MultiplePrecisionOp::Multiply(lhs.Digits(), rhs.Digits()));
    }
    if (lhs.Size() < kNTTThreashold || rhs.Size() < kNTTThreashold) {
        return BigInteger(MultiplePrecisionOp::Multiply_Karatsuba(lhs.Digits(), rhs.Digits()));
    }
#ifdef _MSC_BUILD
    return BigInteger(MultiplePrecisionOp::Multiply_FFT(lhs.Digits(), rhs.Digits()));
#else
    return BigInteger(MultiplePrecisionOp::Multiply_NTT(lhs.Digits(), rhs.Digits()));
#endif
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
