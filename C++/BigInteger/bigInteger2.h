#ifndef BIG_INTEGER_H
#define BIG_INTEGER_H

#include <stdint.h>

#include <type_traits>
#include <limits>

#include "Helper.h"

template<typename T, typename DoubleT, typename TripleT>
struct ExtendedPrecisionOp {
    static_assert(sizeof(DoubleT) >= 2 * sizeof(T), "");
    static_assert(sizeof(TripleT) >= 3 * sizeof(T), "");

    static const DoubleT BASE = DoubleT(1) << (sizeof(T) * 8);
    static const int RECURSIVE_LIMIT_N = 128 / sizeof(T);

    static int length(const T *a, int na) {
        assert(na > 0);
        int i = na - 1;
        for (; i > 0 && a[i] == 0; --i);
        return i + 1;
    }
    static int compare(const T *a, int na, const T *b, int nb) {
        assert(length(a, na) == na && length(b, nb) == nb);
        if (na != nb) return na < nb ? -1 : 1;
        else return compare(a, b, na);
    }
    static int compare(const T *a, const T *b, int n) {
        assert(n > 0);
        int i = n - 1;
        for (; i > 0 && a[i] == b[i]; --i);
        if (a[i] < b[i]) return -1;
        else return a[i] > b[i] ? 1 : 0;
    }
    static bool isZero(const T* a, int na) {
        return na == 1 && a[0] == 0;
    }
    static void setZero(T *a, int na) {
        for (int i = 0; i < na; ++i) a[i] = T();
    }

    static T addSingleInplace(T *a, int na, DoubleT carry) {
        assert(na > 0);
        assert(carry < BASE);

        for (int i = 0; carry && i < na; ++i) {
            carry += a[i];
            a[i] = carry % BASE;
            carry /= BASE;
        }

        assert(carry <= 1);
        return carry;
    }

    static T subSingleInplace(T *a, int na, DoubleT borrow) {
        assert(na > 0);
        assert(borrow < BASE);

        for (int i = 0; borrow && i < na; ++i) {
            if (a[i] < borrow) {
                a[i] += BASE - borrow;
                borrow = 1;
            } else {
                a[i] -= borrow;
                borrow = 0;
            }
        }

        assert(borrow <= 1);
        return borrow;
    }

    static T mulSingle(T *r, int nr, const T *a, int na, DoubleT b) {
        assert(length(a, na) == na);
        assert(r != a);
        assert(nr == na);
        assert(b < BASE);

        DoubleT carry = 0;
        for (int i = 0; i < na; ++i) {
            carry += a[i] * b;
            r[i] = carry % BASE;
            carry /= BASE;
        }

        assert(carry < BASE);
        return carry;
    }
    static T mulSingleInplace(T *r, int nr, DoubleT a) {
        assert(a < BASE);

        DoubleT carry = 0;
        for (int i = 0; i < nr; ++i) {
            carry += r[i] * a;
            r[i] = carry % BASE;
            carry /= BASE;
        }

        assert(carry < BASE);
        return carry;
    }
    static T mulSingleAccumulate(T * RESTRICT r, int nr, const T* RESTRICT a, int na, DoubleT b) {
        assert(length(a, na) == na);
        assert(r != a);
        assert(nr >= na);
        assert(b < BASE);

        DoubleT carry = 0;
        for (int i = 0; i < na; ++i) {
            carry += a[i] * b + r[i];
            r[i] = carry % BASE;
            carry /= BASE;
        }

        assert(carry < BASE);
        if (nr > na) return addSingleInplace(r + na, nr - na, carry);
        else return carry;
    }

    // enable: r == a
    static T divSingle(T *r, int nr, const T *a, int na, DoubleT b) {
        assert(length(a, na) == na);
        assert(nr == na);
        assert(b < BASE);

        DoubleT remain = 0;
        for (int i = na - 1; i >= 0; --i) {
            remain = remain * BASE + a[i];
            r[i] = remain / b;
            remain %= b;
        }

        assert(remain < b);
        return remain;
    }
    static T divSingleInplace(T *a, int na, DoubleT b) {
        return divSingle(a, na, a, na, b);
    }

    // enable: a==b
    static T add(T * RESTRICT r, int nr, const T *a, int na, const T *b, int nb) {
        if (na < nb) {
            swap(a, b);
            swap(na, nb);
        }

        assert(length(a, na) == na && length(b, nb) == nb);
        assert(na >= nb && nr == na);
        assert(r != a && r != b);

        DoubleT carry = 0;
        for (int i = 0; i < nb; ++i) {
            carry = carry + a[i] + b[i];
            r[i] = carry % BASE;
            carry /= BASE;
        }
        for (int i = nb; i < na; ++i) {
            carry += a[i];
            r[i] = carry % BASE;
            carry /= BASE;
        }

        assert(carry <= 1);
        return carry;
    }
    // enable: r == a
    static T addInplace(T *r, int nr, const T *a, int na) {
        assert(length(a, na) == na);
        assert(nr >= na);

        DoubleT carry = 0;
        for (int i = 0; i < na; ++i) {
            carry = carry + r[i] + a[i];
            r[i] = carry % BASE;
            carry /= BASE;
        }
        for (int i = na; carry && i < nr; ++i) {
            carry += r[i];
            r[i] = carry % BASE;
            carry /= BASE;
        }

        assert(carry <= 1);
        return carry;
    }

    // enable: na==nb&&compare(a,b,na)<0
    static T sub(T * RESTRICT r, int nr, const T * RESTRICT a, int na, const T * RESTRICT b, int nb) {
        assert(length(a, na) == na && length(b, nb) == nb);
        assert(r != a && r !=b && a != b);
        assert(na >= nb && nr == na);

        DoubleT borrow = 0;
        for (int i = 0; i < nb; ++i) {
            borrow += b[i];
            if (a[i] < borrow) {
                r[i] = a[i] + BASE - borrow;
                borrow = 1;
            } else {
                r[i] = a[i] - borrow;
                borrow = 0;
            }
        }
        assert(borrow <= 1);

        for (int i = nb; i < na; ++i) {
            if (a[i] < borrow) {
                assert(a[i] == 0 && borrow == 1);
                r[i] = BASE - 1;
            } else {
                r[i] = a[i] - borrow;
                borrow = 0;
            }
        }

        assert(borrow <= 1);
        return borrow;
    }
    // enable: na==nb&&compare(a,b,na)<0
    static T subInplace(T *r, int nr, const T *a, int na) {
        assert(length(a, na) == na);
        assert(nr >= na);
        assert(r != a);

        DoubleT borrow = 0;
        for (int i = 0; i < na; ++i) {
            borrow += a[i];
            if (r[i] < borrow) { 
                r[i] += BASE - borrow;
                borrow = 1;
            } else {
                r[i] -= borrow;
                borrow = 0;
            }
        }
        for (int i = na; borrow && i < nr; ++i) {
            if (r[i] < borrow) {
                assert(r[i] == 0 && borrow == 1);
                r[i] = BASE - 1;
            } else {
                r[i] -= borrow;
                borrow = 0;
            }
        }

        assert(borrow <= 1);
        return borrow;
    }

    static void _mulSmallAccumulate(T * RESTRICT r, int nr, const T* RESTRICT a, int na, const T * RESTRICT b, int nb) {
        assert(nr >= na + nb);
        assert(r != a && r != b && a != b);
        na = length(a, na);
        nb = length(b, nb);
        if (na < nb) {
            swap(a, b);
            swap(na, nb);
        }
        for (int i = 0; i < nb; ++i) {
            mulSingleAccumulate(r + i, nr - i, a, na, b[i]);
        }
    }
    static void _mulSameSizeAccumulate(T * RESTRICT r, int nr, const T* RESTRICT a, const T* RESTRICT b, int n, T * RESTRICT t, int nt) {
        assert(nr >= n + n);
        assert(nt >= n + n);
        assert(r != a && r != b && r != t);
        assert(a != b && a != t && b != t);

        if (n <= RECURSIVE_LIMIT_N) {
            _mulSmallAccumulate(r, nr, a, n, b, n);
            return;
        }

        int half = n / 2;
        const T * RESTRICT a1 = a + half, * RESTRICT a0 = a, * RESTRICT b1 = b + half, * RESTRICT b0 = b;
        int na1 = n - half, na0 = half, nb1 = n - half, nb0 = half;

        // (a1+a0)(b1+b0)
        {
            T *RESTRICT a10 = t;
            int na10 = na1;
            memcpy(a10, a1, na1 * sizeof(T));
            a10[na10] = addInplace(a10, na10, a0, length(a0, na0));
            ++na10;

            T *RESTRICT b10 = a10 + na10;
            int nb10 = nb1;
            memcpy(b10, b1, nb1 * sizeof(T));
            b10[nb10] = addInplace(b10, nb10, b0, length(b0, nb0));
            ++nb10;

            _mulSameSizeAccumulate(r + half, nr - half, a10, b10, nb10, b10 + nb10, nt - na10 - nb10);
        }
        // a1b1
        {
            T *RESTRICT a1b1 = t;
            int na1b1 = na1 + nb1;
            setZero(a1b1, na1b1);
            _mulSameSizeAccumulate(a1b1, na1b1, a1, b1, nb1, a1b1 + na1b1, nt - na1b1);
            na1b1 = length(a1b1, na1b1);

            if (addInplace(r + 2 * half, nr - 2 * half, a1b1, na1b1)) {
                assert(0);
            }
            if (subInplace(r + half, nr - half, a1b1, na1b1)) {
                assert(0);
            }
        }
        // a0b0
        {
            T *RESTRICT a0b0 = t;
            int na0b0 = na0 + nb0;
            setZero(a0b0, na0b0);
            _mulSameSizeAccumulate(a0b0, na0b0, a0, b0, nb0, a0b0 + na0b0, nt - na0b0);
            na0b0 = length(a0b0, na0b0);

            if (addInplace(r, nr, a0b0, na0b0)) {
                assert(0);
            }
            if (subInplace(r + half, nr - half, a0b0, na0b0)) {
                assert(0);
            }
        }
    }
    static void _mulDiffSizeAccumulate(T * RESTRICT r, int nr, const T * RESTRICT a, int na, const T * RESTRICT b, int nb, T * RESTRICT t, int nt) {
        if (na < nb) {
            swap(a, b);
            swap(na, nb);
        }
        assert(na > nb);
        assert(nr >= na + nb);
        assert(nt >= nb * 2);
        assert(r != a && r != b && r != t);
        assert(a != b && a != t && b != t);

        if (na <= RECURSIVE_LIMIT_N || nb <= 2) {
            _mulSmallAccumulate(r, nr, a, na, b, nb);
            return;
        }

        int i = 0;
        for (; i + nb <= na; i += nb) {
            _mulSameSizeAccumulate(r + i, nr - i, a + i, b, nb, t, nt);
        }
        if (i < na) {
            _mulDiffSizeAccumulate(r + i, nr - i, b, nb, a + i, na - i, t, nt);
        }
    }
    // enable: a==b
    static void mul(T * RESTRICT r, int nr, const T *a, int na, const T *b, int nb, T * RESTRICT t, int nt) {
        assert(isZero(r, length(r, nr)));
        assert(length(a, na) == na && length(b, nb) == nb);
        assert(r != a && r != b && r != t);
        assert(a != t && b != t);
        assert(nr == na + nb);

        if (a == b) square(r, nr, a, na, t, nt);
        else if (na == nb) _mulSameSizeAccumulate(r, nr, a, b, na, t, nt);
        else _mulDiffSizeAccumulate(r, nr, a, na, b, nb, t, nt);
    }

    static void _squareSmallAccumulate(T * RESTRICT r, int nr, const T * RESTRICT a, int na) {
        assert(nr >= na * 2);
        assert(r != a);
        na = length(a, na);

        for (int i = 0; i < na; ++i) {
            TripleT v = a[i];
            TripleT carry = r[i + i] + v * v;
            r[i + i] = carry % BASE;
            carry /= BASE;

            v *= 2;
            for (int j = i + 1; j < na; ++j) {
                carry += r[i + j] + v * a[j];
                r[i + j] = carry % BASE;
                carry /= BASE;
            }
            for (int j = i + na; carry && j < nr; ++j) {
                carry += r[j];
                r[j] = carry % BASE;
                carry /= BASE;
            }
            assert(carry == 0);
        }
    }
    static void _squareAccumulate(T * RESTRICT r, int nr, const T * RESTRICT a, int na, T * RESTRICT t, int nt) {
        assert(r != a && r != t);
        assert(nr >= na * 2);
        assert(nt >= na * 2);

        if (na <= RECURSIVE_LIMIT_N) {
            _squareSmallAccumulate(r, nr, a, na);
            return;
        }

        int half = na / 2;
        const T* RESTRICT a1 = a + half, * RESTRICT a0 = a;
        int na1 = na - half, na0 = half;

        _squareAccumulate(r + 2 * half, nr - 2 * half, a1, na1, t, nt);
        _squareAccumulate(r, nr, a0, na0, t, nt);

        T *RESTRICT a10 = t;
        int na10 = na1 + na0;
        setZero(a10, na10);
        if (na1 == na0) {
            _mulSameSizeAccumulate(a10, na10, a1, a0, na0, a10 + na10, nt - na10);
        } else {
            _mulDiffSizeAccumulate(a10, na10, a1, na1, a0, na0, a10 + na10, nt - na10);
        }
        a10[na10] = mulSingleInplace(a10, na10, 2);
        ++na10;
        na10 = length(a10, na10);
        if (addInplace(r + half, nr - half, a10, na10)) {
            assert(0);
        }
    }
    static void square(T * RESTRICT r, int nr, const T * RESTRICT a, int na, T * RESTRICT t, int nt) {
        assert(isZero(r, length(r, nr)));
        assert(length(a, na) == na);
        assert(r != a && r != t);
        assert(nr == na * 2);
        assert(nt >= na * 2);

        _squareAccumulate(r, nr, a, na, t, nt);
    }

    static T _divWithSingleQuotient(T * RESTRICT a, int na, const T * RESTRICT b, int nb, T * RESTRICT t, int nt) {
        assert(length(b, nb) == nb);
        assert(nb >= 2);
        assert(na == nb + 1);
        assert(nt == nb + 1);
        assert(a != b);
        assert(compare(a + 1, length(a + 1, na - 1), b, nb) < 0);

        TripleT _a(a[na - 1]);
        _a = _a * BASE + a[na - 2];
        _a = _a * BASE + a[na - 3];
        TripleT _b(b[nb - 1]);
        _b = _b * BASE + b[nb - 2];

        T q;
        if (_a / _b < BASE) {
            q = T(_a / _b);
        } else {
            assert(_a / _b == BASE);
            q = BASE - 1;
        }

        t[nt - 1] = mulSingle(t, nt - 1, b, nb, q);
        if (compare(a, t, na) < 0) {
            assert(q > 0);
            --q;
            t[nt - 1] = mulSingle(t, nt - 1, b, nb, q);
        }
        nt = length(t, nt);

        if (subInplace(a, na, t, nt)) {
            assert(0);
        }
        assert(compare(a, length(a, na), b, nb) < 0);

        return q;
    }
    static void div(T * RESTRICT a, int na, const T * RESTRICT b, int nb, T * RESTRICT q, int nq, T * RESTRICT t, int nt) {
        assert(length(a, na) == na - 1);
        assert(length(b, nb) == nb && nb > 1);
        assert(q == nullptr || nq == (na - 1) - (nb - 1));
        assert(nt == nb + 1);
        assert(a != b && a != q && a != t);
        assert(b != q && b != t && q != t);
        assert(compare(a, length(a, na), b, nb) > 0);

        for (int i = (na - 1) - nb; i >= 0; --i) {
            T _q = _divWithSingleQuotient(a + i, nb + 1, b, nb, t, nt);
            if (q != nullptr) q[i] = _q;
        }
    }

    template<typename Uint>
    static bool toUint(Uint &v, const T *a, int na, typename enable_if<!is_signed<Uint>::value, Uint>::type* =0) {
        assert(length(a, na) == na);

        v = 0;
        for (int i = na - 1; i >= 0; --i) {
            v = v * BASE + a[i];
        }

        return na <= sizeof(Uint) / sizeof(T);
    }
    template<typename Uint>
    static bool fromUint(T *a, int na, Uint v, typename enable_if<!is_signed<Uint>::value, Uint>::type* =0) {
        assert(na >= sizeof(Uint) / sizeof(T));

        for (int i = 0; i < na; ++i) {
            a[i] = v % BASE;
            v /= BASE;
        }

        return v == 0;
    }

    static bool toDouble(double &d, const T* a, int na) {
        static const int MAX_DOUBLE_BITS = (int)floor(log(numeric_limits<double>::max()) / log(2));

        d = 0;
        for (int i = na - 1; i >= 0; --i) {
            d = d * BASE + a[i];
        }

        return na * (int)sizeof(T) * 8 < MAX_DOUBLE_BITS;
    }
    static int fromDouble(T *a, int na, double d) {
        assert(d >= 0);
        int requireN = (int)ceil((log(d) / log(2)) / (sizeof(T) * 8));
        if (a == nullptr) return requireN;
        assert(na >= requireN);

        for (int i = 0; i < na; ++i) {
            a[i] = (T)fmod(d, BASE);
            d = floor(d / BASE);
        }
        assert(d == 0);

        return length(a, na);
    }

    static int fromString(T *a, int na, const char *str, int size, int base) {
        const double tPerChar = (log(base) / log(2)) / (sizeof(T) * 8);
        int requireN = (int)ceil(size * tPerChar);
        if (a == nullptr) return requireN;
        assert(na >= requireN);

        int blockSize = 1, blockBound = base;
        for (; blockBound * base < BASE; ++blockSize, blockBound *= base);

        Char2DigitTable *table = Char2DigitTable::instance();

        setZero(a, na);

        int i = 0;
        for (; i + blockSize <= size; i += blockSize) {
            T v = 0;
            for (int j = 0; j < blockSize; ++j) {
                int digit = table->toDigit(str[i + j], base);
                assert(digit != -1);
                v = v * base + digit;
            }

            if (mulSingleInplace(a, na, blockBound)) {
                assert(0);
            }
            if (addSingleInplace(a, na, v)) {
                assert(0);
            }
        }
        for (; i < size; ++i) {
            int digit = table->toDigit(str[i], base);
            assert(digit != -1);

            if (mulSingleInplace(a, na, base)) {
                assert(0);
            }
            if (addSingleInplace(a, na, digit)) {
                assert(0);
            }
        }

        return length(a, na);
    }
    static int toString(char *str, int size, T *a, int na, int base) {
        assert(length(a, na) == na);

        const double charPerT = (sizeof(T) * 8) / (log(base) / log(2));
        int requireN = (int)ceil(na * charPerT);
        if (str == nullptr) return requireN;
        assert(size >= requireN);

        int blockSize = 1, blockBound = base;
        for (; blockBound * base < BASE; ++blockSize, blockBound *= base);

        Char2DigitTable *table = Char2DigitTable::instance();

        char *p = str;
        for (; na > 1 || a[0] >= blockBound; ) {
            T v = divSingleInplace(a, na, blockBound);
            if (a[na - 1] == 0) --na;
            for (int i = 0; i < blockSize; ++i) {
                *p++ = table->toChar(v % base);
                v /= base;
            }
        }
        T v = a[0];
        for (; v >= base; v /= base) {
            *p++ = table->toChar(v % base);
        }
        *p++ = table->toChar(v);
        *p = 0;

        reverse(str, p);

        return int(p - str);
    }

    static int findHighestBitIndex(T v) {
        int i = sizeof(T) * 8 - 1;
        for (; i > 0 && (v >> i) == 0; --i);
        return i;
    }
};

class BigInteger {
private:
    //typedef uint8_t T;
    //typedef SmallVector<T, 16 / sizeof(T)> Buffer;
    //typedef ExtendedPrecisionOp<T, uint32_t, uint32_t> Ops;
    typedef uint16_t T;
    typedef SmallVector<T, 16 / sizeof(T)> Buffer;
    typedef ExtendedPrecisionOp<T, uint32_t, uint64_t> Ops;
public:
    static BigInteger ONE;
    static BigInteger NEGATIVE_ONE;
    static BigInteger ZERO;
public:
    template<typename Int>
    BigInteger(Int i, typename enable_if<is_signed<Int>::value, Int>::type* =0): BigInteger((typename make_unsigned<Int>::type)i) {
        mNegative = i < 0;
    }
    template<typename Uint>
    BigInteger(Uint i, typename enable_if<!is_signed<Uint>::value, Uint>::type* =0): mNegative(0) {
        mBuf.resize(sizeof(i));
        Ops::fromUint(mBuf.ptr(), mBuf.size(), i);
        normalize();
    }
    explicit BigInteger(double d): mNegative(d < 0 ? 1 : 0) {
        d = d < 0 ? -d : d;
        mBuf.resize(Ops::fromDouble(nullptr, 0, d));
        Ops::fromDouble(mBuf.ptr(), mBuf.size(), d);
        normalize();
    }
    BigInteger(const char *str, int base): BigInteger(0) {
        fromString(str, (int)strlen(str), base);
    }

    template<typename Int>
    Int toInt() const {
        typename make_unsigned<Int>::type v;
        if (Ops::toUint(v, mBuf.ptr(), mBuf.size())) return mNegative ? -(Int)v : (Int)v;
        else return -1;
    }
    template<typename Uint>
    Uint toUint() const {
        return (Uint)toInt<typename make_signed<Uint>::type>();
    }
    double toDouble() const {
        double d;
        if (Ops::toDouble(d, mBuf.ptr(), mBuf.size())) return mNegative ? -d : d;
        else return -1;
    }
    int fromString(const char *str, int size, int base) {
        int i = 0;
        char sign = '+';
        for (; i < size && isspace((uint8_t)str[i]); ++i);
        if (i < size && (str[i] == '-' || str[i] == '+')) sign = str[i++];
        for (; i < size && isspace((uint8_t)str[i]); ++i);

        Char2DigitTable *table = Char2DigitTable::instance();

        int begin = i;
        for (; i < size && table->toDigit(str[i], base) != -1; ++i);
        if (i - begin == 0) return 0;

        mNegative = sign == '-';
        mBuf.resize(Ops::fromString(nullptr, 0, str + begin, i - begin, base));
        Ops::fromString(mBuf.ptr(), mBuf.size(), str + begin, i - begin, base);
        normalize();

        return i;
    }
    void toString(string &s, int base) const {
        s.clear();

        Buffer tmp(mBuf);
        s.resize(Ops::toString(nullptr, 0, tmp.ptr(), tmp.size(), base));
        int n = Ops::toString(&s[0], (int)s.size(), tmp.ptr(), tmp.size(), base);
        s.resize(n);
    }
    string toString(int base = 10) const {
        string s;
        toString(s, base);
        return s;
    }

    bool isNegative() const { return mNegative == 1; }
    bool isPositive() const { return !mNegative; }
    bool isZero() const { return Ops::isZero(mBuf.ptr(), mBuf.size()); }
    const T* getBuffer() const { return mBuf.ptr(); }
    int getBufferSize() const { return mBuf.size(); }
    int getBitCount() const {
        return (mBuf.size() - 1) * sizeof(T) * 8 + Ops::findHighestBitIndex(mBuf.back()) + 1;
    }

    BigInteger square() const {
        Buffer result(mBuf.size() * 2), temp(mBuf.size() * 2 + 32 * 2);
        Ops::square(result.ptr(), result.size(), mBuf.ptr(), mBuf.size(), temp.ptr(), temp.size());
        return BigInteger(move(result), 0);
    }
    BigInteger pow(const BigInteger &a, const BigInteger *mod = nullptr) const {
        BigInteger r(1), accum(*this);
        if (mod != nullptr) accum = accum % *mod;

        for (int i = 0; i < a.mBuf.size(); ++i) {
            T v = a.mBuf[i];
            for (int j = sizeof(T) * 8; j > 0 && (i < a.mBuf.size() - 1 || v > 0); --j, v /= 2) {
                if (v & 1) {
                    r = r * accum;
                    if (mod != nullptr) r = r % *mod;
                }
                accum = accum.square();
                if (mod != nullptr) accum = accum % *mod;
            }
        }
        return r;
    }

    BigInteger& operator ++ () {
        if (mNegative) {
            if (Ops::subSingleInplace(mBuf.ptr(), mBuf.size(), 1)) assert(0);
        } else {
            if (Ops::addSingleInplace(mBuf.ptr(), mBuf.size(), 1)) mBuf.push_back(1);
        }
        normalize();
        return *this;
    }
    BigInteger operator ++ (int) {
        BigInteger old(*this);
        ++*this;
        return old;
    }
    BigInteger& operator -- () {
        if (mNegative) {
            if (Ops::addSingleInplace(mBuf.ptr(), mBuf.size(), 1)) mBuf.push_back(1);
        } else {
            if (Ops::subSingleInplace(mBuf.ptr(), mBuf.size(), 1)) assert(0);
        }
        normalize();
        return *this;
    }
    BigInteger operator -- (int) {
        BigInteger old(*this);
        --*this;
        return old;
    }

    BigInteger& operator += (const BigInteger &a) {
        if (mNegative == a.mNegative) {
            bufferAddIplace(mBuf, a.mBuf);
        } else {
            if (compareABS(a) > 0) {
                bufferSubInplace(mBuf, a.mBuf);
            } else {
                Buffer tmp(a.mBuf);
                bufferSubInplace(tmp, mBuf);
                std::swap(mBuf, tmp);
                mNegative = a.mNegative;
            }
        }
        normalize();
        return *this;
    }
    BigInteger& operator -= (const BigInteger &a) {
        if (mNegative == a.mNegative) {
            switch (compareABS(a)) {
                case 1:
                    bufferSubInplace(mBuf, a.mBuf);
                    break;
                case 0:
                    return *this = ZERO;
                case -1: {
                    Buffer tmp(a.mBuf);
                    bufferSubInplace(tmp, mBuf);
                    std::swap(tmp, mBuf);
                    mNegative = !mNegative;
                     }
                    break;
                default: assert(0); break;
            }
        } else {
            bufferAddIplace(mBuf, a.mBuf);
        }
        normalize();
        return *this;
    }
    BigInteger& operator *= (const BigInteger &a) {
        return *this = *this * a;
    }
    BigInteger& operator /= (const BigInteger &a) {
        return *this = *this / a;
    }
    BigInteger& operator %= (const BigInteger &a) {
        switch (compareABS(a)) {
            case -1:
                break;
            case 0:
                return *this = ZERO;
            case 1: {
                    if (a.mBuf.size() == 1) {
                        T r = Ops::divSingleInplace(mBuf.ptr(), mBuf.size(), a.mBuf[0]);
                        mBuf.assign(1, r);
                    } else {
                        Buffer tmp(a.mBuf.size() + 1);
                        mBuf.push_back(0);
                        Ops::div(mBuf.ptr(), mBuf.size(), a.mBuf.ptr(), a.mBuf.size(), nullptr, 0, tmp.ptr(), tmp.size());
                        bufferNormalize(mBuf);
                    }
                }
                break;
            default: assert(0); break;
        }

        if (mNegative && !Ops::isZero(mBuf.ptr(), mBuf.size())) {
            Buffer tmp(a.mBuf);
            if (Ops::subInplace(tmp.ptr(), tmp.size(), mBuf.ptr(), mBuf.size())) assert(0);
            std::swap(tmp, mBuf);
        }
        mNegative = 0;

        return *this;
    }

    friend BigInteger operator + (const BigInteger &a, const BigInteger &b) { return BigInteger(a) += b;}
    friend BigInteger operator + (BigInteger &&a, const BigInteger &b) { return move(a += b);}
    friend BigInteger operator + (const BigInteger &a, BigInteger &&b) { return move(b += a); }
    friend BigInteger operator - (const BigInteger &a, const BigInteger &b) { return BigInteger(a) -= b;}
    friend BigInteger operator - (BigInteger &&a, const BigInteger &b) { return move(a -= b); }
    friend BigInteger operator * (const BigInteger &a, const BigInteger &b) {
        Buffer result(a.mBuf.size() + b.mBuf.size());
        Buffer tmp(min(a.mBuf.size(), b.mBuf.size()) * 2);
        Ops::mul(result.ptr(), result.size(), a.mBuf.ptr(), a.mBuf.size(), b.mBuf.ptr(), b.mBuf.size(), tmp.ptr(), tmp.size());
        return BigInteger(move(result), a.mNegative != b.mNegative);
    }
    friend BigInteger operator / (const BigInteger &a, const BigInteger &b) {
        assert(!b.isZero());

        Buffer quotient, tmp;
        const Buffer *remainder = nullptr;
        switch (a.compareABS(b)) {
            case -1:
                quotient = ZERO.mBuf;
                remainder = &a.mBuf;
                break;
            case 0:
                return a.mNegative == b.mNegative ? ONE : NEGATIVE_ONE;
            case 1: {
                    if (b.mBuf.size() == 1) {
                        quotient.resize(a.mBuf.size());
                        T r = Ops::divSingle(quotient.ptr(), quotient.size(), a.mBuf.ptr(), a.mBuf.size(), b.mBuf[0]);
                        tmp.assign(1, r);
                        remainder = &tmp;
                    } else {
                        tmp = a.mBuf;
                        tmp.push_back(0);
                        remainder = &tmp;
                        quotient.resize(tmp.size() - b.mBuf.size() + 1);
                        Buffer tmp2(b.mBuf.size() + 1);
                        Ops::div(tmp.ptr(), tmp.size(), b.mBuf.ptr(), b.mBuf.size(), quotient.ptr(), quotient.size(), tmp2.ptr(), tmp2.size());
                        bufferNormalize(tmp);
                        bufferNormalize(quotient);
                    }
                }
                break;
            default: assert(0); break;
        }

        int negative;
        if (!a.mNegative) {
            negative = b.mNegative;
        } else {
            if (!Ops::isZero(remainder->ptr(), remainder->size())) {
                if (Ops::addSingleInplace(quotient.ptr(), quotient.size(), 1)) quotient.push_back(1);
            }
            negative = !b.mNegative;
        }

        return BigInteger(move(quotient), negative);
    }
    friend BigInteger operator % (const BigInteger &a, const BigInteger &b) { return BigInteger(a) %= b; }
    friend BigInteger operator % (BigInteger &&a, const BigInteger &b) { return move(a %= b); }
 
    int compare(const BigInteger &a) const {
        if (mNegative == a.mNegative) {
            int cmp = compareABS(a);
            return mNegative ? -cmp : cmp;
        } else {
            return mNegative ? -1 : 1;
        }
    }
    int compareABS(const BigInteger &a) const { return Ops::compare(mBuf.ptr(), mBuf.size(), a.mBuf.ptr(), a.mBuf.size()); }
    friend bool operator == (const BigInteger &a, const BigInteger &b) { return a.compare(b) == 0; }
    friend bool operator < (const BigInteger &a, const BigInteger &b) { return a.compare(b) < 0; }
    friend bool operator != (const BigInteger &a, const BigInteger &b) { return !(a == b);}
    friend bool operator <= (const BigInteger &a, const BigInteger &b) { return !(b < a); }
    friend bool operator > (const BigInteger &a, const BigInteger &b) { return b < a; }
    friend bool operator >= (const BigInteger &a, const BigInteger &b) { return !(a < b);}

    friend ostream& operator << (ostream &so, const BigInteger &v) {
        return so << v.toString(10);
    }
    friend istream& operator >> (istream &si, BigInteger &v) {
        string s;
        si >> s;
        int n = v.fromString(s.c_str(), (int)s.size(), 10);
        for (; (int)s.size() > n; s.pop_back()) si.putback(s.back());
        return si;
    }
private:
    BigInteger(Buffer &&buf, T negative): mBuf(forward<Buffer>(buf)), mNegative(negative) {
        normalize();
    }
    void normalize() {
        bufferNormalize(mBuf);
        if (Ops::isZero(mBuf.ptr(), mBuf.size())) mNegative = 0;
    }
    static void bufferNormalize(Buffer &a) {
        for (; a.size() > 1 && a.back() == 0;) a.pop_back();
    }
    static void bufferAddIplace(Buffer &a, const Buffer &b) {
        a.resize(max(a.size(), b.size()));
        if (Ops::addInplace(a.ptr(), a.size(), b.ptr(), b.size())) a.push_back(1);
    }
    static void bufferSubInplace(Buffer &a, const Buffer &b) {
        if (Ops::subInplace(a.ptr(), a.size(), b.ptr(), b.size())) assert(0);
    }

private:
    Buffer mBuf;
    T mNegative;
};
BigInteger BigInteger::ONE(1);
BigInteger BigInteger::NEGATIVE_ONE(-1);
BigInteger BigInteger::ZERO(0);

#endif
