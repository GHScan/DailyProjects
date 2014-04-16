#ifndef BIG_INTEGER_H
#define BIG_INTEGER_H

#include "Helper.h"

#include <stdint.h>

#include <type_traits>
#include <limits>

template<typename T, typename DoubleT, typename TripleT>
struct ExtendedPrecisionOp {
    static const int GAUSS_MULTIPLY_CUTOFF = 128 / sizeof(T);
    static const DoubleT BASE = DoubleT(1) << (sizeof(T) * 8);
    static double LOG_BASE;
    static double logBase(double f) {
        return ::log(f) / LOG_BASE;
    }

    static int length(const T *x, int nx) {
        for (; nx > 1 && x[nx - 1] == 0; --nx);
        return nx;
    }
    static int compare(const T *x, const T *y, int n) {
        int i = n - 1;
        for (; i > 0 && x[i] == y[i]; --i);
        if (x[i] == y[i]) return 0;
        else return x[i] > y[i] ? 1 : -1;
    }
    static int compare(const T *x, int nx, const T *y, int ny) {
        assert(length(x, nx) == nx && length(y, ny) == ny);
        if (nx != ny) return nx > ny ? 1 : -1;
        else return compare(x, y, nx);
    }
    static bool isZero(const T *x, int nx) {
        return nx == 1 && x[0] == 0;
    }
    static void setZero(T *x, int nx) {
        for (int i = 0; i < nx; ++i) x[i] = T();
    }
    static void add_1_to_n(T * RESTRICT x, int nx, T y, T * RESTRICT pcarry = nullptr) {
        DoubleT carry = y;
        for (int i = 0; i < nx && carry != 0; ++i) {
            carry += x[i];
            x[i] = carry % BASE;
            carry /= BASE;
        }

        assert(carry < BASE);
        if (pcarry != nullptr) {
            assert(carry + *pcarry < BASE);
            *pcarry += carry;
        } else assert(carry == 0);
    }
    static void add_m_to_n(T * RESTRICT x, int nx, const T * RESTRICT y, int ny, T * RESTRICT pcarry = nullptr) {
        assert(length(y, ny) == ny);
        assert(x != y);

        DoubleT carry = 0;
        for (int i = 0; i < ny; ++i) {
            carry += DoubleT(x[i]) + y[i];
            x[i] = carry % BASE;
            carry /= BASE;
        }

        assert(carry < BASE);
        if (carry) add_1_to_n(x + ny, nx - ny, carry, pcarry);
    }
    static void sub_1_from_n(T * RESTRICT x, int nx, T y, T * RESTRICT pborrow = nullptr) {
        DoubleT borrow = y;
        for (int i = 0; i < nx && borrow != 0; ++i) {
            if (x[i] >= borrow) {
                x[i] -= borrow;
                borrow = 0;
            } else {
                x[i] += BASE - borrow;
                borrow = 1;
            }
        }

        assert(borrow < BASE);
        if (pborrow != nullptr) {
            assert(borrow + *pborrow < BASE);
            *pborrow += borrow;
        } else assert(borrow == 0);
    }
    static void sub_m_from_n(T * RESTRICT x, int nx, const T * RESTRICT y, int ny, T * RESTRICT pborrow = nullptr) {
        assert(length(x, nx) == nx);
        assert(length(y, ny) == ny);
        assert(x != y);

        DoubleT borrow = 0;
        for (int i = 0; i < ny; ++i) {
            borrow += y[i];
            if (x[i] >= borrow) {
                x[i] -= borrow;
                borrow = 0;
            } else {
                x[i] += BASE - borrow;
                borrow = 1;
            }
        }

        assert(borrow < BASE);
        sub_1_from_n(x + ny, nx - ny, borrow, pborrow);
    }
    static void mul_1_to_n(T * r, int nr, const T * x, int nx, T y) {
        assert(length(x, nx) == nx);
        assert(nr == nx + 1);
        // assert(r != x);

        DoubleT carry = 0;
        for (int i = 0; i < nx; ++i) {
            carry += DoubleT(x[i]) * y;
            r[i] = carry % BASE;
            carry /= BASE;
        }
        assert(carry < BASE);
        r[nx] = carry;
    }
    static void mul_1_to_n_accum(T * RESTRICT r, int nr, const T * RESTRICT x, int nx, T y) {
        assert(length(x, nx) == nx);
        assert(nr > nx);
        assert(r != x);

        DoubleT carry = 0;
        for (int i = 0; i < nx; ++i) {
            carry += DoubleT(x[i]) * y + r[i];
            r[i] = carry % BASE;
            carry /= BASE;
        }

        assert(carry < BASE);
        if (carry) add_1_to_n(r + nx, nr - nx, carry);
    }
    static void mul_m_to_n(T * RESTRICT r, int nr, const T * RESTRICT x, int nx, const T * RESTRICT y, int ny) {
        assert(length(x, nx) == nx && length(y, ny) == ny);
        assert(nx >= ny);
        assert(isZero(r, length(r, nr)));
        assert(nr == nx + ny);
        assert(r != x && r != y);

        for (int i = 0; i < ny; ++i) {
            mul_1_to_n_accum(r + i, nr - i, x, nx, y[i]);
        }
    }

    static void _gauss_mul_m_to_n_directly(T * RESTRICT r, int nr, const T * RESTRICT x, int nx, const T * RESTRICT y, int ny) {
        assert(length(x, nx) == nx && length(y, ny) == ny);
        assert(nx >= ny);
        assert(nr >= nx + ny);
        assert(r != x && r != y);
        if (isZero(x, nx) || isZero(y, ny)) return;

        for (int i = 0; i < ny; ++i) {
            mul_1_to_n_accum(r + i, nr - i, x, nx, y[i]);
        }
    }
    static void _gauss_mul_m_to_m(T * RESTRICT r, int nr, const T * RESTRICT x, int nx, const T * RESTRICT y, int ny, T * RESTRICT tmp, int nt) {
        assert(nr >= nx * 2);
        assert(nt >= nx * 4);
        nx = length(x, nx);
        ny = length(y, ny);
        if (nx != ny) {
            _gauss_mul_m_to_n(r, nr, x, nx, y, ny, tmp, nt);
            return;
        }
        if (nx <= GAUSS_MULTIPLY_CUTOFF) {
            _gauss_mul_m_to_n_directly(r, nr, x, nx, y, nx);
            return;
        }

        int half = (nx + 1) / 2;
        const T* a = x + half, *b = x, *c = y + half, *d = y;
        int na = nx - half, nb = half, nc = nx - half, nd = half;

        T carry = 0, borrow = 0;
        // ac
        {
            int nac = na + nc;
            setZero(tmp, nac);
            _gauss_mul_m_to_m(tmp, nac, a, na, c, nc, tmp + nac, nt - nac);
            nac = length(tmp, nac);
            add_m_to_n(r + half, nr - half, tmp, nac, &carry);
            add_m_to_n(r + 2 * half, nr - 2 * half, tmp, nac, &carry);
        }
        // bd
        {
            int nbd = nb + nd;
            setZero(tmp, nbd);
            _gauss_mul_m_to_m(tmp, nbd, b, nb, d, nd, tmp + nbd, nt - nbd);
            nbd = length(tmp, nbd);
            add_m_to_n(r, nr, tmp, nbd, &carry);
            add_m_to_n(r + half, nr - half, tmp, nbd, &carry);
        }
        // (a-b)(d-c)
        {
            bool positive = true;
            na = length(a, na), nb = length(b, nb), nc = length(c, nc), nd = length(d, nd);
            T *ab, *dc;
            int nab, ndc;

            ab = tmp;
            if (compare(a, na, b, nb) >= 0) {
                for (nab = 0; nab < na; ++nab) ab[nab] = a[nab];
                sub_m_from_n(ab, nab, b, nb);
            } else {
                positive = !positive;
                for (nab = 0; nab < nb; ++nab) ab[nab] = b[nab];
                sub_m_from_n(ab, nab, a, na);
            }
            nab = length(ab, nab);

            dc = ab + nab;
            if (compare(d, nd, c, nc) >= 0) {
                for (ndc = 0; ndc < nd; ++ndc) dc[ndc] = d[ndc];
                sub_m_from_n(dc, ndc, c, nc);
            } else {
                positive = !positive;
                for (ndc = 0; ndc < nc; ++ndc) dc[ndc] = c[ndc];
                sub_m_from_n(dc, ndc, d, nd);
            }
            ndc = length(dc, ndc);

            if (positive) {
                _gauss_mul_m_to_n(r + half, nr - half, ab, nab, dc, ndc, dc + ndc, nt - nab - ndc);
            } else {
                T *tr = dc + ndc; 
                int ntr = nab + ndc;
                setZero(tr, ntr);
                _gauss_mul_m_to_n(tr, ntr, ab, nab, dc, ndc, tr + ntr, nt - nab - ndc - ntr);
                ntr = length(tr, ntr);
                sub_m_from_n(r + half, length(r + half, nr - half), tr, ntr, &borrow);
            }
        }
        assert(carry - borrow == 0);
    }
    static void _gauss_mul_m_to_n(T * RESTRICT r, int nr, const T * RESTRICT x, int nx, const T * RESTRICT y, int ny, T * RESTRICT tmp, int nt) {
        assert(length(x, nx) == nx && length(y, ny) == ny);
        if (nx < ny) {
            _gauss_mul_m_to_n(r, nr, y, ny, x, nx, tmp, nt);
            return;
        } else if (nx == ny) {
            _gauss_mul_m_to_m(r, nr, x, nx, y, ny, tmp, nt);
            return;
        }
        assert(nx >= ny && ny > 0);
        if (ny < 3) {
            _gauss_mul_m_to_n_directly(r, nr, x, nx, y, ny);
            return;
        }
        assert(nr >= nx + ny);
        assert(r != x && r != y);
        assert(nt >= 4 * ny);

        int i = 0;
        for (; i + ny <= nx; i += ny) {
            _gauss_mul_m_to_m(r + i, nr - i, x + i, ny, y, ny, tmp, nt);
        }
        if (i < nx) {
            _gauss_mul_m_to_n(r + i, nr - i, y, ny, x + i, nx - i, tmp, nt);
        }
    }
    static void gauss_mul_m_to_n(T * RESTRICT r, int nr, const T * RESTRICT x, int nx, const T * RESTRICT y, int ny, T * RESTRICT tmp, int nt) {
        assert(isZero(r, length(r, nr)));
        _gauss_mul_m_to_n(r, nr, x, nx, y, ny, tmp, nt);
    }
    static void _square_small(T * RESTRICT r, int nr, const T * RESTRICT x, int nx) {
        assert(length(x, nx) == nx);
        assert(nr >= 2 * nx);
        assert(r != x);

        for (int i = 0; i < nx; ++i) {
            TripleT v = x[i];
            TripleT carry = r[i + i] + v * v;
            r[i + i] = carry % BASE;
            carry /= BASE;

            v *= 2;
            for (int j = i + 1; j < nx; ++j) {
                carry += r[i + j] + x[j] * v;
                r[i + j] = carry % BASE;
                carry /= BASE;
            }
            for (int j = i + nx; j < nr && carry; ++j) {
                carry += r[j];
                r[j] = carry % BASE;
                carry /= BASE;
            }
            assert(carry < BASE);
        }
    }
    static void _square(T * RESTRICT r, int nr, const T * RESTRICT x, int nx, T * RESTRICT tmp, int nt) {
        assert(length(x, nx) == nx);
        assert(nr >= 2 * nx);
        assert(nt >= 3 * nx);
        assert(r != x && r != tmp);

        if (nx <= GAUSS_MULTIPLY_CUTOFF) {
            // _square_small(r, nr, x, nx);
            _gauss_mul_m_to_n_directly(r, nr, x, nx, x, nx);
            return;
        }

        int half = (nx + 1) / 2;
        const T* a = x + half, *b = x;
        int na = nx - half, nb = half;
        na = length(a, na);
        nb = length(b, nb);

        _square(r, nr, b, nb, tmp, nt);
        _square(r + 2 * half, nr - 2 * half, a, na, tmp, nt);

        T *ab = tmp;
        int nab = na + nb;
        setZero(ab, nab);
        _gauss_mul_m_to_n(ab, nab, a, na, b, nb, ab + nab, nt - nab);
        nab = length(ab, nab);
        mul_1_to_n(ab, nab + 1, ab, nab, 2);
        nab = length(ab, nab + 1);
        add_m_to_n(r + half, nr - half, ab, nab);
    }
    static void square(T * RESTRICT r, int nr, const T * RESTRICT x, int nx, T * RESTRICT tmp, int nt) {
        assert(isZero(r, length(r, nr)));
        assert(length(x, nx) == nx);
        assert(nr == nx * 2);
        assert(nt >= nx * 3);
        assert(r != x && r != tmp);

        _square(r, nr, x, nx, tmp, nt);
    }
    static T div_1_from_n(T * RESTRICT x, int nx, T y) {
        assert(length(x, nx) == nx);
        assert(compare(x, nx, &y, 1) >= 0);

        DoubleT remain = 0;
        for (int i = nx - 1; i >= 0; --i) {
            remain = remain * BASE + x[i];
            x[i] = remain / y;
            remain %= y;
        }
        return remain;
    }
    static T div_n_from_nplus1(T * RESTRICT x, int nx, const T * RESTRICT y, int ny, T * RESTRICT temp, int nt) {
        assert(length(y, ny) == ny);
        assert(nx == ny + 1);
        assert(nx == nt);
        assert(ny >= 2);
        assert(x != y && x != temp && y != temp);

        TripleT _a(x[nx - 1]);
        _a = _a * BASE + x[nx - 2];
        _a = _a * BASE + x[nx - 3];
        TripleT _b(y[ny - 1]);
        _b = _b * BASE + y[ny - 2];

        assert(_a / _b < BASE);
        T q = T(_a / _b);
        mul_1_to_n(temp, nt, y, ny, q);
        if (compare(x, temp, nx) < 0) {
            --q;
            mul_1_to_n(temp, nt, y, ny, q);
        }
        sub_m_from_n(x, length(x, nx), temp, length(temp, nt));

        return q;
    }
    static void div_m_from_n(T * RESTRICT x, int nx, const T * RESTRICT y, int ny, T * RESTRICT q, int nq, T * RESTRICT temp, int nt) {
        assert(length(y, ny) == ny);
        assert(length(x, nx) + 1 == nx);
        assert(compare(x, nx - 1, y, ny) >= 0);
        assert(q == nullptr || nx - 1 - (ny - 1) == nq);
        assert(nt == ny + 1);
        assert(ny >= 2);
        assert(x != y && x != q && x != temp);
        assert(y != q && y != temp && q != temp);

        for (int i = nx - ny - 1; i >= 0; --i) {
            T k = div_n_from_nplus1(x + i, ny + 1, y, ny, temp, nt);
            if (q != nullptr) q[i] = k;
        }
    }
    template<typename UintT>
    static void convertFromUint(T *x, int nx, UintT v, typename enable_if<!is_signed<UintT>::value, void>::type* =0) {
        assert(nx * sizeof(T) >= sizeof(UintT));
        int i = 0;
        for (; i < nx && v > 0; ++i) {
            x[i] = v % BASE;
            v /= BASE;
        }
        for (; i < nx; ++i) x[i] = 0;
        assert(v == 0);
    }
    template<typename UintT>
    static UintT convertToUint(const T* x, int nx, typename enable_if<!is_signed<UintT>::value, void>::type* =0) {
        assert(nx * sizeof(T) <= sizeof(UintT));
        assert(length(x, nx) == nx);
        UintT v = 0;
        for (int i = nx - 1; i >= 0; --i) {
            v = v * BASE + x[i];
        }
        return v;
    }
    static void convertFromDouble(T *x, int nx, double f) {
        assert(f >= 0);
        assert(nx >= (int)ceil(logBase(f)));
        double fbase = BASE;
        int i = 0;
        for (; i < nx && f > 0; ++i) {
            x[i] = (T)fmod(f, fbase);
            f = floor(f / fbase);
        }
        for (; i < nx; ++i) x[i] = 0;
        assert(f == 0);
    }
    static double convertToDouble(const T *x, int nx) {
        assert(length(x, nx) == nx);
        assert(nx <= logBase(numeric_limits<double>::max()));
        double r = 0, fbase = BASE;
        for (int i = nx - 1; i >= 0; --i) {
            r = r * fbase + x[i];
        }
        return r;
    }
};
template<typename T, typename DoubleT, typename TripleT>
double ExtendedPrecisionOp<T, DoubleT, TripleT>::LOG_BASE = ::log(BASE);

class BigInteger {
private:
    //typedef uint8_t T;
    //typedef ExtendedPrecisionOp<T, uint32_t, uint32_t> Ops;
    //typedef SmallVector<T, 16 / sizeof(T)> BufferT;
    typedef uint16_t T;
    typedef ExtendedPrecisionOp<T, uint32_t, uint64_t> Ops;
    typedef SmallVector<T, 16 / sizeof(T)> BufferT;

public:
    template<typename IntT>
    BigInteger(IntT i, typename enable_if<is_signed<IntT>::value, void>::type* =0): BigInteger((typename make_unsigned<IntT>::type)(i > 0 ? i : -i)) {
        mNegative = i < 0;
    }
    template<typename UintT>
    BigInteger(UintT i, typename enable_if<!is_signed<UintT>::value, void>::type* =0): BigInteger() {
        mBuf.resize(sizeof(UintT) / sizeof(T));
        Ops::convertFromUint(mBuf.ptr(), mBuf.size(), i);
        mBuf.resize(Ops::length(mBuf.ptr(), mBuf.size()));
    }
    explicit BigInteger(double d): BigInteger() {
        if (d < 0) {
            mNegative = 1;
            d = -d;
        }

        mBuf.resize((int)ceil(Ops::logBase(d))); 
        Ops::convertFromDouble(mBuf.ptr(), mBuf.size(), d);
        mBuf.resize(Ops::length(mBuf.ptr(), mBuf.size()));
    }
    explicit BigInteger(const char *str, int base, const char **end = nullptr): BigInteger() {
        assert(str != nullptr);
        assert(base >= 2 && base <= 36);

        Char2DigitTable *table = Char2DigitTable::instance();

        char sign = '+';
        const char *p = str;
        while (isspace((uint8_t)*p)) ++p;
        if (*p == '+' || *p == '-') sign = *p++;
        while (isspace((uint8_t)*p)) ++p;
        const char *digits = p;
        for (; *p && table->toDigit(*p, base) != -1; ++p);
        if (p == digits) {
            if (end != nullptr) *end = nullptr;
            return;
        }
        if (end != nullptr) *end = p;

        mBuf.reserve((int)ceil((p - digits) * Ops::logBase(base)) + 1);
        mBuf.push_back(0);
        mBuf.push_back(0);
        for (; digits != p; ++digits) {
            int digit = table->toDigit(*digits, base);
            Ops::mul_1_to_n(mBuf.ptr(), mBuf.size(), mBuf.ptr(), mBuf.size() - 1, base);
            if (mBuf.back() != 0) mBuf.push_back(0);
            Ops::add_1_to_n(mBuf.ptr(), mBuf.size(), digit);
            if (mBuf.back() != 0) mBuf.push_back(0);
        }
        assert(mBuf.back() == 0 && mBuf[mBuf.size() - 2] != 0);
        mBuf.pop_back();

        mNegative = sign == '-' && !isZero();
    }
    ~BigInteger() {
    }

    template<typename IntT>
    typename enable_if<is_signed<IntT>::value, IntT>::type toInt() const {
        auto v = (IntT)toInt<typename make_unsigned<IntT>::type>();
        return mNegative ? -v : v;
    }
    template<typename UintT>
    typename enable_if<!is_signed<UintT>::value, UintT>::type toInt() const {
        return Ops::convertToUint<UintT>(mBuf.ptr(), mBuf.size());
    }
    double toDouble() const {
        double d = Ops::convertToDouble(mBuf.ptr(), mBuf.size());
        return mNegative ? -d : d;
    }
    string& toString(string& s, int base) const {
        assert(base >= 0 && base < 36);

        s.clear();

        Char2DigitTable *table = Char2DigitTable::instance();

        int groupSize = 1 , groupValue = base;
        for (; groupValue * base < (int)Ops::BASE; groupValue *= base, ++groupSize);

        BufferT tempBuf(mBuf);
        for (; tempBuf.size() > 1 || tempBuf.back() >= groupValue; ) {
            T remain = Ops::div_1_from_n(tempBuf.ptr(), tempBuf.size(), groupValue);
            if (tempBuf.back() == 0) tempBuf.pop_back();
            for (int i = 0; i < groupSize; ++i, remain /= base) {
                s += table->toChar(remain % base);
            }
        }
        assert(tempBuf.size() == 1);

        T v = tempBuf.back();
        for (; v >= base; v /= base) {
            s += table->toChar(v % base);
        }
        s += table->toChar(v);

        reverse(s.begin(), s.end());
        if (mNegative) s.insert(s.begin(), '-');
        
        return s;
    }
    string toString(int base = 10) const {
        string s;
        toString(s, base);
        return s;
    }

    int log2Ceil() const {
        for (int i = 0; i < mBuf.size() - 1; ++i) {
            if (mBuf[i]) return getBitCount();
        }
        T top = mBuf.back();
        return getBitCount() + (((top - 1) & top) ? 0 : -1);
    }
    int getBitCount() const {
        int i = sizeof(T) * 8 - 1;
        for (T top = mBuf.back(); !(top >> i); --i);
        return i + 1 + (mBuf.size() - 1) * sizeof(T) * 8;
    }
    int getBufferSize() const { return mBuf.size(); }
    const T* getBuffer() const { return mBuf.ptr(); }

    BigInteger(const BigInteger &o): BigInteger() {
        *this = o;
    }
    BigInteger& operator = (const BigInteger &o) {
        if (this != &o) {
            mNegative = o.mNegative;
            mBuf = o.mBuf;
        }
        return *this;
    }
    BigInteger(BigInteger &&o): BigInteger() {
        *this = move(o);
    }
    BigInteger& operator = (BigInteger &&o) {
        if (this != &o) {
            mNegative = o.mNegative;
            mBuf = move(o.mBuf);
        }
        return *this;
    }

    BigInteger& operator += (const BigInteger &o) {
        if (this == &o) {
            return *this *= 2;
        }

        if (mNegative == o.mNegative) {
            bufferAdd(mBuf, o.mBuf);
        } else {
            if (compareABS(o) >= 0) {
                bufferSub(mBuf, o.mBuf);
            } else {
                BufferT tmpBuf(o.mBuf);
                bufferSub(tmpBuf, mBuf);
                std::swap(tmpBuf, mBuf);
                mNegative = o.mNegative;
            }
        }

        normlizeZero();
        return *this;
    }
    BigInteger& operator -= (const BigInteger &o) {
        if (this == &o) {
            return *this = ZERO;
        }

        if (mNegative != o.mNegative) {
            bufferAdd(mBuf, o.mBuf);
        } else {
            if (compareABS(o) >= 0) {
                bufferSub(mBuf, o.mBuf);
                mNegative = 0;
            } else {
                BufferT tmpBuf(o.mBuf);
                bufferSub(tmpBuf, mBuf);
                std::swap(tmpBuf, mBuf);
                mNegative = 1;
            }
        }

        normlizeZero();
        return *this;
    }
    BigInteger& operator *= (const BigInteger &o) {
        if (this == &o) {
            return *this = square();
        }

        // O(N^2)
            //BufferT tempBuf(mBuf.size() + o.mBuf.size());
            //if (mBuf.size() >= o.mBuf.size()) {
            //    Ops::mul_m_to_n(tempBuf.ptr(), tempBuf.size(), mBuf.ptr(), mBuf.size(), o.mBuf.ptr(), o.mBuf.size());
            //} else {
            //    Ops::mul_m_to_n(tempBuf.ptr(), tempBuf.size(), o.mBuf.ptr(), o.mBuf.size(), mBuf.ptr(), mBuf.size());
            //}
            //tempBuf.resize(Ops::length(tempBuf.ptr(), tempBuf.size()));
            //std::swap(mBuf, tempBuf);

        // O(N^1.59)
        BufferT result(mBuf.size() + o.mBuf.size());
        BufferT tmp(min(mBuf.size(), o.mBuf.size()) * 4 + 32);
        Ops::gauss_mul_m_to_n(result.ptr(), result.size(), o.mBuf.ptr(), o.mBuf.size(), mBuf.ptr(), mBuf.size(), tmp.ptr(), tmp.size());
        result.resize(Ops::length(result.ptr(), result.size()));
        std::swap(mBuf, result);

        mNegative = mNegative != o.mNegative;

        normlizeZero();
        return *this;
    }
    BigInteger& operator /= (const BigInteger &o) {
        if (this == &o) {
            return *this = ONE;
        }
        assert(!o.isZero());

        if (compareABS(o) < 0) {
            mBuf = ZERO.mBuf;
        } else {
            if (o.mBuf.size() == 1) {
                Ops::div_1_from_n(mBuf.ptr(), mBuf.size(), o.mBuf[0]);
            } else {
                BufferT qbuf(mBuf.size() - o.mBuf.size() + 1), tmpBuf(o.mBuf.size() + 1);
                mBuf.push_back(0);
                Ops::div_m_from_n(mBuf.ptr(), mBuf.size(), o.mBuf.ptr(), o.mBuf.size(), qbuf.ptr(), qbuf.size(), tmpBuf.ptr(), tmpBuf.size());
                qbuf.resize(Ops::length(qbuf.ptr(), qbuf.size()));
                std::swap(mBuf, qbuf);
            }
        }

        if (!mNegative) {
            mNegative = o.mNegative;
        } else {
            mBuf.resize(mBuf.size() + 1);
            Ops::add_1_to_n(mBuf.ptr(), mBuf.size(), 1);
            mBuf.resize(Ops::length(mBuf.ptr(), mBuf.size()));
            mNegative = !o.mNegative;
        }

        normlizeZero();
        return *this;
    }
    BigInteger& operator %= (const BigInteger &o) {
        if (this == &o) {
            return *this = ZERO;
        }
        assert(!o.isZero());

        if (compareABS(o) < 0) {
        } else {
            if (o.mBuf.size() == 1) {
                T r = Ops::div_1_from_n(mBuf.ptr(), mBuf.size(), o.mBuf[0]);
                mBuf.resize(0);
                mBuf.push_back(r);
            } else {
                BufferT tmpBuf(o.mBuf.size() + 1);
                mBuf.push_back(0);
                Ops::div_m_from_n(mBuf.ptr(), mBuf.size(), o.mBuf.ptr(), o.mBuf.size(), nullptr, 0, tmpBuf.ptr(), tmpBuf.size());
                mBuf.resize(Ops::length(mBuf.ptr(), mBuf.size()));
            }
        }

        if (mNegative) {
            mNegative = 0;
            BufferT tmpBuf(o.mBuf);
            bufferSub(tmpBuf, mBuf);
            swap(mBuf, tmpBuf);
        }

        return *this;
    }
    BigInteger pow(const BigInteger &o, const BigInteger *mod = nullptr) const {
        BigInteger r = ONE;
        BigInteger accum = *this;
        if (mod != nullptr) accum %= *mod;
        for (int i = 0; i < o.mBuf.size(); ++i) {
            T v = o.mBuf[i];
            for (int j = 0; j < int(sizeof(T)) * 8 && (v > 0 || i < o.mBuf.size() - 1); ++j, v /= 2) {
                if (v & 1) {
                    r *= accum;
                    if (mod != nullptr) r %= *mod;
                }
                accum *= accum;
                if (mod != nullptr) accum %= *mod;
            }
        }

        return r;
    }
    BigInteger square() const {
        BufferT result(mBuf.size() * 2), tmpBuf(mBuf.size() * 3 + 32);
        Ops::square(result.ptr(), result.size(), mBuf.ptr(), mBuf.size(), tmpBuf.ptr(), tmpBuf.size());
        result.resize(Ops::length(result.ptr(), result.size()));

        BigInteger r;
        std::swap(r.mBuf, result);
        return r;
    }

    BigInteger& operator ++ () {
        *this += 1;
        return *this;
    }
    BigInteger operator ++ (int) {
        BigInteger old(*this);
        ++*this;
        return old;
    }
    BigInteger& operator -- () {
        *this -= 1;
        return *this;
    }
    BigInteger operator -- (int) {
        BigInteger old(*this);
        --*this;
        return old;
    }

    int compareABS(const BigInteger& o) const {
        return Ops::compare(mBuf.ptr(), mBuf.size(), o.mBuf.ptr(), o.mBuf.size());
    }
    bool isZero() const { return Ops::isZero(mBuf.ptr(), mBuf.size());}

    static BigInteger ONE;
    static BigInteger ZERO;
private:
    BigInteger(): mNegative(0) {
    }
    static void bufferAdd(BufferT &l, const BufferT &r) {
        l.resize(max(l.size(), r.size()) + 1);
        Ops::add_m_to_n(l.ptr(), l.size(), r.ptr(), r.size());
        l.resize(Ops::length(l.ptr(), l.size()));
    }
    static void bufferSub(BufferT &l, const BufferT &r) {
        Ops::sub_m_from_n(l.ptr(), l.size(), r.ptr(), r.size());
        l.resize(Ops::length(l.ptr(), l.size()));
    }
    bool equal(const BigInteger &o) const {
        return mNegative == o.mNegative && compareABS(o) == 0;
    }
    bool less(const BigInteger &o) const {
        if (mNegative == o.mNegative) {
            if (mNegative) return compareABS(o) > 0;
            else return compareABS(o) < 0;
        } else {
            return mNegative == 1;
        }
    }
    void normlizeZero() {
        if (isZero()) mNegative = 0;
    }
    friend bool operator == (const BigInteger &l, const BigInteger &r);
    friend bool operator < (const BigInteger &l, const BigInteger &r);
private:
    BufferT mBuf;
    T mNegative;
};
static_assert(sizeof(BigInteger) == 32, "");
BigInteger BigInteger::ONE(1);
BigInteger BigInteger::ZERO(0);

inline BigInteger operator + (const BigInteger &l, const BigInteger &r) { return BigInteger(l) += r; }
inline BigInteger operator + (BigInteger &&l, const BigInteger &r) { return move(l += r); }
inline BigInteger operator + (const BigInteger &l, BigInteger &&r) { return move(r += l); }
inline BigInteger operator - (const BigInteger &l, const BigInteger &r) { return BigInteger(l) -= r; }
inline BigInteger operator - (BigInteger &&l, const BigInteger &r) { return move(l -= r); }
inline BigInteger operator * (const BigInteger &l, const BigInteger &r) { return BigInteger(l) *= r; }
inline BigInteger operator * (BigInteger &&l, const BigInteger &r) { return move(l *= r); }
inline BigInteger operator * (const BigInteger &l, BigInteger &&r) { return move(r *= l); }
inline BigInteger operator * (BigInteger &&l, BigInteger &&r) { return move(r *= l); }
inline BigInteger operator / (const BigInteger &l, const BigInteger &r) { return BigInteger(l) /= r; }
inline BigInteger operator / (BigInteger &&l, const BigInteger &r) { return move(l /= r); }
inline BigInteger operator % (const BigInteger &l, const BigInteger &r) { return BigInteger(l) %= r; }
inline BigInteger operator % (BigInteger &&l, const BigInteger &r) { return move(l %= r); }

inline bool operator == (const BigInteger &l, const BigInteger &r) { return l.equal(r); }
inline bool operator != (const BigInteger &l, const BigInteger &r) { return !(l == r); }
inline bool operator < (const BigInteger &l, const BigInteger &r) { return l.less(r); }
inline bool operator > (const BigInteger &l, const BigInteger &r) { return r < l; }
inline bool operator <= (const BigInteger &l, const BigInteger &r) { return !(r < l); }
inline bool operator >= (const BigInteger &l, const BigInteger &r) { return !(l < r); }

inline ostream& operator << (ostream &so, const BigInteger &i) {
    so << i.toString();
    return so;
}
inline istream& operator >> (istream &si, BigInteger &i) {
    string s;
    si >> s;
    i = BigInteger(s.c_str(), 10);
    return si;
}

#endif
