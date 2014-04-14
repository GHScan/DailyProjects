#include "pch.h"

#include <assert.h>
#include <stdint.h>

#include <type_traits>
#include <limits>

#define RESTRICT __restrict__

class Char2DigitTable {
public:
    static Char2DigitTable* instance() {
        static Char2DigitTable s_ins;
        return &s_ins;
    }
    int toDigit(unsigned char c, int base) const {
        int r = mTable[c];
        return r >= 0 && r < base ? r : -1;
    }
    static char toChar(int i) {
        assert(i >= 0 && i < 36);
        return i < 10 ? i + '0' : i - 10 + 'a';
    }
private:
    Char2DigitTable() {
        memset(mTable, -1, sizeof(mTable));
        for (int i = 0; i < 10; ++i) mTable[i + '0'] = i;
        for (int i = 0; i < 26; ++i) mTable[i + 'A'] = mTable[i + 'a'] = i + 10;
    }
    Char2DigitTable(const Char2DigitTable&) = delete;
    Char2DigitTable& operator = (const Char2DigitTable&) = delete;
private:
    char mTable[256];
};

template<typename T, int initN>
class SmallVector {
    static_assert(is_pod<T>::value, "");
public:
    SmallVector(): mBuf(mStaticBuffer), mCapacity(initN), mSize(0) { 
    }
    explicit SmallVector(int size, const T &init = T()): SmallVector() {
        resize(size, init);
    }
    ~SmallVector() { 
        clear();
    }
    SmallVector(const SmallVector& o): SmallVector() {
        *this = o;
    }
    SmallVector& operator = (const SmallVector& o) {
        if (this != &o) {
            resize(o.mSize);
            memcpy(mBuf, o.mBuf, o.mSize * sizeof(T));
        }
        return *this;
    }
    SmallVector(SmallVector&& o): SmallVector() {
        *this = move(o);
    }
    SmallVector& operator = (SmallVector&& o) {
        if (this != &o) {
            if (o.isDynamicBuffer() && isDynamicBuffer() && mCapacity < o.mCapacity) {
                clear();
            }

            if (o.isDynamicBuffer() && !isDynamicBuffer()) {
                mCapacity = o.mCapacity;
                mBuf = o.mBuf;
                mSize = o.mSize;
                o.mCapacity = initN;
                o.mBuf = o.mStaticBuffer;
                o.mSize = 0;
            } else {
                // call operator=(const SmallVector&)
                *this = o;
            }
        }
        return *this;
    }

    T& operator [] (int i) { assert(i >= 0 && i < mSize); return mBuf[i]; }
    const T& operator [] (int i) const { assert(i >= 0 && i < mSize); return mBuf[i]; }
    const T& back() const { assert(mSize > 0); return mBuf[mSize - 1]; }
    T& back() { assert(mSize > 0); return mBuf[mSize - 1]; }
    const T& front() const { assert(mSize > 0); return mBuf[0]; }
    T& front() { assert(mSize > 0); return mBuf[0]; }
    void push_back(const T &v) { reserve(mSize + 1); mBuf[mSize++] = v; }
    void pop_back() { assert(mSize > 0); --mSize; }
    T* ptr() { return mBuf; }
    const T* ptr() const { return mBuf; }
    int size() const { return mSize; }
    bool isDynamicBuffer() const { return mBuf != mStaticBuffer;}
    void resize(int size, const T &init = T()) {
        if (size > mSize) {
            reserve(size);
            for (int i = mSize; i < size; ++i) mBuf[i] = init;
        }
        mSize = size;
    }
    void reserve(int capacity) {
        if (capacity <= mCapacity) return;
        if (isDynamicBuffer()) {
            mBuf = (T*)realloc(mBuf, capacity * sizeof(T));
        } else {
            mBuf = (T*)malloc(capacity * sizeof(T));
            memcpy(mBuf, mStaticBuffer, sizeof(mStaticBuffer));
        }
        mCapacity = capacity;
    }
    void clear() {
        mSize = 0;
        if (isDynamicBuffer()) {
            ::free(mBuf);
            mBuf = mStaticBuffer;
            mCapacity = initN;
        }
    }
private:
    T *mBuf;
    int mCapacity;
    int mSize;
    T mStaticBuffer[initN];
};

template<typename T, typename DoubleT, typename TripleT>
struct ExtendedPreciseOp {
    static const DoubleT BASE = 1 << (sizeof(T) * 8);
    static double LOG_BASE;
    static double logBase(double f) {
        return ::log(f) / LOG_BASE;
    }

    static int length(const T *a, int na) {
        for (; na > 1 && a[na - 1] == 0; --na);
        return na;
    }
    static int compare(const T *a, const T *b, int n) {
        int i = n - 1;
        for (; i > 0 && a[i] == b[i]; --i);
        if (a[i] == b[i]) return 0;
        else return a[i] > b[i] ? 1 : -1;
    }
    static int compare(const T *a, int na, const T *b, int nb) {
        assert(length(a, na) == na && length(b, nb) == nb);
        if (na != nb) return na > nb ? 1 : -1;
        else return compare(a, b, na);
    }
    static bool isZero(const T *a, int na) {
        return na == 1 && a[0] == 0;
    }
    static void add_1_to_n(T * RESTRICT a, int na, T b) {
        DoubleT carry = b;
        for (int i = 0; i < na && carry != 0; ++i) {
            carry += a[i];
            a[i] = carry % BASE;
            carry /= BASE;
        }
        assert(carry == 0);
    }
    static void add_m_to_n(T * RESTRICT a, int na, const T * RESTRICT b, int nb) {
        assert(length(b, nb) == nb);
        assert(max(length(a, na), nb) + 1 == na);
        assert(a != b);

        DoubleT carry = 0;
        for (int i = 0; i < nb; ++i) {
            carry += DoubleT(a[i]) + b[i];
            a[i] = carry % BASE;
            carry /= BASE;
        }

        assert(carry < BASE);
        add_1_to_n(a + nb, na - nb, carry);
    }
    static void sub_1_from_n(T * RESTRICT a, int na, T b) {
        assert(na > 1 || a[0] >= b);
        DoubleT borrow = b;
        for (int i = 0; i < na && borrow != 0; ++i) {
            if (a[i] >= borrow) {
                a[i] -= borrow;
                borrow = 0;
            } else {
                a[i] += BASE - borrow;
                borrow = 1;
            }
        }
        assert(borrow == 0);
    }
    static void sub_m_from_n(T * RESTRICT a, int na, const T * RESTRICT b, int nb) {
        assert(length(a, na) == na);
        assert(length(b, nb) == nb);
        assert(compare(a, na, b, nb) >= 0);
        assert(a != b);

        DoubleT borrow = 0;
        for (int i = 0; i < nb; ++i) {
            borrow += b[i];
            if (a[i] >= borrow) {
                a[i] -= borrow;
                borrow = 0;
            } else {
                a[i] += BASE - borrow;
                borrow = 1;
            }
        }

        assert(borrow < BASE);
        sub_1_from_n(a + nb, na - nb, borrow);
    }
    static void mul_1_to_n(T * r, int nr, const T * a, int na, T b) {
        assert(length(a, na) == na);
        assert(nr == na + 1);
        // assert(r != a);

        DoubleT carry = 0;
        for (int i = 0; i < na; ++i) {
            carry += DoubleT(a[i]) * b;
            r[i] = carry % BASE;
            carry /= BASE;
        }
        assert(carry < BASE);
        r[na] = carry;
    }
    static void mul_1_to_n_accum(T * RESTRICT r, int nr, const T * RESTRICT a, int na, T b) {
        assert(length(a, na) == na);
        assert(nr > na);
        assert(r != a);

        DoubleT carry = 0;
        for (int i = 0; i < na; ++i) {
            carry += DoubleT(a[i]) * b + r[i];
            r[i] = carry % BASE;
            carry /= BASE;
        }

        assert(carry < BASE);
        add_1_to_n(r + na, nr - na, carry);
    }
    static void mul_m_to_n(T * RESTRICT r, int nr, const T * RESTRICT a, int na, const T * RESTRICT b, int nb) {
        assert(length(a, na) == na);
        assert(length(b, nb) == nb);
        assert(na >= nb);
        assert(nr == na + nb);
        assert(isZero(r, length(r, nr)));
        assert(r != a && r != b);

        for (int i = 0; i < nb; ++i) {
            mul_1_to_n_accum(r + i, nr - i, a, na, b[i]);
        }
    }
    static T div_1_from_n(T * RESTRICT a, int na, T b) {
        assert(length(a, na) == na);
        assert(compare(a, na, &b, 1) >= 0);

        DoubleT remain = 0;
        for (int i = na - 1; i >= 0; --i) {
            remain = remain * BASE + a[i];
            a[i] = remain / b;
            remain %= b;
        }
        return remain;
    }
    static T div_n_from_nplus1(T * RESTRICT a, int na, const T * RESTRICT b, int nb, T * RESTRICT temp, int nt) {
        assert(length(b, nb) == nb);
        assert(na == nb + 1);
        assert(na == nt);
        assert(nb >= 2);
        assert(a != b && a != temp && b != temp);

        TripleT _a(a[na - 1]);
        _a = _a * BASE + a[na - 2];
        _a = _a * BASE + a[na - 3];
        TripleT _b(b[nb - 1]);
        _b = _b * BASE + b[nb - 2];

        assert(_a / _b < BASE);
        T q = _a / _b;
        mul_1_to_n(temp, nt, b, nb, q);
        if (compare(a, temp, na) < 0) {
            --q;
            mul_1_to_n(temp, nt, b, nb, q);
        }
        sub_m_from_n(a, na, temp, nt);

        return q;
    }
    static void div_m_from_n(T * RESTRICT a, int na, const T * RESTRICT b, int nb, T * RESTRICT q, int nq, T * RESTRICT temp, int nt) {
        assert(length(b, nb) == nb);
        assert(length(a, na) + 1 == na);
        assert(compare(a, na - 1, b, nb) >= 0);
        assert(q == nullptr || na - 1 - (nb - 1) == nq);
        assert(nt == nb + 1);
        assert(nb >= 2);
        assert(a != b && a != q && a != temp);
        assert(b != q && b != temp && q != temp);

        for (int i = na - nb - 1; i >= 0; --i) {
            T k = div_n_from_nplus1(a + i, nb + 1, b, nb, temp, nt);
            if (q != nullptr) q[i] = k;
        }
    }
    template<typename UintT>
    static void convertFromUint(T *a, int na, UintT v, typename enable_if<!is_signed<UintT>::value, void>::type* =0) {
        assert(sizeof(na * sizeof(T) >= sizeof(UintT)));
        int i = 0;
        for (; i < na && v > 0; ++i) {
            a[i] = v % BASE;
            v /= BASE;
        }
        for (; i < na; ++i) a[i] = 0;
        assert(v == 0);
    }
    template<typename UintT>
    static UintT convertToUint(const T* a, int na, typename enable_if<!is_signed<UintT>::value, void>::type* =0) {
        assert(sizeof(na * sizeof(T) <= sizeof(UintT)));
        assert(length(a, na) == na);
        UintT v = 0;
        for (int i = na - 1; i >= 0; --i) {
            v = v * BASE + a[i];
        }
        return v;
    }
    static void convertFromDouble(T *a, int na, double f) {
        assert(f >= 0);
        assert(na >= (int)ceil(logBase(f)));
        double fbase = BASE;
        int i = 0;
        for (; i < na && f > 0; ++i) {
            a[i] = (T)fmod(f, fbase);
            f = floor(f / fbase);
        }
        for (; i < na; ++i) a[i] = 0;
        assert(f == 0);
    }
    static double convertToDouble(const T *a, int na) {
        assert(length(a, na) == na);
        assert(na <= logBase(numeric_limits<double>::max()));
        double r = 0, fbase = BASE;
        for (int i = na - 1; i >= 0; --i) {
            r = r * fbase + a[i];
        }
        return r;
    }
};
template<typename T, typename DoubleT, typename TripleT>
double ExtendedPreciseOp<T, DoubleT, TripleT>::LOG_BASE = ::log(BASE);

class BigInteger {
private:
    typedef uint8_t T;
    typedef ExtendedPreciseOp<T, uint32_t, uint32_t> Ops;
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
        while (isspace(*p)) ++p;
        if (*p == '+' || *p == '-') sign = *p++;
        while (isspace(*p)) ++p;
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
        s.clear();

        Char2DigitTable *table = Char2DigitTable::instance();

        BufferT tempBuf(mBuf);
        for (; tempBuf.size() > 1 || tempBuf.back() >= base; ) {
            T remain = Ops::div_1_from_n(tempBuf.ptr(), tempBuf.size(), base);
            s += table->toChar(remain);
            if (tempBuf.back() == 0) tempBuf.pop_back();
        }
        assert(tempBuf.size() == 1);

        s += table->toChar(tempBuf.back());
        reverse(s.begin(), s.end());
        if (mNegative) s.insert(s.begin(), '-');
        
        return s;
    }

    int log2Ceil() const {
        for (int i = 0; i < mBuf.size() - 1; ++i) {
            if (mBuf[i]) return log2Floor() + 1;
        }
        T top = mBuf.back();
        return log2Floor() + (((top - 1) & top) ? 1 : 0);
    }
    int log2Floor() const {
        int i = sizeof(T) * 8 - 1;
        for (T top = mBuf.back(); !(top >> i); --i);
        return i + (mBuf.size() - 1) * sizeof(T) * 8;
    }
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
            // Ok
        }

        BufferT tempBuf(mBuf.size() + o.mBuf.size());
        if (mBuf.size() >= o.mBuf.size()) {
            Ops::mul_m_to_n(tempBuf.ptr(), tempBuf.size(), mBuf.ptr(), mBuf.size(), o.mBuf.ptr(), o.mBuf.size());
        } else {
            Ops::mul_m_to_n(tempBuf.ptr(), tempBuf.size(), o.mBuf.ptr(), o.mBuf.size(), mBuf.ptr(), mBuf.size());
        }
        tempBuf.resize(Ops::length(tempBuf.ptr(), tempBuf.size()));
        std::swap(mBuf, tempBuf);

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
            for (int i = 0; i < int(sizeof(T)) * 8; ++i, v /= 2) {
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
            return mNegative;
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
    string s;
    i.toString(s, 10);
    so << s;
    return so;
}
inline istream& operator >> (istream &si, BigInteger &i) {
    string s;
    si >> s;
    i = BigInteger(s.c_str(), 10);
    return si;
}

int main() {
    for (; cin;) {
        BigInteger base(0), p(0);
        cin >> base >> p;
        cout << base.pow(p) << endl;
        break;
    }
}
