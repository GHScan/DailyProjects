#include "pch.h"

#include "BigInteger.h"

#include <integer.h>

static BigIntegerImpl* doubleToBigInteger(double n) {
    bool sign = n < 0;
    n = fabs(n);

    int BASE = 65536;

    BigIntegerImpl *r = new BigIntegerImpl((int)fmod(n, BASE));
    n = floor(n / BASE);
    for (BigIntegerImpl tmp = BASE; n > 0; tmp *= BASE, n = floor(n / BASE)) {
        *r += tmp * (int)fmod(n, BASE); 
    }

    if (sign) r->SetNegative();
    return r;
}

static double bigIntegerToDouble(BigIntegerImpl *n) {
    int BASE_BIT_COUNT = 16;
    int BASE = 1 << BASE_BIT_COUNT;

    double d = 0;

    for (int i = (n->BitCount() + BASE_BIT_COUNT - 1) / BASE_BIT_COUNT - 1; i >= 0; --i) {
        d = d * BASE + n->GetBits(i * BASE_BIT_COUNT, BASE_BIT_COUNT);
    }

    return n->IsNegative() ? -d : d;
}

BigInteger::BigInteger(int n): BigInteger(new BigIntegerImpl(n)) {
}

BigInteger::BigInteger(double d): BigInteger(doubleToBigInteger(d)) {
}

int BigInteger::toInt() const {
    int v = mImpl->GetBits(0, 32);
    return mImpl->IsNegative() ? -v : v;
}

double BigInteger::toDouble() const {
    return bigIntegerToDouble(mImpl.get());
}

bool BigInteger::isNegative() const {
    return mImpl->IsNegative();
}

BigInteger BigInteger::operator + (const BigInteger &o) const {
    return BigInteger(new BigIntegerImpl(*mImpl + *o.mImpl));
}

BigInteger BigInteger::operator - (const BigInteger &o) const {
    return BigInteger(new BigIntegerImpl(*mImpl - *o.mImpl));
}

BigInteger BigInteger::operator * (const BigInteger &o) const {
    return BigInteger(new BigIntegerImpl(*mImpl * *o.mImpl));
}

BigInteger BigInteger::operator / (const BigInteger &o) const {
    return BigInteger(new BigIntegerImpl(*mImpl / *o.mImpl));
}

BigInteger BigInteger::operator % (const BigInteger &o) const {
    return BigInteger(new BigIntegerImpl(*mImpl % *o.mImpl));
}

BigInteger BigInteger::powMod(const BigInteger &e, const BigInteger *m) const {
    BigInteger r(1);

    BigIntegerImpl tmp(*mImpl);
    for (int i = 0; i < (int)e.mImpl->BitCount(); ++i) {
        if (e.mImpl->GetBit(i)) {
            *r.mImpl *= tmp;
            if (m != nullptr) *r.mImpl %= *m->mImpl;
        }

        tmp *= tmp;
        if (m != nullptr) tmp %= *m->mImpl;
    }

    return r;
}

int BigInteger::getLowestBits(int n) const {
    return mImpl->GetBits(0, n);
}

bool BigInteger::operator == (const BigInteger &o) const {
    return *mImpl == *o.mImpl;
}

bool BigInteger::operator != (const BigInteger &o) const {
    return *mImpl != *o.mImpl;
}

bool BigInteger::operator < (const BigInteger &o) const {
    return *mImpl < *o.mImpl;
}

bool BigInteger::operator <= (const BigInteger &o) const {
    return *mImpl <= *o.mImpl;
}

bool BigInteger::operator > (const BigInteger &o) const {
    return *mImpl > *o.mImpl;
}

bool BigInteger::operator >= (const BigInteger &o) const {
    return *mImpl >= *o.mImpl;
}

void BigInteger::print(ostream &so) const {
    so << *mImpl;
}

BigInteger::BigInteger(BigIntegerImpl *impl): mImpl(impl) {
}
