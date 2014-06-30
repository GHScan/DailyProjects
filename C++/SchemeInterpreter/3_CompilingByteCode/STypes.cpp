#include "pch.h"
#include "STypes.h"

ostream& SPair::_writeToStream(ostream& so) const {
    so << '(';
    car.writeToStream(so);

    const SPair *pair = this;
    while (pair->cdr.getType() == SPair::TYPE) {
        pair = pair->cdr.getObject()->staticCast<SPair>();
        so << ' ';
        pair->car.writeToStream(so);
    }

    if (pair->cdr != SValue::EMPTY) {
        so << " . ";
        pair->cdr.writeToStream(so);
    }

    so << ')';
    return so;
}

double SBigInt::toDouble() const {
    double r = 0;

    double base = 1LU << (WORD_BIT_COUNT - 1);
    base *= 2;

    for (int i = number.WordCount() - 1; i >= 0; --i) {
        r *= base;
        r += number.GetBits(i * WORD_BIT_COUNT, WORD_BIT_COUNT);
    }

    return number.IsNegative() ? -r : r;
}

SBigInt::BigInt SDouble::toBigInt() const {
    double f = fabs(number);
    double fbase = 1LU << (SBigInt::WORD_BIT_COUNT - 1);
    fbase *= 2;

    SBigInt::BigInt r = 0;
    SBigInt::BigInt bibase = 1;

    while (f > 0) {
        r += fmod(f, fbase) * bibase;
        bibase *= SBigInt::BigInt::Power2(SBigInt::WORD_BIT_COUNT);
        f = floor(f / fbase);
    }

    if (number < 0) r.SetNegative();
    return r;
}
