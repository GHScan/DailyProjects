#include "pch.h"
#include "ScmTypes.h"

double ScmBigInt::toDouble() const {
    double r = 0;

    double base = 1LU << (WORD_BIT_COUNT - 1);
    base *= 2;

    for (int i = number.WordCount() - 1; i >= 0; --i) {
        r *= base;
        r += number.GetBits(i * WORD_BIT_COUNT, WORD_BIT_COUNT);
    }

    return number.IsNegative() ? -r : r;
}

ScmBigInt::BigInt ScmDouble::toBigInt() const {
    double f = fabs(number);
    double fbase = 1LU << (ScmBigInt::WORD_BIT_COUNT - 1);
    fbase *= 2;

    ScmBigInt::BigInt r = 0;
    ScmBigInt::BigInt bibase = 1;

    while (f > 0) {
        r += fmod(f, fbase) * bibase;
        bibase *= ScmBigInt::BigInt::Power2(ScmBigInt::WORD_BIT_COUNT);
        f = floor(f / fbase);
    }

    if (number < 0) r.SetNegative();
    return r;
}

void ScmPair::_writeToStream(ostream &so) const {
    if (this == EMPTY) {
        so << "()";
        return;
    }

    const ScmPair *pair = this;

    so << '(';
    pair->car->writeToStream(so);

    for (const ScmPair *nextPair = pair->cdr->dynamicCast<ScmPair>();
            nextPair != nullptr && nextPair != EMPTY;
            nextPair = pair->cdr->dynamicCast<ScmPair>()) {
        pair = nextPair;
        so << ' ';
        pair->car->writeToStream(so);
    }

    if (pair->cdr != EMPTY) {
        so << " . ";
        pair->cdr->writeToStream(so);
    }

    so << ')';
}
