#include "pch.h"
#include "STypes.h"

void SPair::_writeToStream(ostream &so) const {
    so << '(';
    mCar.writeToStream(so);

    const SPair *p = this;
    while (p->mCdr.getType() == SVT_Pair) {
        p = p->mCdr.getObject<SPair>();
        so << ' ';
        p->mCar.writeToStream(so);
    }

    if (p->mCdr != SValue::EMPTY) {
        so << " . ";
        p->mCdr.writeToStream(so);
    }

    so << ')';
}
