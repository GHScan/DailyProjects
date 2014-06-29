#include "pch.h"
#include "STypes.h"

void SPair::_writeToStream(ostream &so) const {
    so << '(';
    car.writeToStream(so);

    const SPair *p = this;
    while (p->cdr.getType() == SVT_Pair) {
        p = p->cdr.getObject<SPair>();
        p->car.writeToStream(so);
    }

    if (p->cdr != SValue::EMPTY) {
        p->cdr.writeToStream(so);
    }

    so << ')';
}
