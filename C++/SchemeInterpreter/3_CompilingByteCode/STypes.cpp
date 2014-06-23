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
        so << " * ";
        pair->cdr.writeToStream(so);
    }

    so << ')';
    return so;
}
