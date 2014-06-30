#include "pch.h"
#include "SValue.h"

SValue SValue::TRUE = SValue(true);
SValue SValue::FALSE = SValue(false);
SValue SValue::EMPTY = SValue::createReserved(1);
SValue SValue::VOID = SValue::createReserved(2);
