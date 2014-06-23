#include "pch.h"
#include "SValue.h"

SValue SValue::TRUE = SValue(true);
SValue SValue::FALSE = SValue(false);
SValue SValue::INT_0 = SValue(int(0));
SValue SValue::INT_1 = SValue(1);
SValue SValue::INT_N_1 = SValue(-1);
SValue SValue::EMPTY = SValue(TV_Reserved, 1);
SValue SValue::VOID = SValue(TV_Reserved, 2);
