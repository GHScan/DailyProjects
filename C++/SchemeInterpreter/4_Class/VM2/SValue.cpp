#include "pch.h"
#include "SValue.h"

SValue SValue::TRUE = SValue(TT_Bool, 1);
SValue SValue::FALSE = SValue(TT_Bool, 0);
SValue SValue::EMPTY = SValue(TT_Reserved, 1);
SValue SValue::VOID = SValue(TT_Reserved, 2);
