#include "pch.h"
#include "SStack.h"

SEvalStack::SEvalStack(int initCapactiy): mSize(0) {
    mCapacity = initCapactiy;
    mBasePtr = static_cast<SValue*>(::calloc(mCapacity, sizeof(mBasePtr[0])));
}

SEvalStack::~SEvalStack() {
    FREE(mBasePtr);
}

void SEvalStack::growCapacity() {
    int newCapacity = mCapacity * 3 / 2;

    mBasePtr = static_cast<SValue*>(::realloc(mBasePtr, newCapacity * sizeof(mBasePtr[0])));
    memset(mBasePtr + mCapacity, 0, (newCapacity - mCapacity) * sizeof(mBasePtr[0]));

    mCapacity = newCapacity;
}
