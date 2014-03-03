#ifndef BIT_VECTOR_H
#define BIT_VECTOR_H

#include <vector>

class BitVector {
public:
    BitVector(int size): mSize(size), mData((size + ELEM_BITS - 1) / ELEM_BITS) {}
    int getSize() const { return mSize;}
    int get(int i) const {
        return (mData[i / ELEM_BITS] >> (i % ELEM_BITS)) & 1;
    }
    void set(int i, int b) {
        int v = mData[i / ELEM_BITS];
        v &= ~(1 << i % ELEM_BITS);
        v |= b << (i % ELEM_BITS);
        mData[i / ELEM_BITS] = v;
    }
private:
    static const int ELEM_BITS = sizeof(int) * 8;
private:
    vector<int> mData;
    int mSize;
};

#endif
