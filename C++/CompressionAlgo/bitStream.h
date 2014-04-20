#ifndef BIT_STREAM_H
#define BIT_STREAM_H

#include <stdint.h>

#include <type_traits>

class InputBitStream {
public:
    InputBitStream(const void *buf, int bitCount): mBuf((const uint8_t*)buf), mBitCount(bitCount), mPos(0) { }
    int bitCount() const { return mBitCount; }
    int pos() const { return mPos; }
    void rewind() { mPos = 0; }
    bool hasMore() const { return mPos < mBitCount; }
    const uint8_t* buf() const { return mBuf; }

    bool fetchBool() const {
        assert(mPos < mBitCount);
        return ((mBuf[mPos / ELEM_BITS] >> (mPos % ELEM_BITS)) & 1) != 0;
    }
    template<typename IntT>
    typename enable_if<is_integral<IntT>::value, IntT>::type fetchInt() const {
        assert(mPos + (int)sizeof(IntT) * 8 <= mBitCount);

        typedef typename make_unsigned<IntT>::type UintT;

        const int ELEM_BITS = sizeof(UintT) * 8;
        const UintT *buf = ((const UintT*)mBuf) + mPos / ELEM_BITS;
        UintT r = buf[0] >> (mPos % ELEM_BITS);
        if (mPos % ELEM_BITS) {
            r |= buf[1] << (ELEM_BITS - mPos % ELEM_BITS);
        }
        return (IntT)r;
    }

    void advance(int bitCount) {
        assert(mPos + bitCount <= mBitCount);
        mPos += bitCount;
    }

    bool readBool() {
        bool b = fetchBool();
        ++mPos;
        return b;
    }
    template<typename IntT>
    IntT readInt() {
        auto i = fetchInt<IntT>();
        mPos += sizeof(i) * 8;
        return i;
    }
    void readBits(void *buf, int bitCount) {
        assert(mPos + bitCount <= mBitCount);

        if (mPos % ELEM_BITS) {
            for (; bitCount >= 64; bitCount -= 64) *((uint64_t*&)buf)++ = readInt<uint64_t>();
            if (bitCount >= 32) *((uint32_t*&)buf)++ = readInt<uint32_t>(), bitCount -= 32;
            if (bitCount >= 16) *((uint16_t*&)buf)++ = readInt<uint16_t>(), bitCount -= 16;
            if (bitCount >= 8) *((uint8_t*&)buf)++ = readInt<uint8_t>(), bitCount -= 8;
            if (bitCount) {
                auto _buf = (uint8_t*)buf;
                *_buf = 0;
                for (int i = 0; i < bitCount; ++i) {
                    *_buf |= (readBool() ? 1 : 0) << i;
                }
            }
        } else {
            auto srcBuf = (uint8_t*)mBuf + mPos / 8;
            memcpy(buf, srcBuf, bitCount / 8);
            if (bitCount % 8) {
                ((uint8_t*)buf)[bitCount / 8] = (srcBuf[bitCount / 8]) & ((1 << bitCount % 8) - 1);
            }
            mPos += bitCount;
        }
    }
private:
    static const int ELEM_BITS = sizeof(uint8_t) * 8;
private:
    const uint8_t *mBuf;
    int mBitCount;
    int mPos;
};

class OutputBitStream {
public:
    OutputBitStream(void *buf, int bitCount): mBuf((uint8_t*)buf), mBitCount(bitCount), mPos(0) {
        memset(buf, 0, bitCount / 8);
    }
    int bitCount() const { return mBitCount; }
    int pos() const { return mPos; }
    void rewind() { mPos = 0; }
    uint8_t* buf() { return mBuf; }

    void writeBool(bool b) {
        assert(mPos < mBitCount);
        assert((mBuf[mPos / ELEM_BITS] & (1 << mPos % ELEM_BITS)) == 0);
        mBuf[mPos / ELEM_BITS] |= ((b ? 1 : 0) << mPos % ELEM_BITS);
        ++mPos;
    }
    template<typename IntT>
    void writeInt(IntT i, typename enable_if<is_integral<IntT>::value, IntT>::type* =0) {
        assert(mPos + (int)sizeof(IntT) * 8 <= mBitCount);

        typedef typename make_unsigned<IntT>::type UintT;

        const int ELEM_BITS = sizeof(UintT) * 8;
        UintT *buf = ((UintT*)mBuf) + mPos / ELEM_BITS;
        assert((buf[0] >> (mPos % ELEM_BITS)) == 0);
        buf[0] |= i << (mPos % ELEM_BITS);
        if (mPos % ELEM_BITS) {
            assert(buf[1] == 0);
            buf[1] |= i >> (ELEM_BITS - mPos % ELEM_BITS);
        }

        mPos += sizeof(i) * 8;
    }

    void writeBits(const void *buf, int bitCount) {
        assert(mPos + bitCount <= mBitCount);

        if (mPos % ELEM_BITS) {
            for (; bitCount >= 64; bitCount -= 64) writeInt(*((uint64_t*&)buf)++);
            if (bitCount >= 32) writeInt(*((uint32_t*&)buf)++), bitCount -= 32;
            if (bitCount >= 16) writeInt(*((uint16_t*&)buf)++), bitCount -= 16;
            if (bitCount >= 8) writeInt(*((uint8_t*&)buf)++), bitCount -= 8;
            if (bitCount) {
                auto _buf = (const uint8_t*)buf;
                for (int i = 0; i < bitCount; ++i) {
                    writeBool(((*_buf >> i) & 1) == 1);
                }
            }
        } else {
            auto destBuf = (uint8_t*)mBuf + mPos / 8;
            memcpy(destBuf, buf, bitCount / 8);
            if (bitCount % 8) {
                destBuf[bitCount / 8] = (((const uint8_t*)buf)[bitCount / 8]) & ((1 << (bitCount % 8)) - 1);
            }
            mPos += bitCount;
        }
    }
private:
    static const int ELEM_BITS = sizeof(uint8_t) * 8;

private:
    uint8_t *mBuf;
    int mBitCount;
    int mPos;
};

#endif
