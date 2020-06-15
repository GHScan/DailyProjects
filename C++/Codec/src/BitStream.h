#ifndef BIT_STREAM_H
#define BIT_STREAM_H

#include <cstdint>
#include <cassert>

#include <memory>
#include <type_traits>

#include "Utils.h"


class InputBitStream final
{
public:
    InputBitStream(void const *data, size_t size)
        : mData(static_cast<uint8_t const*>(data))
        , mSize(size)
        , mOffset(0)
    {
    }

    bool Eof() const
    {
        return mOffset >= mSize * 8;
    }

    int Peak() const
    {
        assert(!Eof());

        size_t byteOff = mOffset / 8;
        size_t bitOff = mOffset % 8;
        return (mData[byteOff] >> bitOff) & 1;
    }

    int Read()
    {
        int b = Peak();

        ++mOffset;

        return b;
    }

    template<typename T, typename std::enable_if<std::is_integral<T>::value, int>::type = 0>
    void Read(T &value, int length)
    {
        assert(sizeof(T) * 8 >= length);

        value = 0;
        for (int i = length - 1; i >= 0; --i)
            value |= T(Read() << i);
    }

    size_t Offset() const
    {
        return mOffset;
    }

    size_t OffsetInBytes() const
    {
        return DivideUp(Offset(), size_t(8));
    }

private:
    uint8_t const *mData;
    size_t mSize;
    size_t mOffset; // in bits
};


class OutputBitStream final
{
public:
    OutputBitStream(void *data, size_t size, bool ownData = false)
        : mData(
            static_cast<uint8_t*>(data),
            [this](uint8_t *p)
            { 
                if (mOwnData)
                    delete p;
            })
        , mSize(size)
        , mOwnData(ownData)
        , mOffset(0)
    {
        std::memset(mData.get(), 0, mSize);
    }

    OutputBitStream()
        : OutputBitStream(new uint8_t[16], 16, true)
    {
    }

    size_t Offset() const
    {
        return mOffset;
    }

    size_t OffsetInBytes() const
    {
        return DivideUp(Offset(), size_t(8));
    }

    void Write(int b)
    {
        if (mOffset >= mSize * 8)
        {
            assert(mOwnData);

            size_t newSize = mSize * 3 / 2;
            uint8_t *p = new uint8_t[newSize];

            std::memcpy(p, mData.get(), mSize);
            std::memset(p + mSize, 0, newSize - mSize);

            mSize = newSize;
            mData.reset(p);
        }

        size_t byteOff = mOffset / 8;
        size_t bitOff = mOffset % 8;
        mData.get()[byteOff] |= b << bitOff;

        ++mOffset;
    }

    template<typename T, typename std::enable_if<std::is_integral<T>::value, int>::type = 0>
    void Write(T v, int length)
    {
        assert(sizeof(T) * 8 >= length);

        for (int i = length - 1; i >= 0; --i)
            Write((v >> i) & 1);
    }

private:
    std::shared_ptr<uint8_t> mData;
    size_t mSize;
    bool const mOwnData;
    size_t mOffset;
};


#endif