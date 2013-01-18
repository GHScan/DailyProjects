#pragma once

#include <cassert>

#include <vector>
#include <functional>

using std::min;
using std::max;

typedef unsigned char byte;
typedef unsigned int  uint;

template<typename T>
inline void copyBitsForT(T* dest, size_t destOffset, T src, size_t srcOffset, size_t len)
{
    assert(len > 0);

    const size_t BIT_W = sizeof(T) * 8;
    size_t bitLeftInDest = BIT_W - destOffset, bitLeftInSrc = BIT_W - srcOffset;
    assert(bitLeftInDest >= len && bitLeftInSrc >= len);

    T initMask = (T)((1 << len) - 1);
    T destMask = (T)(initMask << (bitLeftInDest - len));
    T srcMask = (T)(initMask << (bitLeftInSrc - len));
    
    T srcData = src;
    if (bitLeftInDest > bitLeftInSrc) srcData = (T)((src & srcMask) << (bitLeftInDest - bitLeftInSrc));
    else srcData = (T)((src & srcMask) >> (bitLeftInSrc - bitLeftInDest));

    *dest = (*dest & ~destMask) | (srcData & destMask);
}

inline void copyBitsImpl_byteUnaligned(
                     void* dest, size_t destOffset, const void* src, size_t srcOffset, size_t len)
{
    ((byte*&)dest) += (destOffset >> 3), destOffset = destOffset & 0x7;
    ((const byte*&)src) += (srcOffset >> 3), srcOffset = srcOffset & 0x7;

    if (srcOffset > 0)
    {   // 移动一个字节内的部分
        size_t bitLeftInDestByte = 8 - destOffset;
        size_t bitLeftInSrcByte = 8 - srcOffset;
        size_t bitsToCopy = min(bitLeftInSrcByte, len);

        if (bitsToCopy >= bitLeftInDestByte) // 相等是因为引入了len, 否则因为非字节对齐不可能相等
        {
            size_t subBitsToCopy = bitLeftInDestByte;

            copyBitsForT<byte>((byte*)dest, destOffset, *(const byte*)src, srcOffset, subBitsToCopy);

            ++(byte*&)dest;
            destOffset = 0, srcOffset += subBitsToCopy;
            len -= subBitsToCopy;

            bitLeftInDestByte = 8, bitLeftInSrcByte -= subBitsToCopy;
            bitsToCopy -= subBitsToCopy;
        }
        assert(bitsToCopy < bitLeftInDestByte);

        if (bitsToCopy > 0)
        {            
            copyBitsForT<byte>((byte*)dest, destOffset, *(const byte*)src, srcOffset, bitsToCopy);

            ++(const byte*&)src;
            destOffset += bitsToCopy, srcOffset = 0;
            len -= bitsToCopy;
        }
    }
    if (len == 0) return;
    assert(srcOffset == 0 && destOffset > 0);
    
    // 不能用32位拷贝...

    if (size_t bit8Count = len >> 3)
    {   // 8位部分
        for (size_t i = 0; i < bit8Count; ++i)
        {
            copyBitsForT<byte>((byte*)dest, destOffset, *(const byte*)src, 0, 8 - destOffset);
            ++(byte*&)dest;
            copyBitsForT<byte>((byte*)dest, 0, *(const byte*)src, 8 - destOffset, destOffset);
            ++(const byte*&)src;
        }

        len &= 0x7;
    }
    
    if (len > 0)
    {
        size_t bitLeftInDestByte = 8 - destOffset;
        if (len > bitLeftInDestByte)
        {
            size_t subBitsToCopy = bitLeftInDestByte;

            copyBitsForT<byte>((byte*)dest, destOffset, *(const byte*)src, srcOffset, subBitsToCopy);

            ++(byte*&)dest;
            destOffset = 0, srcOffset += subBitsToCopy;
            len -= bitLeftInDestByte;

            bitLeftInDestByte = 8;
        }
        assert(bitLeftInDestByte >= len);
        
        if (len > 0)
        {
            copyBitsForT<byte>((byte*)dest, destOffset, *(const byte*)src, srcOffset, len);
        }
    }
}

inline void copyBitsImpl_byteAligned(
                                 void* dest, size_t destOffset, const void* src, size_t srcOffset, size_t len)
{
    assert((destOffset & 0x7) == (srcOffset & 0x7));
    ((byte*&)dest) += (destOffset >> 3), destOffset = destOffset & 0x7;
    ((const byte*&)src) += (srcOffset >> 3), srcOffset = srcOffset & 0x7;

    if (srcOffset > 0) 
    {   // 移动一个字节内的部分
        size_t bitsToCopy = min(8 - srcOffset, len);

        copyBitsForT<byte>((byte*)dest, destOffset, *(const byte*)src, srcOffset, bitsToCopy);

        ++(byte*&)dest, ++(const byte*&)src;
        len -= bitsToCopy;
    }

    if (size_t bit32Count = len >> 5)
    {  // 移动32位对齐的
        for (size_t i = 0; i < bit32Count; ++i) *((uint*&)dest)++ = *((const uint*&)src)++;
        len &= 0x1f;
    }

    if (size_t bit8Count = len >> 3)
    {  // 移动8位对齐的
        for (size_t i = 0; i < bit8Count; ++i) *((byte*&)dest)++ = *((const byte*&)src)++;
        len &= 0x7;
    }

    if (len > 0)
    {  // 移动剩余
        copyBitsForT<byte>((byte*)dest, 0, *(const byte*)src, 0, len);
    }
}

inline void copyBits_quick(
    void *dest, size_t destOffset,
    const void *src, size_t srcOffset, size_t len)
{
    if ((destOffset & 0x7) == (srcOffset & 0x7))
    {
        copyBitsImpl_byteAligned(dest, destOffset, src, srcOffset, len);
    }
    else copyBitsImpl_byteUnaligned(dest, destOffset, src, srcOffset, len);
}

inline void copyBits_slow(
   void *dest, size_t destOffset, 
   const void *src, size_t srcOffset, size_t len)
{
    byte *destPtr = (byte*)dest;
    const byte* srcPtr = (const byte*)src;
    destPtr += destOffset >> 3, destOffset &= 0x7;
    srcPtr += srcOffset >> 3, srcOffset &= 0x7;

    for (size_t i = 0; i < len; ++i)
    { // 逐位操作
        if (((*srcPtr >> (7 - srcOffset)) & 0x1) == 1) 
        {
            *destPtr |= 1 << (7 - destOffset);
        }
        else *destPtr &= ~(1 << (7 - destOffset));
        
        if (destOffset == 7) destOffset = 0, ++destPtr;
        else ++destOffset;
        if (srcOffset == 7) srcOffset = 0, ++srcPtr;
        else ++srcOffset;
    }
}