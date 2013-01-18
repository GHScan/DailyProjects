#pragma once

#include <cassert>

#include <vector>
#include <sstream>

typedef unsigned char Byte;

inline void bitsCopy_front2Front(Byte& dest, Byte src, size_t bitCnt)
{
    assert(bitCnt > 0);
    Byte t = 0xff >> bitCnt;
    dest = (dest & t) | (src & ~t);
}
inline void bitsCopy_front2Back(Byte& dest, Byte src, size_t bitCnt)
{
    assert(bitCnt > 0);
    size_t t = 0xff << bitCnt;
    dest = (dest & t) | (src >> (8 - bitCnt));
}
inline void bitsCopy_back2Front(Byte& dest, Byte src, size_t bitCnt)
{
    assert(bitCnt > 0);
    size_t t = 0xff >> bitCnt;
    dest = (dest & t) | (src << (8 - bitCnt));
}
inline void bitsCopy_back2Back(Byte& dest, Byte src, size_t bitCnt)
{
    assert(bitCnt > 0);
    Byte t = 0xff << bitCnt;
    dest = (dest & t) | (src & ~t);
}

inline void bitsCopy(Byte &dest, size_t destStartBit, Byte src, size_t srcStartBit, size_t bitCnt)
{
    assert(destStartBit + bitCnt < 8);
    assert(srcStartBit + bitCnt < 8);
    assert(bitCnt > 0);
    size_t t_8_sub_bitCnt = 8 - bitCnt;

    Byte destMask = 0xff << destStartBit;
    destMask >>= t_8_sub_bitCnt;
    destMask <<= t_8_sub_bitCnt - destStartBit;
    Byte srcMask = 0xff << srcStartBit;
    srcMask >>= t_8_sub_bitCnt;
    srcMask <<= t_8_sub_bitCnt - srcStartBit;
    dest &= ~destMask;
    src &= srcMask;
    dest |= destStartBit < srcStartBit ? src << (srcStartBit - destStartBit) : src >> (destStartBit - srcStartBit);
}

class BitStream
{
public:
    BitStream(): m_bitCnt(0){}
    BitStream(const void *src, size_t bitCnt):m_bitCnt(0)
    {
        push_back(src, bitCnt);
    }
    BitStream(const void* src, size_t startBit, size_t bitCnt):
    m_bitCnt(0)
    {
        push_back(src, startBit, bitCnt);
    }

    BitStream& operator += (const BitStream &o)
    {
        if (o.m_bitCnt == 0) return *this;
        push_back(&o.m_data[0], o.m_bitCnt);
        return *this;
    }

    size_t getBitCount() const 
    {
        return m_bitCnt;
    }

    size_t getByteCount() const
    {
        return m_data.size();
    }

    void* getPtr()
    {
        return m_bitCnt == 0 ? NULL : &m_data[0];
    }

    const void* getPtr() const
    {
        return const_cast<BitStream*>(this)->getPtr();
    }


    void push_back(bool b)
    {
        size_t mod8 = m_bitCnt & 7;
        if (mod8 == 0)
        {
            m_data.resize((m_bitCnt + 1 + 7) >> 3);
        }
        m_data.back() |= b << (7 - mod8);
        ++m_bitCnt;
    }

    void push_back(const void* src, size_t bitCnt)
    {
        if (m_bitCnt == 0)
        {
            m_bitCnt = bitCnt;
            m_data.assign((const Byte*)src, (const Byte*)src + ((bitCnt + 7) >> 3));
            return;
        }

        if ((m_bitCnt & 7) == 0)
        {
            m_data.insert(m_data.end(), (const Byte*)src, (const Byte*)src + ((bitCnt + 7) >> 3));
            m_bitCnt += bitCnt;
        }
        else
        {
            m_data.resize((m_bitCnt + bitCnt + 7) >> 3);

            size_t bitLeft = bitCnt;
            const Byte* _src = (const Byte*)src;
            Byte *_dest = &m_data[m_bitCnt >> 3];

            size_t mod8 = m_bitCnt & 7;
            size_t mod8_ = 8 - mod8;
            bool first = true;

            while (bitLeft > 0)
            {
                if (first)
                {
                    if (bitLeft < mod8_) break;
                    bitsCopy_front2Back(*_dest++, *_src, mod8_);
                    bitLeft -= mod8_;
                    first = false;
                }
                else
                {
                    if (bitLeft < mod8) break;
                    bitsCopy_back2Front(*_dest, *_src++, mod8);
                    bitLeft -= mod8;
                    first = true;
                }
            }

            if (bitLeft > 0)
            {
                if (first)
                {
                    bitsCopy(*_dest, mod8, *_src, 0, bitLeft);
                }
                else
                {
                    bitsCopy(*_dest, 0, *_src, mod8_, bitLeft);
                }
            }

            m_bitCnt += bitCnt;
        }
    }

    void push_back(const void* src, size_t startBit, size_t bitCnt)
    {
        assert(startBit < 8);

        size_t srcFirstByteBits = std::min(8 - startBit, bitCnt);
        bitCnt -= srcFirstByteBits;

        const Byte* _src = (const Byte*)src;
        Byte srcData = *_src;

        while (srcFirstByteBits-- > 0)
        {
            push_back(((srcData >> (7 - startBit++)) & 1) == 1);
        }

        if (bitCnt > 0)
        {
            push_back(_src + 1, bitCnt);
        }
    }

    void pop_back()
    {
        assert(m_bitCnt > 0);
        size_t mod8 = m_bitCnt & 7;
        if (mod8 == 1)
        {
            m_data.pop_back();
        }
        --m_bitCnt;
    }

    void pop_back(size_t bitCnt)
    {
        assert(m_bitCnt >= bitCnt);
        m_bitCnt -= bitCnt;
        m_data.resize((m_bitCnt + 7) >> 3);
    }



    void push_front(bool b)
    {
        if (m_bitCnt == 0)
        {
            push_back(b);
            return;
        }

        pushN0(1);
        rotateRight(1);
        write(0, b);
    }

    void push_front(const void* src, size_t bitCnt)
    {
        if (m_bitCnt == 0)
        {
            push_back(src, bitCnt);
            return;
        }

        pushN0(bitCnt);
        rotateRight(bitCnt);

        size_t div8 = bitCnt >> 3;
        size_t mod8 = bitCnt & 7;
        void* dest = &m_data[0];
        memcpy(dest, src, div8);
        if (mod8 > 0) bitsCopy_front2Front(*((Byte*)dest + div8), *((const Byte*)src + div8), mod8);
    }

    void pop_front()
    {
        assert(m_bitCnt > 0);
        rotateLeft(1);
        pop_back();
    }

    void pop_front(size_t bitCnt)
    {
        assert(m_bitCnt >= bitCnt);
        rotateLeft(bitCnt);
        pop_back(bitCnt);
    }

    bool read(size_t pos) const
    {
        assert(pos < m_bitCnt);
        return ((m_data[pos >> 3] >> (7 - (pos & 7))) & 1 ) == 1;
    }

    void write(size_t pos, bool b)
    {
        if (b) write1(pos); else write0(pos);
    }

    void write1(size_t pos)
    {
        assert(pos < m_bitCnt);
        m_data[pos >> 3] |= 1 << (7 - (7 & pos));
    }

    void write0(size_t pos)
    {
        assert(pos < m_bitCnt);
        m_data[pos >> 3] &= ~(1 << (7 - (7 & pos)));
    }

    void rotateLeft(size_t bitCnt)
    {
        assert(bitCnt < m_bitCnt);
        reverse(0, bitCnt);
        reverse(bitCnt, m_bitCnt);
        reverse(0, m_bitCnt);
    }

    void rotateRight(size_t bitCnt)
    {
        assert(bitCnt < m_bitCnt);
        rotateLeft(m_bitCnt  -bitCnt);
    }

    void reverse(size_t beginPos, size_t endPos)
    {
        assert(beginPos < m_bitCnt && endPos <= m_bitCnt && beginPos < endPos);
        size_t lastPos = endPos - 1;
        while (beginPos < lastPos)
        {
            bitSwap(beginPos++, lastPos--);
        }
    }

    void bitSwap(size_t pos0, size_t pos1)
    {
        assert(pos0 < m_bitCnt && pos1 < m_bitCnt);
        bool b = read(pos0);
        write(pos0, read(pos1));
        write(pos1, b);
    }

    void pushN0(size_t n)
    {
        m_bitCnt += n;
        m_data.resize((m_bitCnt + 7) >> 3, 0);
    }

    const std::string toString() const
    {
        std::ostringstream so;
        for (size_t i = 0; i < m_bitCnt; ++i)
        {
            so << read(i);
        }
        return so.str();
    }

    void swap(BitStream &o)
    {
        o.m_data.swap(m_data);
        std::swap(o.m_bitCnt, m_bitCnt);
    }

private:
    typedef std::vector<Byte>   Buffer;

private:
    Buffer  m_data;
    size_t  m_bitCnt;
};

class BitStreamIterator
{
public:
    BitStreamIterator(const BitStream& bs):
      m_bs(const_cast<BitStream*>(&bs)), m_curPos(0){}
      ~BitStreamIterator(){}

      bool hasMore() const { return m_curPos < m_bs->getBitCount(); }
      bool readNext() { bool ret = m_bs->read(m_curPos); move(1); return ret; }
      void writeNext(bool b) { m_bs->write(m_curPos, b); move(1); }

      void move(int step = 1) { m_curPos += step; }

private:
    BitStream   *m_bs;
    int          m_curPos;
};