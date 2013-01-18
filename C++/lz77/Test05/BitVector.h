#pragma once

#include <vector>
#include <locale>

#include <boost/type_traits.hpp>
#include <boost/assert.hpp>

#include "CopyBits.h"

#define BITN_TO_BYTEN(bitCount) ((bitCount + 7) / 8)

class BitVector
{
public:
    BitVector(void): m_size(0) {}
    BitVector(const void *src, size_t offset, size_t len) { assign(src, offset, len); }
    template<typename T>
    BitVector(const T &a) { assign(a); }
    BitVector(bool b, size_t count) { assign(b, count); }

    template<typename T>
    void assign(const T& a) { checkPOD<T>(); assign(&a, 0, sizeof(a) * 8); }
    void assign(const void *src, size_t offset, size_t len);
    void assign(bool b, size_t count);

    template<typename T>
    void push_back(const T& a) { checkPOD<T>(); push_back(&a, 0, sizeof(a) * 8); }
    void push_back(const BitVector& o) { push_back(o.data(), 0, o.size()); }
    void push_back(const void *src, size_t offset, size_t len);
    void push_back(bool b);

    void pop_back() { pop_back(1); }
    void pop_back(size_t count);

    bool get(size_t i) const;
    void set(size_t i, bool b);
    template<typename T>
    T get(size_t offset) const;
    template<typename T>
    void set(size_t offset, const T& a);

    const void* data() const { return &m_buf[0]; }

    size_t size() const { return m_size; }
    bool empty() const { return size() == 0; }
    void clear() { m_buf.clear(); m_size = 0; }
    
    void and(const BitVector& o);
    void or(const BitVector& o);
    void xor(const BitVector& o);
    void not();

private:
    template<typename T>
    static void checkPOD() { BOOST_ASSERT(boost::is_pod<T>::value); }

private:
    std::vector<byte>   m_buf;
    size_t              m_size;
};

class BitVectorIterator
{
public:
    BitVectorIterator(const BitVector& o): m_o(o), m_pos(0){}
    bool moveNext() { return ++m_pos <= m_o.size(); }
    bool getValue() const { return m_o.get(m_pos - 1); };

private:
    const BitVector &m_o;
    size_t           m_pos;
};

inline void BitVector::assign(const void *src, size_t offset, size_t len)
{
    assert(src != NULL);
    m_size = len;
    m_buf.assign(BITN_TO_BYTEN(len), 0);
    copyBits_quick(&m_buf[0], 0, src, offset, len);
}

inline void BitVector::assign(bool b, size_t count)
{
    m_size = count;
    if (b) m_buf.assign(BITN_TO_BYTEN(count), 0xff);
    else m_buf.assign(BITN_TO_BYTEN(count), 0);
}

inline void BitVector::push_back(const void *src, size_t offset, size_t len)
{
    size_t newByteSize = BITN_TO_BYTEN(size() + len);
    m_buf.resize(newByteSize);
    copyBits_quick(&m_buf[0], size(), src, offset, len);
    m_size += len;
}

inline void BitVector::push_back(bool b)
{
    size_t newByteSize = BITN_TO_BYTEN(size() + 1);
    m_buf.resize(newByteSize);
    copyBits_quick(&m_buf[0], size(), &b, 7, 1);
    ++m_size;
}

inline void BitVector::pop_back(size_t count)
{
    assert(size() >= count);
    size_t newByteSize = BITN_TO_BYTEN(size() - count);
    m_buf.resize(newByteSize);
    m_size = m_size - count;
}

inline bool BitVector::get(size_t i) const
{
    assert(i < size());
    return ((m_buf[i / 8] >> (7 - (i % 8))) & 0x1) == 1;
}

inline void BitVector::set(size_t i, bool b)
{
    assert(i < size());
    if (b) m_buf[i / 8] |= 1 << (7 - (i % 8));
    else m_buf[i / 8] &= ~(1 << (7 - (i % 8)));
}

template<typename T>
inline T BitVector::get(size_t offset) const
{
    checkPOD<T>();
    assert(offset + sizeof(T) * 8 <= size());
    T ret;
    copyBits_quick(&ret, 0, data(), offset, sizeof(T) * 8);
    return ret;
}

template<typename T>
inline void BitVector::set(size_t offset, const T& a)
{
    checkPOD<T>();
    assert(offset + sizeof(T) * 8 <= size());
    copyBits_quick(data(), offset, &a, 0, sizeof(T) * 8);
}

inline void BitVector::and(const BitVector& o)
{
    assert(size() == o.size());
    for (size_t i = 0; i < m_buf.size(); ++i) m_buf[i] &= o.m_buf[i];
}

inline void BitVector::or(const BitVector& o)
{
    assert(size() == o.size());
    for (size_t i = 0; i < m_buf.size(); ++i) m_buf[i] |= o.m_buf[i];
}

inline void BitVector::xor(const BitVector& o)
{
    assert(size() == o.size());
    for (size_t i = 0; i < m_buf.size(); ++i) m_buf[i] ^= o.m_buf[i];
}

inline void BitVector::not()
{
    for (size_t i = 0; i < m_buf.size(); ++i) m_buf[i] = ~m_buf[i];
}


inline std::istream& operator >> (std::istream &si, BitVector& o)
{
    char c = si.get();
    while (si.good() && !isgraph(c)) c = si.get();
    
    while (si.good() && c == '1' || c == '0')
    {
        o.push_back(c == '1');
        c = si.get();
    }

    if (!si.good()) return si;
    si.putback(c); return si;
}

inline std::ostream& operator << (std::ostream& so, BitVector& o)
{
    for (size_t i = 0; i < o.size(); ++i) so << (o.get(i) ? '1' : '0');
    return so;
}

#undef BITN_TO_BYTEN