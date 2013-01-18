// Test05.cpp : 定义控制台应用程序的入口点。
//

#include "stdafx.h"

#include <cassert>

#include <vector>
#include <string>
#include <algorithm>

#include <boost/progress.hpp>
#include <boost/unordered_map.hpp>
#include <boost/pool/pool_alloc.hpp>

#include "BitVector.h"

#define MAX_MATCH_DISTANCE_BW  21
#define MAX_MATCH_LEN_BW       10

struct IInputStream
{
    virtual ~IInputStream() {}
    virtual bool isOpened() = 0;
    virtual size_t read(void* dest, size_t len) = 0;
};

struct IOutputStream
{
    virtual ~IOutputStream() {}
    virtual bool isOpened() = 0;
    virtual size_t write(void *src, size_t len) = 0;
};

class CircleBuffer
{
public:
    struct ICircleBufferListener
    {
        virtual ~ICircleBufferListener() {}
        virtual void onPreCoverData(size_t offset, size_t len) = 0; // 覆盖数据之前
        virtual void onWritedData(size_t offset, size_t len) = 0;   // 写数据之后
    };

public:
    CircleBuffer(size_t maxSize);
    size_t getMaxSize() const { return m_maxSize; }
    void setListener(ICircleBufferListener *listener) { m_listener = listener; }

    void writeData(const void *src, size_t len);
    
    size_t getOffsetOfWritePtrDistance(size_t distan);
    size_t addOffset(size_t offset, size_t change); // 最多到数据段尾
    size_t subOffset(size_t offset, size_t change); // 最多到数据段头

private:
    friend class CircleBufferDataSeg;
    size_t getWritePtrOffset() const;
    const void* data() const { return &m_buf[0]; }

private:
    std::vector<byte>        m_buf;
    size_t                   m_maxSize;
    ICircleBufferListener   *m_listener;
    size_t                   m_startPos;
};

CircleBuffer::CircleBuffer(size_t maxSize):
m_maxSize(maxSize), m_listener(NULL), m_startPos(0)
{
    m_buf.reserve(m_maxSize);
}

void CircleBuffer::writeData(const void *src, size_t len)
{
    assert(len > 0);
    if (size_t incBytes = min(m_maxSize - m_buf.size(), len))
    {
        m_buf.resize(m_buf.size() + incBytes);
        memcpy(&m_buf[0] + m_buf.size() - incBytes, src, incBytes);
        (const byte*&)src += incBytes, len -= incBytes;

        if (m_listener != NULL) m_listener->onWritedData(m_buf.size() - incBytes, incBytes);
    }

    while (len > 0)
    {
        size_t bytesToCopy = min(getMaxSize() - m_startPos, len);

        if (m_listener != NULL) m_listener->onPreCoverData(m_startPos, bytesToCopy);

        memcpy(&m_buf[0] + m_startPos, src, bytesToCopy);

        if (m_listener != NULL) m_listener->onWritedData(m_startPos, bytesToCopy);

        m_startPos += bytesToCopy;
        if (m_startPos == m_maxSize) m_startPos = 0;

        (const byte*&)src += bytesToCopy, len -= bytesToCopy;
    }
}

inline size_t CircleBuffer::getOffsetOfWritePtrDistance(size_t distan)
{
    return subOffset(getWritePtrOffset(), distan);
}

inline size_t CircleBuffer::addOffset(size_t offset, size_t change)
{
    size_t writePtrOffset = getWritePtrOffset();
    if (offset > writePtrOffset)
    {
        assert(m_buf.size() == m_maxSize); // 已经回绕了
        size_t inc = min(getMaxSize() - offset, change);
        offset += inc, change -= inc;
        if (offset == m_maxSize) offset = 0;
    }
    return min(offset + change, writePtrOffset);
}

inline size_t CircleBuffer::subOffset(size_t offset, size_t change)
{
    size_t writePtrOffset = getWritePtrOffset();
    if (offset <= writePtrOffset)
    {
        size_t dec = min(change, offset);
        offset -= dec, change -= dec;
    }
    if (change > 0 && m_buf.size() == m_maxSize)  
    { // 回绕了
        offset = max(m_maxSize - change, writePtrOffset);
    }
    return offset;
}

inline size_t CircleBuffer::getWritePtrOffset() const
{
    return m_buf.size() == m_maxSize ? m_startPos : m_buf.size();
}

class CircleBufferDataSeg
{
public:
    CircleBufferDataSeg(const CircleBuffer& buf, size_t offset);
    size_t getLength() const;
    void getData(void* dest, size_t len) const;
    size_t getMaxMatchLength(const void *src, size_t len, size_t minMatchLen) const;

private:
    const CircleBuffer    &m_buf;
    size_t                 m_offset;
};

inline CircleBufferDataSeg::CircleBufferDataSeg(const CircleBuffer& buf, size_t offset): 
m_buf(buf), m_offset(offset)
{ 
    assert(m_offset < m_buf.getMaxSize());
}

inline size_t CircleBufferDataSeg::getLength() const
{
    size_t writePtrOffset = m_buf.getWritePtrOffset();
    if (m_offset <= writePtrOffset) return writePtrOffset - m_offset;
    return m_buf.getMaxSize() - m_offset + writePtrOffset;
}

inline void CircleBufferDataSeg::getData(void* dest, size_t len) const
{
    assert(getLength() >= len);
    size_t writePtrOffset = m_buf.getWritePtrOffset();
    if (m_offset <= writePtrOffset) 
    {
        memcpy(dest, (const byte*)m_buf.data() + m_offset, len);
        return;
    }

    if (size_t bytesToCopy = min(m_buf.getMaxSize() - m_offset, len))
    { // 拷贝前半段
        memcpy(dest, (const byte*)m_buf.data() + m_offset, bytesToCopy);
        (byte*&)dest += bytesToCopy, len -= bytesToCopy;
    }

    if (len > 0)
    { // 拷贝后半段
        memcpy(dest, m_buf.data(), len);
    }
}

inline size_t CircleBufferDataSeg::getMaxMatchLength(const void *src, size_t len, size_t minMatchLen) const
{
    assert(len >= minMatchLen);
    
    // 获取前后半段数据
    const void* p1 = NULL, *p2 = NULL;
    size_t len1 = 0, len2 = 0;    
    size_t writePtrOffset = m_buf.getWritePtrOffset();
    if (m_offset <= writePtrOffset)
    {
        p1 = (const byte*)m_buf.data() + m_offset, len1 = writePtrOffset - m_offset;
    }
    else
    {
        p1 = (const byte*)m_buf.data() + m_offset, len1 = m_buf.getMaxSize() - m_offset;
        p2 = m_buf.data(), len2 = writePtrOffset;
    }

    // 将源分割成两部分
    bool splitSrc = len2 > 0 && len1 < len;
    const void *src1 = src, *src2 = splitSrc ? (const byte*)src + len1 : NULL;
    size_t srcLen1 = splitSrc ? len1 : len, srcLen2 = splitSrc ? len - len1 : 0;

    size_t maxMatchLen = 0;
    // 匹配第一段
    do
    {
        size_t maxLen = min(len1, srcLen1);
        if (maxLen <= minMatchLen) 
        {
            if (memcmp(src1, p1, maxLen) == 0) 
            {
                minMatchLen -= maxLen, maxMatchLen += maxLen;
                break; 
            }
            else return 0;
        }
        assert(maxLen > minMatchLen);

        if (memcmp(src1, p1, minMatchLen) != 0) return 0;
        maxMatchLen = minMatchLen, minMatchLen = 0;

        while (
            maxMatchLen < maxLen && 
            memcmp(src1, p1, maxMatchLen + 1) == 0) ++ maxMatchLen;

        if (maxMatchLen < maxLen) return maxMatchLen;
    }
    while (0);

    // 匹配第二段
    assert(maxMatchLen == min(len1, srcLen1));
    if (p2 == NULL || src2 == NULL) return maxMatchLen;
    
    {
        assert(len2 > 0 && srcLen2 > 0);
        size_t maxLen = min(len2, srcLen2);
        assert(maxLen >= minMatchLen);
        size_t maxMatchLen2 = 0;

        if (minMatchLen > 0 && memcmp(src2, p2, minMatchLen) != 0) return 0;
        maxMatchLen2 += minMatchLen, minMatchLen = 0;

        while (
            maxMatchLen2 < maxLen && 
            memcmp(src2, p2, maxMatchLen2 + 1) == 0) ++maxMatchLen2;

        maxMatchLen += maxMatchLen2;
    }

    return maxMatchLen;
}


class SlidingWindow:
    public CircleBuffer::ICircleBufferListener
{
public:
    SlidingWindow(bool hash4FindMatch = false);
    void sliding(const void *src, size_t len);
    size_t read(void *dest, size_t distan, size_t len);

    bool findMatch(const void* src, size_t len, size_t &distan, size_t &matchLen);

private:
    virtual void onPreCoverData(size_t offset, size_t len);
    virtual void onWritedData(size_t offset, size_t len);

private:
    typedef boost::unordered_multimap<
        size_t, size_t, 
        boost::hash<size_t>, 
        std::equal_to<size_t>, 
        boost::fast_pool_allocator<std::pair<size_t, size_t>>> HashTable;

private:
    CircleBuffer    m_buf;
    bool            m_hash4FindMatch;
    HashTable       m_hashTable;
};

SlidingWindow::SlidingWindow(bool hash4FindMatch):
m_hash4FindMatch(hash4FindMatch), m_buf(1 << MAX_MATCH_DISTANCE_BW)
{
    if (m_hash4FindMatch) m_buf.setListener(this);
}

void SlidingWindow::sliding(const void *src, size_t len)
{   
    m_buf.writeData(src, len);
}

size_t SlidingWindow::read(void *dest, size_t distan, size_t len)
{
    CircleBufferDataSeg seg(m_buf, m_buf.getOffsetOfWritePtrDistance(distan));
    assert(seg.getLength() >= len);
    seg.getData(dest, len);
    return len;
}

void SlidingWindow::onPreCoverData(size_t offset, size_t len)
{
    for (size_t i = 0; i < len; ++i)
    {
        CircleBufferDataSeg seg(m_buf, offset);
        if (seg.getLength() < 4) break;

        size_t key;
        seg.getData(&key, 4);

        std::pair<HashTable::iterator, HashTable::iterator> p = m_hashTable.equal_range(key);
        while (p.first != p.second)
        {
            if (p.first->second == offset) break;
            ++p.first;
        }
        assert(p.first != p.second);
        m_hashTable.erase(p.first);
        
        offset = m_buf.addOffset(offset, 1);
    }
}

void SlidingWindow::onWritedData(size_t offset, size_t len)
{
    offset = m_buf.subOffset(offset, 3); // 补上上次的最后几个段
    for (size_t i = 0; i < len; ++i)
    {
        CircleBufferDataSeg seg(m_buf, offset);
        if (seg.getLength() < 4) break; // 最后几个段因为追上了写指针...

        size_t key;
        seg.getData(&key, 4);

#ifdef _DEBUG
        std::pair<HashTable::iterator, HashTable::iterator> p = m_hashTable.equal_range(key);
        while (p.first != p.second)
        {
            assert(p.first->second != offset);
            ++p.first;
        }
#endif

        m_hashTable.insert(std::make_pair(key, offset));

        offset = m_buf.addOffset(offset, 1);
    }
}

bool SlidingWindow::findMatch(const void* src, size_t len, size_t &distan, size_t &matchLen)
{
    assert(m_hash4FindMatch);
    if (len < 4) return false;
    len = min<size_t>(len, 1 << MAX_MATCH_LEN_BW); // 最长匹配到MAX_MATCH_LEN_BW位
    
    size_t key = *(const size_t*)src;
    std::pair<HashTable::iterator, HashTable::iterator> p = m_hashTable.equal_range(key);
    if (p.first == p.second) return false;

    matchLen = 5;
    distan = CircleBufferDataSeg(m_buf, p.first->second).getLength();
    for (; p.first != p.second; ++p.first)
    {
        if (matchLen > len) break;

        CircleBufferDataSeg seg(m_buf, p.first->second);
        if (seg.getLength() < matchLen) continue;
        size_t r = seg.getMaxMatchLength(src, len, matchLen);
        if (r >= matchLen)
        {
            matchLen = r + 1;
            distan = seg.getLength();
        }
    }
    assert(distan != -1);
    --matchLen;

    return true;
}

class Packer
{
public:
    Packer(IOutputStream *so): m_so(so), m_bufOffset(0) {}
    ~Packer(){ packUncompressData(); flushToStream(); }
    void writeUncompressData(byte b) 
    { 
        m_uncompressData.push_back(b); 
        if (m_uncompressData.size() >= 128) packUncompressData();  
    }
    void writeCompressInfo(size_t distan, size_t len)
    {
        packUncompressData();
        packCompressInfo(distan, len);
    }

private:
    void packUncompressData()
    {
        if (m_uncompressData.empty()) return;

        assert(m_uncompressData.size() <= 128);
        byte head = (byte)(0x00 | (m_uncompressData.size() - 1));
        writeToStream(&head, 1);
        writeToStream(&m_uncompressData[0], m_uncompressData.size());
        m_uncompressData.clear();
    }
    void packCompressInfo(size_t distan, size_t len)
    {
        assert(distan <= (1 << MAX_MATCH_DISTANCE_BW) && len <= (1 << 10) && distan > 0 && len > 0);
        --distan, --len; // 可以保存更大
        static BitVector v;
        v.clear();
        v.push_back(true); // 1位
        BOOST_STATIC_ASSERT(MAX_MATCH_DISTANCE_BW == 21 && MAX_MATCH_LEN_BW == 10);
        v.push_back(&distan, 0, 8); v.push_back(&distan, 8, 8); v.push_back(&distan, 19, 5); // MAX_MATCH_DISTANCE_BW位
        v.push_back(&len, 0, 8); v.push_back(&len, 14, 2); // MAX_MATCH_LEN_BW位
        writeToStream(v.data(), 4);
    }
    void writeToStream(const void* src, size_t len) 
    { // 这里先写到缓冲, 不直接写文件
        while (len > 0)
        {
            size_t bytesToCopy = min(len, sizeof(m_buf) - m_bufOffset);
            memcpy(m_buf + m_bufOffset, src, bytesToCopy);

            m_bufOffset += bytesToCopy;
            if (m_bufOffset == sizeof(m_buf)) m_so->write(m_buf, m_bufOffset), m_bufOffset = 0;

            len -= bytesToCopy, (const byte*&)src += bytesToCopy;
        }
    }
    void flushToStream() { m_so->write(m_buf, m_bufOffset), m_bufOffset = 0; }

private:
    IOutputStream       *m_so;
    std::vector<byte>    m_uncompressData;
    byte                 m_buf[1 << 16];
    size_t               m_bufOffset;
};

class UnPacker
{
public:
    UnPacker(IInputStream *si): 
      m_si(si), m_bufOffset(0), m_distan(0), 
      m_matchLen(0), m_hasCompressInfo(false), m_bytesInBuf(0){}
    bool readUncompressData(byte &b)
    {
        tryReadNextData();
        if (m_uncompressData.empty()) return false;
        b = m_uncompressData.back(), m_uncompressData.pop_back();
        return true;
    }
    bool readCompressInfo(size_t &distan, size_t &len)
    {
        tryReadNextData();
        if (!m_hasCompressInfo)  return false;
        distan = m_distan, len = m_matchLen;
        m_hasCompressInfo = false;
        return true;
    }

private:
    void tryReadNextData()
    {
        if (m_hasCompressInfo || !m_uncompressData.empty()) return;
        byte b = 0;
        if (!readFromStream(&b, 1)) return;
        if (b >> 7)
        {
            BitVector v;
            v.push_back(b);
            byte _t[3] = {0};
            if (!readFromStream(_t, 3)) assert(0);
            v.push_back(_t);

            BOOST_STATIC_ASSERT(MAX_MATCH_DISTANCE_BW == 21 && MAX_MATCH_LEN_BW == 10);
            byte *p = (byte*)&m_distan;
            p[0] = v.get<byte>(1);
            p[1] = v.get<byte>(9);
            p[2] = v.get<byte>(14) & 0x1f;
            p = (byte*)&m_matchLen;
            p[0] = v.get<byte>(22);
            p[1] = v.get<byte>(24) & 0x3;
            ++m_distan, ++m_matchLen;

            m_hasCompressInfo = true;
        }
        else
        {
            size_t cnt = (b & 0x7f) + 1;
            m_uncompressData.resize(cnt);
            if (!readFromStream(&m_uncompressData[0], cnt)) assert(0);
            std::reverse(m_uncompressData.begin(), m_uncompressData.end());
        }
    }

    bool readFromStream(void *dest, size_t len)
    {
        while (len > 0)
        {
            size_t bytesToCopy = min(len, m_bytesInBuf - m_bufOffset);
            memcpy(dest, m_buf + m_bufOffset, bytesToCopy);

            (byte*&)dest += bytesToCopy, len -= bytesToCopy;

            m_bufOffset += bytesToCopy;
            if (m_bufOffset >= m_bytesInBuf) 
            {
                m_bufOffset = 0;
                m_bytesInBuf = m_si->read(m_buf, sizeof(m_buf));
                if (m_bytesInBuf == 0 && len > 0) return false;
            }
        }
        return true;
    }

private:
    std::vector<byte>m_uncompressData;
    size_t           m_distan, m_matchLen;
    bool             m_hasCompressInfo;

    byte             m_buf[1 << 16];
    size_t           m_bufOffset;
    size_t           m_bytesInBuf;
    IInputStream    *m_si;
};

void lz77_encode(IInputStream *is, IOutputStream *os)
{
    assert(is->isOpened() && os->isOpened());

    SlidingWindow win(true);
    Packer packer(os);

    byte buf[1 << 16] = {0}; // 这个缓冲不只是为了性能! 不可以将它移到IInputStream的实现中
    size_t leftBytesInBuf = 0;
    for (;;)
    {
        assert(leftBytesInBuf < sizeof(buf));
        size_t readed = is->read(buf + leftBytesInBuf, sizeof(buf) - leftBytesInBuf);
        size_t len = readed + leftBytesInBuf;
        leftBytesInBuf = 0;
        if (len == 0) break;
        bool endOfInput = readed == 0;

        size_t offset = 0;
        for (;;)
        {
            if (offset >= len) break;

            size_t distan = 0, matchLen = 0;
            if (!win.findMatch(buf + offset, len - offset, distan, matchLen))
            {
                win.sliding(buf + offset, 1);
                packer.writeUncompressData(buf[offset]);
                ++offset;
                continue;
            }

            assert(offset + matchLen <= len);
            if (offset + matchLen < len || endOfInput)
            {
                win.sliding(buf + offset, matchLen);
                packer.writeCompressInfo(distan, matchLen);
                offset += matchLen;
                continue;
            }

            assert(offset + matchLen == len && !endOfInput);

            leftBytesInBuf = len - offset;
            memcpy(buf, buf + offset, leftBytesInBuf);
            break;
        }
    }
}

void lz77_decode(IInputStream *is, IOutputStream *os)
{
    assert(is->isOpened() && os->isOpened());

    UnPacker unpacker(is);
    SlidingWindow win;

    byte buf[1 << 16] = {0};
    size_t offset = 0;
    for (;;)
    {
        byte b;
        size_t distan = 0, matchLen = 0;
        if (unpacker.readUncompressData(b))
        {
            buf[offset++] = b;
            win.sliding(&b, 1);
            if (offset == sizeof(buf)) os->write(buf, offset), offset = 0;
        }
        else if (unpacker.readCompressInfo(distan, matchLen))
        {
            while (matchLen > 0)
            {
                size_t t = min(matchLen, sizeof(buf) - offset);
                if (win.read(buf + offset, distan, t) < t) assert(0);
                win.sliding(buf + offset, t);

                offset += t;
                matchLen -= t;

                if (offset == sizeof(buf)) os->write(buf, offset), offset = 0;
            }
        }
        else 
        {
            os->write(buf, offset);
            break;
        }
    }
}

class FileInputStream:
    public IInputStream
{
public:
    FileInputStream(const char *name) { m_file = fopen(name, "rb");  }
    ~FileInputStream() { fclose(m_file); }

private:
    virtual bool isOpened() { return m_file != NULL; }
    virtual size_t read(void* dest, size_t len) { return fread(dest, 1, len, m_file); }

private:
    FILE    *m_file;
};

class FileOutputStream:
    public IOutputStream
{
public:
    FileOutputStream(const char *name) { m_file = fopen(name, "wb"); }
    ~FileOutputStream() { fclose(m_file); }

private:
    virtual bool isOpened() { return m_file != NULL; }
    virtual size_t write(void *src, size_t len) { return fwrite(src, 1, len, m_file); }
private:
    FILE    *m_file;
};

void pauseConsole()
{
    cin.get();
}

int main(int argc, char *argv[])
{
    atexit(pauseConsole);

    if (argc < 2) {cout << "没有输入要压缩的文件" << endl; return 0; }

    std::string fileName = argv[1];
    size_t pos = fileName.rfind('.');
    assert(pos != -1);
    if (fileName.substr(pos + 1) != "pak")
    {   // 压缩
        boost::progress_timer __timer;

        FileInputStream fi(fileName.c_str());
        FileOutputStream fo((fileName.substr(0, pos) + ".pak").c_str());
        lz77_encode(&fi, &fo);
        cout << "压缩完毕" << endl;
    }   
    else
    {   // 解压
        boost::progress_timer __timer;
        
        FileInputStream fi(fileName.c_str());
        FileOutputStream fo((fileName.substr(0, pos) + ".rst").c_str());
        lz77_decode(&fi, &fo);
        cout << "解压完毕" << endl;
    }
}   