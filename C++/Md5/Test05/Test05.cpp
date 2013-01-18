// Test05.cpp : 定义控制台应用程序的入口点。
//

#include "stdafx.h"

#include <cassert>
#include <cmath>

#include <vector>
#include <string>

typedef unsigned char byte;
typedef unsigned int  uint;

namespace Md5Algo
{
    class TiTable
    {
    public:
        uint get(size_t i)
        {
            assert(i < 64);
            return m_tis[i];
        }

        TiTable()
        {
            for (int i = 0; i < 64; ++i) 
            {
                long long l = long long(((long long)1 << 32) * abs(sin(i + 1.0)));
                m_tis[i] = uint(l);
            }
        }

    private:
        uint m_tis[64];
    }g_tiTable;

    inline uint F(uint x, uint y, uint z) { return (x & y) | (~x & z); }
    inline uint G(uint x, uint y, uint z) { return (x & z) | (~z & y); }
    inline uint H(uint x, uint y, uint z) { return x ^ y ^ z; }
    inline uint I(uint x, uint y, uint z) { return y ^ (x | ~z); }

    inline uint rotateLeft(uint val, uint bitCnt) { return (val >> (32 - bitCnt)) | (val << bitCnt); }

    inline void FF(uint& a, uint b, uint c, uint d, uint i, uint s, uint bit32)  {  a = b + rotateLeft(a + F(b, c, d) + bit32 + g_tiTable.get(i), s); }
    inline void GG(uint& a, uint b, uint c, uint d, uint i, uint s, uint bit32)  {  a = b + rotateLeft(a + G(b, c, d) + bit32 + g_tiTable.get(i), s); }
    inline void HH(uint& a, uint b, uint c, uint d, uint i, uint s, uint bit32)  {  a = b + rotateLeft(a + H(b, c, d) + bit32 + g_tiTable.get(i), s); }
    inline void II(uint& a, uint b, uint c, uint d, uint i, uint s, uint bit32)  {  a = b + rotateLeft(a + I(b, c, d) + bit32 + g_tiTable.get(i), s); }

    inline void turn1(uint *abcd, const uint *bit512)
    {
        uint &a = abcd[0], &b = abcd[1], &c = abcd[2], &d = abcd[3];

        FF(a, b, c, d, 0, 7 , bit512[0]); 
        FF(d, a, b, c, 1, 12, bit512[1]);
        FF(c, d, a, b, 2, 17, bit512[2]);
        FF(b, c, d, a, 3, 22, bit512[3]);

        FF(a, b, c, d, 4, 7 , bit512[4]); 
        FF(d, a, b, c, 5, 12, bit512[5]);
        FF(c, d, a, b, 6, 17, bit512[6]);
        FF(b, c, d, a, 7, 22, bit512[7]);

        FF(a, b, c, d, 8, 7 , bit512[8]); 
        FF(d, a, b, c, 9, 12, bit512[9]);
        FF(c, d, a, b, 10, 17, bit512[10]);
        FF(b, c, d, a, 11, 22, bit512[11]);

        FF(a, b, c, d, 12, 7 , bit512[12]); 
        FF(d, a, b, c, 13, 12, bit512[13]);
        FF(c, d, a, b, 14, 17, bit512[14]);
        FF(b, c, d, a, 15, 22, bit512[15]);
    }

    inline void turn2(uint *abcd, const uint *bit512)
    {
        uint &a = abcd[0], &b = abcd[1], &c = abcd[2], &d = abcd[3];

        GG(a, b, c, d, 16 + 0, 5 , bit512[1]);
        GG(d, a, b, c, 16 + 1, 9 , bit512[6]);
        GG(c, d, a, b, 16 + 2, 14, bit512[11]);
        GG(b, c, d, a, 16 + 3, 20, bit512[0]);

        GG(a, b, c, d, 16 + 4, 5 , bit512[5]);
        GG(d, a, b, c, 16 + 5, 9 , bit512[10]);
        GG(c, d, a, b, 16 + 6, 14, bit512[15]);
        GG(b, c, d, a, 16 + 7, 20, bit512[4]);

        GG(a, b, c, d, 16 + 8, 5 , bit512[9]);
        GG(d, a, b, c, 16 + 9, 9 , bit512[14]);
        GG(c, d, a, b, 16 + 10, 14, bit512[3]);
        GG(b, c, d, a, 16 + 11, 20, bit512[8]);

        GG(a, b, c, d, 16 + 12, 5 , bit512[13]);
        GG(d, a, b, c, 16 + 13, 9 , bit512[2]);
        GG(c, d, a, b, 16 + 14, 14, bit512[7]);
        GG(b, c, d, a, 16 + 15, 20, bit512[12]);
    }

    inline void turn3(uint *abcd, const uint *bit512)
    {
        uint &a = abcd[0], &b = abcd[1], &c = abcd[2], &d = abcd[3];

        HH(a, b, c, d, 32 + 0, 4 , bit512[5]);
        HH(d, a, b, c, 32 + 1, 11, bit512[8]);
        HH(c, d, a, b, 32 + 2, 16, bit512[11]);
        HH(b, c, d, a, 32 + 3, 23, bit512[14]);

        HH(a, b, c, d, 32 + 4, 4 , bit512[1]);
        HH(d, a, b, c, 32 + 5, 11, bit512[4]);
        HH(c, d, a, b, 32 + 6, 16, bit512[7]);
        HH(b, c, d, a, 32 + 7, 23, bit512[10]);

        HH(a, b, c, d, 32 + 8, 4 , bit512[13]);
        HH(d, a, b, c, 32 + 9, 11, bit512[0]);
        HH(c, d, a, b, 32 + 10, 16, bit512[3]);
        HH(b, c, d, a, 32 + 11, 23, bit512[6]);

        HH(a, b, c, d, 32 + 12, 4 , bit512[9]);
        HH(d, a, b, c, 32 + 13, 11, bit512[12]);
        HH(c, d, a, b, 32 + 14, 16, bit512[15]);
        HH(b, c, d, a, 32 + 15, 23, bit512[2]);
    }

    inline void turn4(uint *abcd, const uint *bit512)
    {
        uint &a = abcd[0], &b = abcd[1], &c = abcd[2], &d = abcd[3];

        II(a, b, c, d, 48 + 0, 6 , bit512[0]);
        II(d, a, b, c, 48 + 1, 10, bit512[7]);
        II(c, d, a, b, 48 + 2, 15, bit512[14]);
        II(b, c, d, a, 48 + 3, 21, bit512[5]);

        II(a, b, c, d, 48 + 4, 6 , bit512[12]);
        II(d, a, b, c, 48 + 5, 10, bit512[3]);
        II(c, d, a, b, 48 + 6, 15, bit512[10]);
        II(b, c, d, a, 48 + 7, 21, bit512[1]);

        II(a, b, c, d, 48 + 8, 6 , bit512[8]);
        II(d, a, b, c, 48 + 9, 10, bit512[15]);
        II(c, d, a, b, 48 + 10, 15, bit512[6]);
        II(b, c, d, a, 48 + 11, 21, bit512[13]);

        II(a, b, c, d, 48 + 12, 6 , bit512[4]);
        II(d, a, b, c, 48 + 13, 10, bit512[11]);
        II(c, d, a, b, 48 + 14, 15, bit512[2]);
        II(b, c, d, a, 48 + 15, 21, bit512[9]);
    }

    inline uint reverseBytes(uint i)
    {
        const byte *p = (const byte*)&i;
        return p[0] << 24 | p[1] << 16 | p[2] << 8 | p[3];
    }
}

class Md5
{
public:
    void begin()
    {
        m_abcd[0] = 0x67452301;
        m_abcd[1] = 0xefcdab89;
        m_abcd[2] = 0x98badcfe;
        m_abcd[3] = 0x10325476;
        m_leftBytes.clear();
        m_totalLen = 0;
    }
    void pushData(const void* src, size_t len)
    {
        m_totalLen += len;
        m_leftBytes.insert(m_leftBytes.end(), (const byte*)src, (const byte*)src + len);
        processLeftBytes();
    }
    void end()
    {
        append01AndLengthToLeftBytes();
        processLeftBytes();
    }

    const uint* getResult() const
    {
        return m_abcd;
    }

    const std::string getResultString() const
    {
        // 这个逆序只是为了输出
        int rabcd[4];
        for (int i = 0; i < 4; ++i) rabcd[i] = Md5Algo::reverseBytes(m_abcd[i]);

        char buf[36] = "";
        sprintf_s(buf, "%08x%08x%08x%08x", rabcd[0], rabcd[1], rabcd[2], rabcd[3]);
        return buf;
    }

private: 
    void processLeftBytes()
    {
        if (m_leftBytes.empty()) return;

        size_t div64 = m_leftBytes.size() / 64;
        const uint *data = (const uint*)&m_leftBytes[0];
        for (size_t i = 0; i < div64; ++i)
        {
            process512Bits(data);
            data += 16;
        }
        m_leftBytes.assign((const byte*)data, (const byte*)data + m_leftBytes.size() % 64);
    }
    void append01AndLengthToLeftBytes()
    {
        size_t mod64 = m_leftBytes.size() % 64;
        size_t fillByteCount = mod64 < 56 ? 56 - mod64 : 56 + 64 - mod64;
        if (fillByteCount > 0) m_leftBytes.push_back(0x80);
        while (fillByteCount-- > 1) m_leftBytes.push_back(0);

        m_totalLen *= 8;
        m_leftBytes.insert(m_leftBytes.end(), (const byte*)&m_totalLen, (const byte*)&m_totalLen + 8);
    }
    void process512Bits(const uint* src)
    {
        uint abcd[4];
        memcpy(abcd, m_abcd, sizeof(abcd));

        Md5Algo::turn1(abcd, src);
        Md5Algo::turn2(abcd, src);
        Md5Algo::turn3(abcd, src);
        Md5Algo::turn4(abcd, src);

        for (int i = 0; i < 4; ++i) m_abcd[i] += abcd[i];
    }

private:
    uint                m_abcd[4];
    std::vector<byte>   m_leftBytes;
    unsigned long long  m_totalLen;
};

#include <fstream>
#include <boost/progress.hpp>

void pauseConsole()
{
    cin.get();
}

int main(int argc, char * argv[])
{
    atexit(pauseConsole);

    Md5 m;

    const char *strs[] = 
    {
        "",
        "a",
        "abc",
        "message digest",
        "abcdefghijklmnopqrstuvwxyz",
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
    };
    for (int i = 0; i < _countof(strs); ++i)
    {
        m.begin();
        m.pushData(strs[i], strlen(strs[i]));
        m.end();
        cout << m.getResultString() << endl;
    }

    if (argc > 1)    
    {
        std::string fileName = argv[1];

        char *pre = setlocale(LC_ALL, "");
        std::ifstream fi(fileName.c_str(), std::ios::binary);
        setlocale(LC_ALL, pre);

        if (fi)
        {
            boost::progress_timer _timer;

            m.begin();
            char buff[1 << 15] = {0};
            while (fi)
            {
                fi.read(buff, sizeof(buff));
                size_t sz = fi.gcount();
                if (sz > 0) m.pushData(buff, sz);
            }
            m.end();

            cout << "文件的md5 : " << m.getResultString() << endl;
        }
        else cout << "文件没有打开" << endl;
    }
    else cout << "要计算文件的md5请拖拽文件" << endl;
}
