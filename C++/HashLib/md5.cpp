#include "pch.h"

#include "utils.h"
#include "md5.h"

static void process(const uint8_t data[Md5::BLOCK_BYTES], uint32_t H[Md5::OUTPUT_BYTES / sizeof(uint32_t)]) {
    const int BLOCK_WORDS = Md5::BLOCK_BYTES / sizeof(uint32_t);
    uint32_t w[BLOCK_WORDS];
    for (int i = 0; i < BLOCK_WORDS; ++i) {
        w[i] = readUint_littleEndian<uint32_t>(data + i * sizeof(uint32_t));
    }

    uint32_t a = H[0], b = H[1], c = H[2], d = H[3];

#define FF(a, b, c, d, m, k, r)    a = b + leftRotate(a + k + m + ((b&c)|((~b)&d)), r);
#define GG(a, b, c, d, m, k, r)    a = b + leftRotate(a + k + m + ((d&b)|((~d)&c)), r);
#define HH(a, b, c, d, m, k, r)    a = b + leftRotate(a + k + m + ((b^c)^d), r);
#define II(a, b, c, d, m, k, r)    a = b + leftRotate(a + k + m + (c^(b|(~d))), r);
    FF(a, b, c, d, w[ 0], 0xd76aa478,  7);
    FF(d, a, b, c, w[ 1], 0xe8c7b756, 12);
    FF(c, d, a, b, w[ 2], 0x242070db, 17);
    FF(b, c, d, a, w[ 3], 0xc1bdceee, 22);
    FF(a, b, c, d, w[ 4], 0xf57c0faf,  7);
    FF(d, a, b, c, w[ 5], 0x4787c62a, 12);
    FF(c, d, a, b, w[ 6], 0xa8304613, 17);
    FF(b, c, d, a, w[ 7], 0xfd469501, 22);
    FF(a, b, c, d, w[ 8], 0x698098d8,  7);
    FF(d, a, b, c, w[ 9], 0x8b44f7af, 12);
    FF(c, d, a, b, w[10], 0xffff5bb1, 17);
    FF(b, c, d, a, w[11], 0x895cd7be, 22);
    FF(a, b, c, d, w[12], 0x6b901122,  7);
    FF(d, a, b, c, w[13], 0xfd987193, 12);
    FF(c, d, a, b, w[14], 0xa679438e, 17);
    FF(b, c, d, a, w[15], 0x49b40821, 22);
    GG(a, b, c, d, w[ 1], 0xf61e2562,  5);
    GG(d, a, b, c, w[ 6], 0xc040b340,  9);
    GG(c, d, a, b, w[11], 0x265e5a51, 14);
    GG(b, c, d, a, w[ 0], 0xe9b6c7aa, 20);
    GG(a, b, c, d, w[ 5], 0xd62f105d,  5);
    GG(d, a, b, c, w[10], 0x02441453,  9);
    GG(c, d, a, b, w[15], 0xd8a1e681, 14);
    GG(b, c, d, a, w[ 4], 0xe7d3fbc8, 20);
    GG(a, b, c, d, w[ 9], 0x21e1cde6,  5);
    GG(d, a, b, c, w[14], 0xc33707d6,  9);
    GG(c, d, a, b, w[ 3], 0xf4d50d87, 14);
    GG(b, c, d, a, w[ 8], 0x455a14ed, 20);
    GG(a, b, c, d, w[13], 0xa9e3e905,  5);
    GG(d, a, b, c, w[ 2], 0xfcefa3f8,  9);
    GG(c, d, a, b, w[ 7], 0x676f02d9, 14);
    GG(b, c, d, a, w[12], 0x8d2a4c8a, 20);
    HH(a, b, c, d, w[ 5], 0xfffa3942,  4);
    HH(d, a, b, c, w[ 8], 0x8771f681, 11);
    HH(c, d, a, b, w[11], 0x6d9d6122, 16);
    HH(b, c, d, a, w[14], 0xfde5380c, 23);
    HH(a, b, c, d, w[ 1], 0xa4beea44,  4);
    HH(d, a, b, c, w[ 4], 0x4bdecfa9, 11);
    HH(c, d, a, b, w[ 7], 0xf6bb4b60, 16);
    HH(b, c, d, a, w[10], 0xbebfbc70, 23);
    HH(a, b, c, d, w[13], 0x289b7ec6,  4);
    HH(d, a, b, c, w[ 0], 0xeaa127fa, 11);
    HH(c, d, a, b, w[ 3], 0xd4ef3085, 16);
    HH(b, c, d, a, w[ 6], 0x04881d05, 23);
    HH(a, b, c, d, w[ 9], 0xd9d4d039,  4);
    HH(d, a, b, c, w[12], 0xe6db99e5, 11);
    HH(c, d, a, b, w[15], 0x1fa27cf8, 16);
    HH(b, c, d, a, w[ 2], 0xc4ac5665, 23);
    II(a, b, c, d, w[ 0], 0xf4292244,  6);
    II(d, a, b, c, w[ 7], 0x432aff97, 10);
    II(c, d, a, b, w[14], 0xab9423a7, 15);
    II(b, c, d, a, w[ 5], 0xfc93a039, 21);
    II(a, b, c, d, w[12], 0x655b59c3,  6);
    II(d, a, b, c, w[ 3], 0x8f0ccc92, 10);
    II(c, d, a, b, w[10], 0xffeff47d, 15);
    II(b, c, d, a, w[ 1], 0x85845dd1, 21);
    II(a, b, c, d, w[ 8], 0x6fa87e4f,  6);
    II(d, a, b, c, w[15], 0xfe2ce6e0, 10);
    II(c, d, a, b, w[ 6], 0xa3014314, 15);
    II(b, c, d, a, w[13], 0x4e0811a1, 21);
    II(a, b, c, d, w[ 4], 0xf7537e82,  6);
    II(d, a, b, c, w[11], 0xbd3af235, 10);
    II(c, d, a, b, w[ 2], 0x2ad7d2bb, 15);
    II(b, c, d, a, w[ 9], 0xeb86d391, 21);
#undef II
#undef HH
#undef GG
#undef FF

    H[0] += a; H[1] += b; H[2] += c; H[3] += d;
}

Md5::Md5(): mSize(0) {
    static_assert(sizeof(mSize) == LENGTH_BYTES, "");

    mH[0] = 0x67452301;
    mH[1] = 0xefcdab89;
    mH[2] = 0x98badcfe;
    mH[3] = 0x10325476;
}

void Md5::update(const void *_buf, int n) {
    auto buf = (const uint8_t*)_buf;
    assert(buf != nullptr && n > 0);
    assert(mSize != (uint64_t)-1);

    int blockSize = mSize % BLOCK_BYTES;
    if (blockSize + n < BLOCK_BYTES) {
        memcpy(mBuf + blockSize, buf, n);
        mSize += n;
        return;
    } 

    {
        int remain = BLOCK_BYTES - blockSize;
        memcpy(mBuf + blockSize, buf, remain);
        process(mBuf, mH);
        mSize += remain;
        buf += remain;
        n -= remain;
    }

    for (; n >= BLOCK_BYTES; n -= BLOCK_BYTES, buf += BLOCK_BYTES, mSize += BLOCK_BYTES) {
        process(buf, mH);
    }

    if (n > 0) update(buf, n);
}

static const uint8_t PADDING[Md5::BLOCK_BYTES] = {
    0x80, 
};
void Md5::finalize() {
    if (mSize == (uint64_t)-1) return;

    uint8_t sizeBytes[sizeof(mSize)];
    writeUint_littleEndian<uint64_t>(sizeBytes, mSize * 8);

    const int FILL_BYTES = BLOCK_BYTES - LENGTH_BYTES;
    const int blockOff = mSize % BLOCK_BYTES;
    update(PADDING, blockOff < FILL_BYTES ? (FILL_BYTES - blockOff) : (FILL_BYTES + BLOCK_BYTES - blockOff));
    update(sizeBytes, sizeof(sizeBytes));
    mSize = -1;
}

void Md5::digest(uint8_t out[OUTPUT_BYTES]) {
    assert(out != nullptr);
    assert(mSize == (uint64_t)-1);

    for (auto i = 0u; i < ASIZE(mH); ++i) {
        writeUint_littleEndian<uint32_t>(out + i * sizeof(uint32_t), mH[i]);
    }
}

string Md5::digestStr() {
    uint8_t out[OUTPUT_BYTES]; 
    digest(out); 
    return binary2string(out, sizeof(out));
}
