#include "pch.h"

#include "utils.h"
#include "md5.h"

static uint32_t R[64] = {
    7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22, 
    5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
    4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
    6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,
};
static uint32_t K[64] = {
    0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501, 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be, 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
    0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa, 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8, 0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed, 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
    0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c, 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70, 0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
    0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039, 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1, 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1, 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
};

static inline void process(const uint8_t data[Md5::BLOCK_BYTES], uint32_t H[4]) {
    const int BLOCK_WORDS = Md5::BLOCK_BYTES / Md5::WORD_BYTES;
    uint32_t w[BLOCK_WORDS];
    for (int i = 0; i < BLOCK_WORDS; ++i) {
        w[i] = readUint_littleEndian<uint32_t>(data + i * Md5::WORD_BYTES);
    }

    uint32_t a = H[0], b = H[1], c = H[2], d = H[3], temp;

#define ROTATE() {temp = d; d = c; c = b; b = a; a = temp; }
    for (int i = 0; i < 16; ++i) {
        uint32_t f = (b & c) | ((~b) & d);
        uint32_t g = i;
        a = leftRotate(a + f + K[i] + w[g], R[i]) + b;
        ROTATE();
    }
    for (int i = 16; i < 32; ++i) {
        uint32_t f = (d & b) | ((~d) & c);
        uint32_t g = (5 * i + 1) % 16;
        a = leftRotate(a + f + K[i] + w[g], R[i]) + b;
        ROTATE();
    }
    for (int i = 32; i < 48; ++i) {
        uint32_t f = (b ^ c) ^ d;
        uint32_t g = (3 * i + 5) % 16;
        a = leftRotate(a + f + K[i] + w[g], R[i]) + b;
        ROTATE();
    }
    for (int i = 48; i < 64; ++i) {
        uint32_t f = c ^ (b | (~d));
        uint32_t g = (7 * i) % 16;
        a = leftRotate(a + f + K[i] + w[g], R[i]) + b;
        ROTATE();
    }
#undef ROTATE

    H[0] += a; H[1] += b; H[2] += c; H[3] += d;
}

Md5::Md5(): mSize(0) {
    static_assert(sizeof(mH[0]) == WORD_BYTES, "");
    static_assert(sizeof(mSize) == LENGTH_BYTES, "");

    mH[0] = 0x67452301;
    mH[1] = 0xefcdab89;
    mH[2] = 0x98badcfe;
    mH[3] = 0x10325476;
}

void Md5::update(const void *_buf, int n) {
    auto buf = (const uint8_t*)_buf;
    assert(buf != nullptr && n > 0);

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

void Md5::digest(uint8_t out[OUTPUT_BYTES]) {
    assert(out != nullptr);
    assert(mSize != (uint64_t)-1);

    const int FILL_BYTES = BLOCK_BYTES - LENGTH_BYTES;
    const int blockSize = mSize % BLOCK_BYTES;
    if (blockSize >= FILL_BYTES) {
        mBuf[blockSize] = 0x80;
        for (int i = blockSize + 1; i < BLOCK_BYTES; ++i) mBuf[i] = 0;
        process(mBuf, mH);
        for (int i = 0; i < FILL_BYTES; ++i) mBuf[i] = 0;
    } else {
        mBuf[blockSize] = 0x80;
        for (int i = blockSize + 1; i < FILL_BYTES; ++i) mBuf[i] = 0;
    }

    writeUint_littleEndian<uint64_t>(mBuf + FILL_BYTES, mSize * 8);
    process(mBuf, mH);

    mSize = -1;

    for (int i = 0; i < 4; ++i) {
        writeUint_littleEndian<uint32_t>(out + i * 4, mH[i]);
    }
}

string Md5::hexdigest() {
    uint8_t out[OUTPUT_BYTES]; 
    digest(out); 

    const char *HEX = "0123456789abcdef";

    string s(OUTPUT_BYTES * 2, 0);
    for (int i = 0; i < OUTPUT_BYTES; ++i) {
        s[i * 2 + 0] = HEX[out[i] >> 4];
        s[i * 2 + 1] = HEX[out[i] & 0xf];
    }
    return s;
}
