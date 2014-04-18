#include "pch.h"

#include "utils.h"
#include "sha1.h"

static void process(const uint8_t data[Sha1::BLOCK_BYTES], uint32_t H[Sha1::OUTPUT_BYTES / sizeof(uint32_t)]) {
    const int BLOCK_WORDS = Sha1::BLOCK_BYTES / sizeof(uint32_t);
    const int EXTENDED_BLOCK_WORDS = BLOCK_WORDS + 64;
    uint32_t w[EXTENDED_BLOCK_WORDS];
    for (int i = 0; i < BLOCK_WORDS; ++i) {
        w[i] = readUint_bigEndian<uint32_t>(data + i * sizeof(uint32_t));
    }
    for (int i = BLOCK_WORDS; i < EXTENDED_BLOCK_WORDS; ++i) {
        w[i] = leftRotate(w[i - 3] ^ w[i - 8] ^ w[i - 14] ^ w[i - 16], 1);
    }

    uint32_t a = H[0], b = H[1], c = H[2], d = H[3], e = H[4];

#define FF(a, b, c, d, e, m)   e = m + 0x5a827999 + e + leftRotate(a, 5) + ((b & c) | ((~b) & d)); b = leftRotate(b, 30);
#define GG(a, b, c, d, e, m)   e = m + 0x6ed9eba1 + e + leftRotate(a, 5) + (b ^ c ^ d); b = leftRotate(b, 30); 
#define HH(a, b, c, d, e, m)   e = m + 0x8f1bbcdc + e + leftRotate(a, 5) + ((b & c) | (b & d) | (c & d)); b = leftRotate(b, 30); 
#define II(a, b, c, d, e, m)   e = m + 0xca62c1d6 + e + leftRotate(a, 5) + (b ^ c ^ d); b = leftRotate(b, 30); 
    FF(a, b, c, d, e, w[ 0]);
    FF(e, a, b, c, d, w[ 1]);
    FF(d, e, a, b, c, w[ 2]);
    FF(c, d, e, a, b, w[ 3]);
    FF(b, c, d, e, a, w[ 4]);
    FF(a, b, c, d, e, w[ 5]);
    FF(e, a, b, c, d, w[ 6]);
    FF(d, e, a, b, c, w[ 7]);
    FF(c, d, e, a, b, w[ 8]);
    FF(b, c, d, e, a, w[ 9]);
    FF(a, b, c, d, e, w[10]);
    FF(e, a, b, c, d, w[11]);
    FF(d, e, a, b, c, w[12]);
    FF(c, d, e, a, b, w[13]);
    FF(b, c, d, e, a, w[14]);
    FF(a, b, c, d, e, w[15]);
    FF(e, a, b, c, d, w[16]);
    FF(d, e, a, b, c, w[17]);
    FF(c, d, e, a, b, w[18]);
    FF(b, c, d, e, a, w[19]);
    GG(a, b, c, d, e, w[20]);
    GG(e, a, b, c, d, w[21]);
    GG(d, e, a, b, c, w[22]);
    GG(c, d, e, a, b, w[23]);
    GG(b, c, d, e, a, w[24]);
    GG(a, b, c, d, e, w[25]);
    GG(e, a, b, c, d, w[26]);
    GG(d, e, a, b, c, w[27]);
    GG(c, d, e, a, b, w[28]);
    GG(b, c, d, e, a, w[29]);
    GG(a, b, c, d, e, w[30]);
    GG(e, a, b, c, d, w[31]);
    GG(d, e, a, b, c, w[32]);
    GG(c, d, e, a, b, w[33]);
    GG(b, c, d, e, a, w[34]);
    GG(a, b, c, d, e, w[35]);
    GG(e, a, b, c, d, w[36]);
    GG(d, e, a, b, c, w[37]);
    GG(c, d, e, a, b, w[38]);
    GG(b, c, d, e, a, w[39]);
    HH(a, b, c, d, e, w[40]);
    HH(e, a, b, c, d, w[41]);
    HH(d, e, a, b, c, w[42]);
    HH(c, d, e, a, b, w[43]);
    HH(b, c, d, e, a, w[44]);
    HH(a, b, c, d, e, w[45]);
    HH(e, a, b, c, d, w[46]);
    HH(d, e, a, b, c, w[47]);
    HH(c, d, e, a, b, w[48]);
    HH(b, c, d, e, a, w[49]);
    HH(a, b, c, d, e, w[50]);
    HH(e, a, b, c, d, w[51]);
    HH(d, e, a, b, c, w[52]);
    HH(c, d, e, a, b, w[53]);
    HH(b, c, d, e, a, w[54]);
    HH(a, b, c, d, e, w[55]);
    HH(e, a, b, c, d, w[56]);
    HH(d, e, a, b, c, w[57]);
    HH(c, d, e, a, b, w[58]);
    HH(b, c, d, e, a, w[59]);
    II(a, b, c, d, e, w[60]);
    II(e, a, b, c, d, w[61]);
    II(d, e, a, b, c, w[62]);
    II(c, d, e, a, b, w[63]);
    II(b, c, d, e, a, w[64]);
    II(a, b, c, d, e, w[65]);
    II(e, a, b, c, d, w[66]);
    II(d, e, a, b, c, w[67]);
    II(c, d, e, a, b, w[68]);
    II(b, c, d, e, a, w[69]);
    II(a, b, c, d, e, w[70]);
    II(e, a, b, c, d, w[71]);
    II(d, e, a, b, c, w[72]);
    II(c, d, e, a, b, w[73]);
    II(b, c, d, e, a, w[74]);
    II(a, b, c, d, e, w[75]);
    II(e, a, b, c, d, w[76]);
    II(d, e, a, b, c, w[77]);
    II(c, d, e, a, b, w[78]);
    II(b, c, d, e, a, w[79]);
#undef II
#undef HH
#undef GG
#undef FF

    H[0] += a; H[1] += b; H[2] += c; H[3] += d; H[4] += e;
}

Sha1::Sha1(): mSize(0) {
    static_assert(sizeof(mSize) == LENGTH_BYTES, "");

    mH[0] = 0x67452301;
    mH[1] = 0xefcdab89;
    mH[2] = 0x98badcfe;
    mH[3] = 0x10325476;
    mH[4] = 0xc3d2e1f0;
}

void Sha1::update(const void *_buf, int n) {
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

static const uint8_t PADDING[Sha1::BLOCK_BYTES] = {
    0x80, 
};
void Sha1::finalize() {
    if (mSize == (uint64_t)-1) return;

    uint8_t sizeBytes[sizeof(mSize)];
    writeUint_bigEndian<uint64_t>(sizeBytes, mSize * 8);

    const int FILL_BYTES = BLOCK_BYTES - LENGTH_BYTES;
    const int blockOff = mSize % BLOCK_BYTES;
    update(PADDING, blockOff < FILL_BYTES ? (FILL_BYTES - blockOff) : (FILL_BYTES + BLOCK_BYTES - blockOff));
    update(sizeBytes, sizeof(sizeBytes));
    mSize = -1;
}

void Sha1::digest(uint8_t out[OUTPUT_BYTES]) {
    assert(out != nullptr);
    assert(mSize == (uint64_t)-1);

    for (auto i = 0u; i < ASIZE(mH); ++i) {
        writeUint_bigEndian<uint32_t>(out + i * sizeof(uint32_t), mH[i]);
    }
}

string Sha1::digestStr() {
    uint8_t out[OUTPUT_BYTES]; 
    digest(out); 
    return binary2string(out, sizeof(out));
}
