#include "pch.h"

#include "utils.h"
#include "sha256.h"

static void process(const uint8_t data[Sha256::BLOCK_BYTES], uint32_t H[Sha256::OUTPUT_BYTES / sizeof(uint32_t)]) {
    const int BLOCK_WORDS = Sha256::BLOCK_BYTES / sizeof(uint32_t);
    const int EXTENDED_BLOCK_WORDS = BLOCK_WORDS + 48;
    uint32_t w[EXTENDED_BLOCK_WORDS];
    for (int i = 0; i < BLOCK_WORDS; ++i) {
        w[i] = readUint_bigEndian<uint32_t>(data + i * sizeof(uint32_t));
    }
    for (int i = BLOCK_WORDS; i < EXTENDED_BLOCK_WORDS; ++i) {
        uint32_t s0 = rightRotate(w[i - 15], 7) ^ rightRotate(w[i - 15], 18) ^ (w[i - 15] >> 3);
        uint32_t s1 = rightRotate(w[i - 2], 17) ^ rightRotate(w[i - 2], 19) ^ (w[i - 2] >> 10);
        w[i] = w[i - 16] + s0 + w[i - 7] + s1;
    }

    uint32_t a = H[0], b = H[1], c = H[2], d = H[3], e = H[4], f = H[5], g = H[6], h = H[7], t;

#define S1(e)           (rightRotate(e, 6) ^ rightRotate(e, 11) ^ rightRotate(e, 25))
#define ch(e, f, g)     ((e & f) ^ ((~e) & g))
#define temp1(e, f, g, h, m, k) (h + k + m + S1(e) + ch(e, f, g))
#define S0(a)           (rightRotate(a, 2) ^ rightRotate(a, 13) ^ rightRotate(a, 22))
#define maj(a, b, c)    ((a & b) ^ (a & c) ^ (b & c))
#define temp2(a, b, c)  (S0(a) + maj(a, b, c))
#define FF(a, b, c, d, e, f, g, h, m, k)    t = temp1(e, f, g, h, m, k); d = d + t; h = t + temp2(a, b, c);
    FF(a, b, c, d, e, f, g, h, w[ 0], 0x428a2f98);
    FF(h, a, b, c, d, e, f, g, w[ 1], 0x71374491);
    FF(g, h, a, b, c, d, e, f, w[ 2], 0xb5c0fbcf);
    FF(f, g, h, a, b, c, d, e, w[ 3], 0xe9b5dba5);
    FF(e, f, g, h, a, b, c, d, w[ 4], 0x3956c25b);
    FF(d, e, f, g, h, a, b, c, w[ 5], 0x59f111f1);
    FF(c, d, e, f, g, h, a, b, w[ 6], 0x923f82a4);
    FF(b, c, d, e, f, g, h, a, w[ 7], 0xab1c5ed5);
    FF(a, b, c, d, e, f, g, h, w[ 8], 0xd807aa98);
    FF(h, a, b, c, d, e, f, g, w[ 9], 0x12835b01);
    FF(g, h, a, b, c, d, e, f, w[10], 0x243185be);
    FF(f, g, h, a, b, c, d, e, w[11], 0x550c7dc3);
    FF(e, f, g, h, a, b, c, d, w[12], 0x72be5d74);
    FF(d, e, f, g, h, a, b, c, w[13], 0x80deb1fe);
    FF(c, d, e, f, g, h, a, b, w[14], 0x9bdc06a7);
    FF(b, c, d, e, f, g, h, a, w[15], 0xc19bf174);
    FF(a, b, c, d, e, f, g, h, w[16], 0xe49b69c1);
    FF(h, a, b, c, d, e, f, g, w[17], 0xefbe4786);
    FF(g, h, a, b, c, d, e, f, w[18], 0x0fc19dc6);
    FF(f, g, h, a, b, c, d, e, w[19], 0x240ca1cc);
    FF(e, f, g, h, a, b, c, d, w[20], 0x2de92c6f);
    FF(d, e, f, g, h, a, b, c, w[21], 0x4a7484aa);
    FF(c, d, e, f, g, h, a, b, w[22], 0x5cb0a9dc);
    FF(b, c, d, e, f, g, h, a, w[23], 0x76f988da);
    FF(a, b, c, d, e, f, g, h, w[24], 0x983e5152);
    FF(h, a, b, c, d, e, f, g, w[25], 0xa831c66d);
    FF(g, h, a, b, c, d, e, f, w[26], 0xb00327c8);
    FF(f, g, h, a, b, c, d, e, w[27], 0xbf597fc7);
    FF(e, f, g, h, a, b, c, d, w[28], 0xc6e00bf3);
    FF(d, e, f, g, h, a, b, c, w[29], 0xd5a79147);
    FF(c, d, e, f, g, h, a, b, w[30], 0x06ca6351);
    FF(b, c, d, e, f, g, h, a, w[31], 0x14292967);
    FF(a, b, c, d, e, f, g, h, w[32], 0x27b70a85);
    FF(h, a, b, c, d, e, f, g, w[33], 0x2e1b2138);
    FF(g, h, a, b, c, d, e, f, w[34], 0x4d2c6dfc);
    FF(f, g, h, a, b, c, d, e, w[35], 0x53380d13);
    FF(e, f, g, h, a, b, c, d, w[36], 0x650a7354);
    FF(d, e, f, g, h, a, b, c, w[37], 0x766a0abb);
    FF(c, d, e, f, g, h, a, b, w[38], 0x81c2c92e);
    FF(b, c, d, e, f, g, h, a, w[39], 0x92722c85);
    FF(a, b, c, d, e, f, g, h, w[40], 0xa2bfe8a1);
    FF(h, a, b, c, d, e, f, g, w[41], 0xa81a664b);
    FF(g, h, a, b, c, d, e, f, w[42], 0xc24b8b70);
    FF(f, g, h, a, b, c, d, e, w[43], 0xc76c51a3);
    FF(e, f, g, h, a, b, c, d, w[44], 0xd192e819);
    FF(d, e, f, g, h, a, b, c, w[45], 0xd6990624);
    FF(c, d, e, f, g, h, a, b, w[46], 0xf40e3585);
    FF(b, c, d, e, f, g, h, a, w[47], 0x106aa070);
    FF(a, b, c, d, e, f, g, h, w[48], 0x19a4c116);
    FF(h, a, b, c, d, e, f, g, w[49], 0x1e376c08);
    FF(g, h, a, b, c, d, e, f, w[50], 0x2748774c);
    FF(f, g, h, a, b, c, d, e, w[51], 0x34b0bcb5);
    FF(e, f, g, h, a, b, c, d, w[52], 0x391c0cb3);
    FF(d, e, f, g, h, a, b, c, w[53], 0x4ed8aa4a);
    FF(c, d, e, f, g, h, a, b, w[54], 0x5b9cca4f);
    FF(b, c, d, e, f, g, h, a, w[55], 0x682e6ff3);
    FF(a, b, c, d, e, f, g, h, w[56], 0x748f82ee);
    FF(h, a, b, c, d, e, f, g, w[57], 0x78a5636f);
    FF(g, h, a, b, c, d, e, f, w[58], 0x84c87814);
    FF(f, g, h, a, b, c, d, e, w[59], 0x8cc70208);
    FF(e, f, g, h, a, b, c, d, w[60], 0x90befffa);
    FF(d, e, f, g, h, a, b, c, w[61], 0xa4506ceb);
    FF(c, d, e, f, g, h, a, b, w[62], 0xbef9a3f7);
    FF(b, c, d, e, f, g, h, a, w[63], 0xc67178f2);
#undef FF
#undef temp2
#undef maj
#undef S0
#undef temp1
#undef ch
#undef S1

    H[0] += a; H[1] += b; H[2] += c; H[3] += d; H[4] += e; H[5] += f; H[6] += g; H[7] += h;
}

Sha256::Sha256(): mSize(0) {
    static_assert(sizeof(mSize) == LENGTH_BYTES, "");

    mH[0] = 0x6a09e667;
    mH[1] = 0xbb67ae85;
    mH[2] = 0x3c6ef372;
    mH[3] = 0xa54ff53a;
    mH[4] = 0x510e527f;
    mH[5] = 0x9b05688c;
    mH[6] = 0x1f83d9ab;
    mH[7] = 0x5be0cd19;
}

void Sha256::update(const void *_buf, int n) {
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

void Sha256::finalize() {
    if (mSize == (uint64_t)-1) return;

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

    writeUint_bigEndian<uint64_t>(mBuf + FILL_BYTES, mSize * 8);
    process(mBuf, mH);

    mSize = -1;
}

void Sha256::digest(uint8_t out[OUTPUT_BYTES]) {
    assert(out != nullptr);
    assert(mSize == (uint64_t)-1);

    for (auto i = 0u; i < ASIZE(mH); ++i) {
        writeUint_bigEndian<uint32_t>(out + i * sizeof(uint32_t), mH[i]);
    }
}

string Sha256::digestStr() {
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
