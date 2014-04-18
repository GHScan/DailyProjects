#ifndef MD5_H
#define MD5_H

#include <stdint.h>

#include <string>

class Md5 {
public:
    static const int OUTPUT_BYTES = 16;
    static const int BLOCK_BYTES = 64;
    static const int WORD_BYTES = 4;
    static const int LENGTH_BYTES = 8;

public:
    Md5();
    void update(const void *buf, int n);
    void digest(uint8_t out[OUTPUT_BYTES]); //little endian
    string hexdigest();

private:
    uint32_t mH[4];
    uint8_t mBuf[BLOCK_BYTES];
    uint64_t mSize;
};

#endif
