#ifndef CRC_H
#define CRC_H

#include <stdint.h>

#include <vector>

class Crc32_littleEndian {
public:
    static const uint32_t INIT_POLY = 0xedb88320;
    static const uint32_t INIT_CRC = 0;

    explicit Crc32_littleEndian(uint32_t poly = INIT_POLY);
    uint32_t update(const void *buf, int n, uint32_t crc = INIT_CRC);
private:
    vector<uint32_t> mCrcTable;
};

class Crc32_bigEndian {
public:
    static const uint32_t INIT_POLY = 0xedb88320;
    static const uint32_t INIT_CRC = ~0;

    explicit Crc32_bigEndian(uint32_t poly = INIT_POLY);
    uint32_t update(const void *buf, int n, uint32_t crc = INIT_CRC);
private:
    vector<uint32_t> mCrcTable;
};

typedef Crc32_littleEndian Crc32;

#endif
