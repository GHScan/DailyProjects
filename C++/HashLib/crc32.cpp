#include "pch.h"
#include "crc32.h"

static const int CRC_TABLE_SIZE = 256;

static void dumpCrcTable(const uint32_t table[CRC_TABLE_SIZE]) {
    for (int i = 0; i < CRC_TABLE_SIZE; ++i) {
        if (i % 4 == 0) puts("");
        printf("%08x, ", table[i]);
    }
    fflush(stdout);
}

Crc32_littleEndian::Crc32_littleEndian(uint32_t poly) {
    mCrcTable.resize(CRC_TABLE_SIZE);
    for (int i = 0; i < CRC_TABLE_SIZE; ++i) {
        uint32_t crc = i;
        for (int j = 0; j < 8; ++j) {
            if (crc & 1) crc = (crc >> 1) ^ poly;
            else crc = (crc >> 1);
        }
        mCrcTable[i] = crc;
    }
    (void)dumpCrcTable;
}

uint32_t Crc32_littleEndian::update(const void *_buf, int n, uint32_t crc) {
    auto buf = (const uint8_t *)_buf;
    assert(buf != nullptr && n > 0);

    crc = ~crc;
    for (int i = 0; i < n; ++i) {
        crc = (crc >> 8) ^ mCrcTable[(crc ^ buf[i]) & 0xff];
    }

    return ~crc;
}

Crc32_bigEndian::Crc32_bigEndian(uint32_t poly) {
    mCrcTable.resize(CRC_TABLE_SIZE);

    for (int i = 0; i < CRC_TABLE_SIZE; ++i) {
        uint32_t crc = i << 24;
        for (int j = 0; j < 8; ++j) {
            if (crc & 0x80000000) crc = (crc << 1) ^ poly;
            else crc = (crc << 1);
        }
        mCrcTable[i] = crc;
    }
}

uint32_t Crc32_bigEndian::update(const void *_buf, int n, uint32_t crc) {
    auto buf = (const uint8_t *)_buf;
    assert(buf != nullptr && n > 0);

    crc = ~crc;
    for (int i = 0; i < n; ++i){ 
        crc = (crc << 8) ^ mCrcTable[(crc >> 24) ^ buf[i]];
    }

    return ~crc;
}
