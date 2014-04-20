#include "pch.h"

#include <limits>

#include "rle.h"
#include "stream.h"
#include "utils.h"

template<typename T>
struct CompressTraits;
template<>
struct CompressTraits<uint8_t> {
    static const uint8_t MAGIC = 0;
};
template<>
struct CompressTraits<uint16_t> {
    static const uint16_t MAGIC = 0x7309;
};
template<>
struct CompressTraits<uint32_t> {
    static const uint32_t MAGIC = 0xfa217309;
};
template<>
struct CompressTraits<uint64_t> {
    static const uint64_t MAGIC = 0xd78e00fc9b3e7309LU;
};

template<typename T>
static void compressT(IInputStream *si, IOutputStream *so) {
    const int BUF_SIZE = 4 * 1024;
    vector<T> readBuf(BUF_SIZE), writeBuf;
    writeBuf.reserve(BUF_SIZE);
    int readBytes, writeBytes;
    (void)readBytes, (void)writeBytes;

    T MAGIC = CompressTraits<T>::MAGIC;
    for (; (readBytes = si->read(&readBuf[0], int(readBuf.size() * sizeof(T)))) > 0; ) {
        assert(readBytes % sizeof(T) == 0);
        int n = readBytes / sizeof(T);

        for (int i = 0; i < n; ) {
            int start = i;
            for (; i < n && readBuf[i] == readBuf[start]; ++i);
            if (readBuf[start] != MAGIC && i - start < 3) {
                for (; start < i; ++start) writeBuf.push_back(readBuf[start]);
            } else {
                for (; start < i; ) {
                    T repeat = uint64_t(i - start) <= T(-1) ? T(i - start) : T(-1);
                    writeBuf.push_back(MAGIC);
                    writeBuf.push_back(readBuf[start]);
                    writeBuf.push_back(repeat);
                    start += int(repeat);
                }
            }
        }

        writeBytes = so->write(&writeBuf[0], int(writeBuf.size() * sizeof(T)));
        assert(writeBytes == int(writeBuf.size() * sizeof(T)));
        writeBuf.clear();
    }
}
template<typename T>
static void uncompressT(IInputStream *si, IOutputStream *so) {
    const int BUF_SIZE = 4 * 1024;
    vector<T> readBuf(BUF_SIZE), writeBuf;
    writeBuf.reserve(BUF_SIZE);
    int readBytes, writeBytes;
    (void)readBytes, (void)writeBytes;

    T MAGIC = CompressTraits<T>::MAGIC;
    for (; (readBytes = si->read(&readBuf[0], int(readBuf.size() * sizeof(T)))) > 0; ) {
        assert(readBytes % sizeof(T) == 0);
        int n = readBytes / sizeof(T);

        for (int i = 0; i < n; ) {
            if (readBuf[i] != MAGIC) {
                writeBuf.push_back(readBuf[i++]);
            } else {
                if (n - i < 3) {
                    int more = 3 - (n - i);
                    readBuf.resize(n + more);
                    readBytes = si->read(&readBuf[n], more * sizeof(T));
                    assert(readBytes == more * sizeof(T));
                    n += more;
                }

                writeBuf.resize(writeBuf.size() + readBuf[i + 2], readBuf[i + 1]);
                i += 3;
            }
        }

        writeBytes = so->write(&writeBuf[0], int(writeBuf.size() * sizeof(T)));
        assert(writeBytes == int(writeBuf.size() * sizeof(T)));
        writeBuf.clear();
    }
}

RleCompressor::RleCompressor(int blockSize): mBlockSize(blockSize) {
    switch (blockSize) {
        case 1: case 2: case 4: case 8: break;
        default: assert(0 && "Invalid rle block size"); break;
    }
}

void RleCompressor::compress(IInputStream *si, IOutputStream *so) {
    int size = si->size();
    so->write(toLittleEndian<uint32_t>(size));
    if (size % mBlockSize) {
        char buf[32];
        int remain = size % mBlockSize;
        if (si->read(buf, remain) != remain) assert(0);
        if (so->write(buf, remain) != remain) assert(0);
    }

    switch (mBlockSize) {
        case 1: compressT<uint8_t>(si, so); break;
        case 2: compressT<uint16_t>(si, so); break;
        case 4: compressT<uint32_t>(si, so); break;
        case 8: compressT<uint64_t>(si, so); break;
        default: assert(0); break;
    }
}

void RleCompressor::uncompress(IInputStream *si, IOutputStream *so) { 
    auto size = si->read<uint32_t>();
    size = fromLittleEndian(size);
    if (size % mBlockSize) {
        char buf[32];
        int remain = size % mBlockSize;
        if (si->read(buf, remain) != remain) assert(0);
        if (so->write(buf, remain) != remain) assert(0);
    }

    switch (mBlockSize) {
        case 1: uncompressT<uint8_t>(si, so); break;
        case 2: uncompressT<uint16_t>(si, so); break;
        case 4: uncompressT<uint32_t>(si, so); break;
        case 8: uncompressT<uint64_t>(si, so); break;
        default: assert(0); break;
    }
}
