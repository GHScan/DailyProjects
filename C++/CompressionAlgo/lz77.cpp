#include "pch.h"

#include "bitStream.h"
#include "stream.h"
#include "stackAllocator.h"
#include "lz77.h"

typedef uint32_t DuplicateCheckType;
static const int MIN_DUPLICATE_SIZE = sizeof(DuplicateCheckType);

static int estimateLz77MaxCompressedSize(int size) {
    return (size * 10)  / 8;
}

static void writeLiteral(OutputBitStream *bs, uint8_t byte) {
    bs->writeBool(true);
    bs->writeInt(byte);
}
static void writeRef(OutputBitStream *bs, int dist, int size, const uint8_t *windowEnd) {
    assert(size >= MIN_DUPLICATE_SIZE);

    const uint8_t *data = windowEnd - dist;
    if (dist < 256 && size < 256) {
        if (size * 9 < 2 * 8 + 3) {
            for (int i = 0; i < size; ++i) writeLiteral(bs, data[i]);
        } else {
            bs->writeBool(false); bs->writeBool(false); bs->writeBool(false);
            bs->writeInt<uint8_t>(dist); bs->writeInt<uint8_t>(size);
        }
    } else if (dist < 256 * 256 && size < 256) {
        if (size * 9 < 3 * 8 + 3) {
            for (int i = 0; i < size; ++i) writeLiteral(bs, data[i]);
        } else {
            bs->writeBool(false); bs->writeBool(true); bs->writeBool(false);
            bs->writeInt<uint16_t>(dist); bs->writeInt<uint8_t>(size);
        }
    } else if (dist < 256 * 256 * 256 && size < 256 * 256) {
        if (size * 9 < 5 * 8 + 3) {
            for (int i = 0; i < size; ++i) writeLiteral(bs, data[i]);
        } else {
            bs->writeBool(false); bs->writeBool(false); bs->writeBool(true);
            bs->writeInt<uint16_t>(dist); bs->writeInt<uint8_t>(dist >> 16);
            bs->writeInt<uint16_t>(size);
        }
    } else {
        if (size * 9 < 8 * 8 + 3) {
            for (int i = 0; i < size; ++i) writeLiteral(bs, data[i]);
        } else {
            bs->writeBool(false); bs->writeBool(true); bs->writeBool(true);
            bs->writeInt<uint32_t>(dist); bs->writeInt<uint32_t>(size);
        }
    }
}
static int readLiteralOrRef(InputBitStream *bs, uint8_t* windowEnd) {
    if (bs->readBool()) {
        windowEnd[0] = bs->readInt<uint8_t>();
        return 1;
    } else {
        int dist, size;
        if (bs->readBool()) {
            if (bs->readBool()) {
                dist = bs->readInt<uint32_t>(); size = bs->readInt<uint32_t>();
            } else {
                dist = bs->readInt<uint16_t>(); size = bs->readInt<uint8_t>();
            }
        } else {
            if (bs->readBool()) {
                dist = bs->readInt<uint16_t>(); dist |= bs->readInt<uint8_t>() << 16;
                size = bs->readInt<uint16_t>();
            } else {
                dist = bs->readInt<uint8_t>(); size = bs->readInt<uint8_t>();
            }
        }
        assert(size >= MIN_DUPLICATE_SIZE);

        const uint8_t *src = windowEnd - dist;
        if (dist >= size) {
            memcpy(windowEnd, src, size);
        } else {
            for (int i = 0; i < size; ++i) windowEnd[i] = src[i];
        }

        return size;
    }
}

class Lz77ChunkCompressor {
public:
    Lz77ChunkCompressor(int chunkSize, int windowSize): mBucketMask(pow2Roundup(chunkSize) - 1), mWindowSize(windowSize), mAllocator(mBucketMask + 1) {
        assert(((mBucketMask + 1) & mBucketMask) == 0);

        mBuckets = (Ref**)::malloc((mBucketMask + 1) * sizeof(mBuckets[0]));
        mInvalidRef = Ref{nullptr, nullptr};
    }
    ~Lz77ChunkCompressor() {
        ::free(mBuckets);
    }
    Lz77ChunkCompressor(const Lz77ChunkCompressor&) = delete;
    Lz77ChunkCompressor& operator = (const Lz77ChunkCompressor&) = delete;

    int compress(const uint8_t *chunk, int initWinSize, int chunkSize, uint8_t *dest, int destsize) {
        setupWithInitWindow(chunk, initWinSize);

        OutputBitStream bs(dest, destsize * 8);

        auto windowEnd = chunk + initWinSize;
        auto chunkEnd = windowEnd + chunkSize;
        for (; windowEnd < chunkEnd; ) {
            auto window = max(chunk, windowEnd - mWindowSize);

            int size = 1;
            Ref *match = findLongestMatch(windowEnd, chunkEnd, window, &size);

            if (match == nullptr) {
                writeLiteral(&bs, windowEnd[0]);
            } else {
                writeRef(&bs, windowEnd - match->data, size, windowEnd);
            }

            for (int i = 0; i < size; ++i) {
                addRef(++windowEnd - MIN_DUPLICATE_SIZE, window);
            }
        }

        return (bs.pos() + 7) / 8;
    }
private:
    struct Ref {
        Ref *next;
        const uint8_t *data;
    };
private:
    void setupWithInitWindow(const uint8_t *window, int winSize) {
        mAllocator.reset();
        for (int i = 0; i <= mBucketMask; ++i) {
            mBuckets[i] = &mInvalidRef;
        }

        for (int i = 0; i < winSize - MIN_DUPLICATE_SIZE; ++i) {
            addRef(window + i, window);
        }
    }
    Ref* findLongestMatch(const uint8_t *begin, const uint8_t *end, const uint8_t *window, int *size) {
        if (end - begin < MIN_DUPLICATE_SIZE) return nullptr;

        Ref *p = mBuckets[mHash(*(DuplicateCheckType*)begin) & mBucketMask];
        for (; p->data >= window && *(DuplicateCheckType*)p->data != *(DuplicateCheckType*)begin; p = p->next);
        if (p->data < window) return nullptr;

        int longest = MIN_DUPLICATE_SIZE;
        Ref *longestRef = p;

        for (; p->data >= window && begin + longest < end; p = p->next) {
            if (((DuplicateCheckType*)(p->data + longest))[-1] != ((DuplicateCheckType*)(begin + longest))[-1]) continue;
            if (memcmp(p->data, begin, longest) != 0) continue;
            if (p->data[longest] != begin[longest]) continue;
            longestRef = p;
            for (; begin + longest < end && p->data[longest] == begin[longest]; ++longest);
        }

        *size = longest;
        return longestRef;
    }
    void addRef(const uint8_t *data, const uint8_t *window) {
        if (data < window) return;

        Ref **p = &mBuckets[mHash(*(DuplicateCheckType*)data) & mBucketMask];
        Ref *ref = mAllocator.malloc();
        ref->data = data;
        ref->next = *p;
        *p = ref;
    }

private:
    Ref **mBuckets;
    Ref mInvalidRef;
    int mBucketMask;
    int mWindowSize;
    TStackAllocator<Ref> mAllocator;
    hash<DuplicateCheckType> mHash;
};

class Lz77ChunkUncompressor {
public:
    void uncompress(const uint8_t *chunk, int chunkSize, uint8_t *dest, int initWinSize, int destsize) {
        uint8_t *windowEnd = dest + initWinSize;
        uint8_t *destEnd = windowEnd + destsize;

        InputBitStream bs(chunk, chunkSize * 8);
        while (windowEnd < destEnd) {
            windowEnd += readLiteralOrRef(&bs, windowEnd);
        }
    }
};

Lz77Compressor::Lz77Compressor(int chunkSize, int windowSize): mChunkSize(chunkSize), mWindowSize(windowSize) {
    assert(chunkSize >= windowSize);
}

void Lz77Compressor::compress(IInputStream *si, IOutputStream *so) {
    vector<uint8_t> readBuf(mChunkSize + mWindowSize), writeBuf(estimateLz77MaxCompressedSize(mChunkSize) + 8);
    Lz77ChunkCompressor compressor(mChunkSize, mWindowSize);

    int initWinSize = 0;
    for (int i = 0, size = si->size(); i < size; i += mChunkSize) {
        int chunkSize = min(size - i, mChunkSize);
        readBuf.resize(initWinSize + chunkSize);
        if (si->read(&readBuf[initWinSize], chunkSize) != chunkSize) assert(0);

        uint32_t *sizes = (uint32_t*)&writeBuf[0];
        sizes[0] = chunkSize;
        sizes[1] = compressor.compress(&readBuf[0], initWinSize, chunkSize, &writeBuf[0] + 8, (int)writeBuf.size() - 8);

        if (so->write(&writeBuf[0], sizes[1] + 8) != (int)sizes[1] + 8) assert(0);

        initWinSize = min(mWindowSize, (int)readBuf.size());
        memmove(&readBuf[0], &readBuf[0] + readBuf.size() - initWinSize, initWinSize);
    }
}

void Lz77Compressor::uncompress(IInputStream *si, IOutputStream *so) {
    vector<uint8_t> readBuf, writeBuf;
    Lz77ChunkUncompressor uncompressor;

    int initWinSize = 0;
    for (int i = 0, size = si->size(); i < size; ) {
        uint32_t sizes[2];
        if (si->read(sizes, sizeof(sizes)) != sizeof(sizes)) assert(0);
        readBuf.resize(sizes[1]);
        writeBuf.resize(initWinSize + sizes[0]);

        if (si->read(&readBuf[0], (int)readBuf.size()) != (int)readBuf.size()) assert(0);
        uncompressor.uncompress(&readBuf[0], (int)readBuf.size(), &writeBuf[0], initWinSize, sizes[0]);
        if (so->write(&writeBuf[initWinSize], sizes[0]) != (int)sizes[0]) assert(0);

        initWinSize = min(mWindowSize, (int)writeBuf.size());
        memmove(&writeBuf[0], &writeBuf[0] + writeBuf.size() - initWinSize, initWinSize);
        i += sizes[1] + 8;
    }
}
