#include "pch.h"

#include "stackAllocator.h"
#include "stream.h"
#include "bitStream.h"
#include "lzw.h"

static int estimateMaxLZWCompressedSize(int size) {
    return (size * 10) / 8;
}

static uint32_t readCode(InputBitStream *bs) {
    if (bs->readBool()) return bs->readInt<uint8_t>();
    else {
        if (bs->readBool()) return bs->readInt<uint16_t>();
        else {
            if (bs->readBool()) {
                auto lo = bs->readInt<uint16_t>();
                return lo | (uint32_t(bs->readInt<uint8_t>()) << 16);
            }
            else return bs->readInt<uint32_t>();
        }
    }
}
static void writeCode(OutputBitStream *bs, uint32_t code) {
    if (code < (1u << 8)) {
        bs->writeBool(true);
        bs->writeInt<uint8_t>(code);
    } else {
        bs->writeBool(false);
        if (code < (1u << 16)) {
            bs->writeBool(true);
            bs->writeInt<uint16_t>(code);
        } else {
            bs->writeBool(false);
            if (code < (1u << 24)) {
                bs->writeBool(true);
                bs->writeInt<uint16_t>(code);
                bs->writeInt<uint8_t>(code >> 16);
            } else {
                bs->writeBool(false);
                bs->writeInt<uint32_t>(code);
            }
        }
    }
}

class LZWChunkCompressor {
public:
    LZWChunkCompressor(int chunkSizeHint): mBucketMask(pow2Roundup(min(chunkSizeHint, 128 * 1024)) - 1), mAllocator(mBucketMask + 1) {
        assert((mBucketMask & (mBucketMask + 1)) == 0);
        mBuckets = (Ref**)::calloc(mBucketMask + 1, sizeof(mBuckets[0]));
    }
    ~LZWChunkCompressor() {
        ::free(mBuckets);
    }
    LZWChunkCompressor(const LZWChunkCompressor&) = delete;
    LZWChunkCompressor& operator = (const LZWChunkCompressor&) = delete;

    int compress(const uint8_t *chunk, int chunkSize, uint8_t *dest, int destsize) {
        OutputBitStream bs(dest, destsize * 8);

        int code = 256;
        for (int i = 0; i < chunkSize;) {
            int longestCode = chunk[i];
            Ref cur(code++, chunk + i++);
            cur.extend();

            for (; i < chunkSize; ++i) {
                cur.extend();
                Ref *p = mBuckets[cur.hash & mBucketMask];
                for (; p != nullptr && !cur.equal(p); p = p->next);
                if (p == nullptr) break;
                longestCode = p->code;
            }

            if (i < chunkSize) {
                Ref *newRef = mAllocator.malloc();
                *newRef = cur;
                Ref **p = &mBuckets[cur.hash & mBucketMask];
                newRef->next = *p;
                *p = newRef;
            }

            writeCode(&bs, longestCode);
        }

        memset(mBuckets, 0, (mBucketMask + 1) * sizeof(mBuckets[0]));
        mAllocator.reset();

        return (bs.pos() + 7) / 8;
    }

public:
    struct Ref {
        Ref *next;
        const uint8_t *data;
        uint32_t size;
        uint32_t code;
        uint32_t hash;
        explicit Ref(uint32_t _code, const uint8_t *_data): data(_data), size(0), code(_code), hash(2166136261u) {}
        bool equal(const Ref *r) {
            assert(size >= 2 && r->size >= 2);
            return hash == r->hash && size == r->size && memcmp(data, r->data, size) == 0;
        }
        void extend() {
            hash = (hash ^ data[size++]) * 16777619u;
        }
    };
private:
    Ref **mBuckets;
    int mBucketMask;
    TStackAllocator<Ref> mAllocator;
};

class LZWChunkUncompressor {
public:
    LZWChunkUncompressor(): mChars(256, 0) {
        for (int i = 0; i < 256; ++i) {
            mChars[i] = i;
            mCode2Ref.push_back(Ref{(uint8_t*)&mChars[i], 1});
        }
    }
    void uncompress(const uint8_t *chunk, int chunkSize, uint8_t *dest, int destsize) {
        assert(destsize > 0);
        InputBitStream bs(chunk, chunkSize * 8);

        mCode2Ref.resize(256);
        mCode2Ref.reserve(destsize * sizeof(Ref));

        uint32_t code = readCode(&bs);
        for (int i = 0; i < destsize; ) {
            assert(code < mCode2Ref.size());
            auto& prefix = mCode2Ref[code];
            Ref cur = {dest + i, prefix.size + 1};

            if (code == mCode2Ref.size() - 1) {
                for (int i = 0; i < prefix.size; ++i) {
                    cur.data[i] = prefix.data[i];
                }
            } else {
                memcpy(cur.data, prefix.data, prefix.size);
            }
            i += prefix.size;

            if (i < destsize) {
                code = readCode(&bs);
                mCode2Ref.push_back(cur);
            }
        }
    }

private:
    struct Ref {
        uint8_t *data;
        int size;
    };
private:
    vector<Ref> mCode2Ref;
    string mChars;
};

LzwCompressor::LzwCompressor(int chunkSize): mChunkSize(chunkSize) {
}

void LzwCompressor::compress(IInputStream *si, IOutputStream *so) {
    vector<uint8_t> readBuf(mChunkSize), writeBuf(estimateMaxLZWCompressedSize(mChunkSize) + 8);
    LZWChunkCompressor compressor(mChunkSize);

    for (int i = 0, size = si->size(); i < size; i += mChunkSize) {
        int chunkSize = min(size - i, mChunkSize);
        if (si->read(&readBuf[0], chunkSize) != chunkSize) assert(0);
        uint32_t *sizes = (uint32_t*)&writeBuf[0];
        sizes[0] = chunkSize;
        sizes[1] = compressor.compress(&readBuf[0], chunkSize, &writeBuf[0] + 8, (int)writeBuf.size() - 8);

        if (so->write(&writeBuf[0], sizes[1] + 8) != (int)sizes[1] + 8) assert(0);
    }
}

void LzwCompressor::uncompress(IInputStream *si, IOutputStream *so) {
    vector<uint8_t> readBuf, writeBuf;
    LZWChunkUncompressor uncompressor;

    for (int i = 0, size = si->size(); i < size; ) {
        uint32_t sizes[2];
        if (si->read(sizes, sizeof(sizes)) != sizeof(sizes)) assert(0);
        readBuf.resize(sizes[1]);
        writeBuf.resize(sizes[0]);

        if (si->read(&readBuf[0], (int)readBuf.size()) != (int)readBuf.size()) assert(0);
        uncompressor.uncompress(&readBuf[0], (int)readBuf.size(), &writeBuf[0], (int)writeBuf.size());

        if (so->write(&writeBuf[0], (int)writeBuf.size()) != (int)writeBuf.size()) assert(0);
        i += (int)readBuf.size() + 8;
    }
}
