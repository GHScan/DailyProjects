#include "pch.h"

#include <stdint.h>

#include <random>
#include <memory>
#include <type_traits>
#include <unordered_set>

//------------------------------
class BitVector {
private:
    typedef uint32_t ElemT;
    static const int ELEM_BIT_COUNT = sizeof(ElemT) * 8;
public:
    BitVector(int size): mElems((size + ELEM_BIT_COUNT - 1) / ELEM_BIT_COUNT, 0), mSize(size) { }
    int size() const { return mSize; }
    void set(int i, bool b) {
        uint32_t &v = mElems[i / ELEM_BIT_COUNT];
        v &= ~(1 << (i % ELEM_BIT_COUNT));
        v |= (b ? 1 : 0) << (i % ELEM_BIT_COUNT);
    }
    bool get(int i) const {
        return ((mElems[i / ELEM_BIT_COUNT] >> (i % ELEM_BIT_COUNT)) & 1) == 1;
    }
    int getMemoryUsage() const { return mElems.size() * sizeof(mElems[0]); }
private: 
    vector<ElemT> mElems;
    int mSize;
};

//------------------------------
class Hasher_Jenkins {
public:
    Hasher_Jenkins(): mSeed(-1) {}
    Hasher_Jenkins(uint32_t seed): mSeed(seed) { }
    uint32_t operator () (const void *_buf, int len) const {
        assert(mSeed != (uint32_t)-1);
        assert(_buf != 0 && len > 0);

        uint8_t *buf = (uint8_t*)_buf;
        uint32_t hash = mSeed;
        for (int i = 0; i < len; ++i) {
            hash += buf[i];
            hash += (hash << 10);
            hash ^= (hash >> 6);
        }
        hash += (hash << 3);
        hash ^= (hash >> 11);
        hash += (hash << 15);
        return hash;
    }
private:
    uint32_t mSeed;
};

class Hasher_FNV1a {
public:
    Hasher_FNV1a(): mSeed(-1) {}
    Hasher_FNV1a(uint32_t seed): mSeed(seed) { }
    uint32_t operator () (const void *_buf, int len) const {
        assert(mSeed != (uint32_t)-1);
        assert(_buf != 0 && len > 0);

        uint8_t *buf = (uint8_t*)_buf;
        uint32_t hash = mSeed + 2166136261;
        for (int i = 0; i < len; ++i) {
            hash ^= buf[i];
            hash *= 16777619;
        }
        return hash;
    }
private:
    uint32_t mSeed;
};

class Hasher_Murmur {
public:
    Hasher_Murmur(): mSeed(-1) {}
    Hasher_Murmur(uint32_t seed): mSeed(seed) { }
    uint32_t operator () (const void *_buf, int len) const {
        assert(mSeed != (uint32_t)-1);
        assert(_buf != 0 && len > 0);

        const uint32_t c1 = 0xcc9e2d51;
        const uint32_t c2 = 0x1b873593;
        const uint32_t r1 = 15;
        const uint32_t r2 = 13;
        const uint32_t m = 5;
        const uint32_t n = 0xe6546b64;

        uint32_t hash = mSeed;
        uint8_t *buf = (uint8_t*)_buf;

        const int nblocks = len / 4;
        const uint32_t *blocks = (const uint32_t *)buf;
        for (int i = 0; i < nblocks; ++i) {
            uint32_t k = blocks[i];
            k *= c1;
            k = (k << r1) | (k >> (32 - r1));
            k *= c2;

            hash ^= k;
            hash = ((hash << r2) | (hash >> (32 - r2))) * m + n;
        }

        const uint8_t *tail = (const uint8_t *) (buf + nblocks * 4);
        uint32_t k1 = 0;

        switch (len & 3) {
            case 3:
                k1 ^= tail[2] << 16;
            case 2:
                k1 ^= tail[1] << 8;
            case 1:
                k1 ^= tail[0];

                k1 *= c1;
                k1 = (k1 << r1) | (k1 >> (32 - r1));
                k1 *= c2;
                hash ^= k1;
        }

        hash ^= len;
        hash ^= (hash >> 16);
        hash *= 0x85ebca6b;
        hash ^= (hash >> 13);
        hash *= 0xc2b2ae35;
        hash ^= (hash >> 16);

        return hash;
    }
private:
    uint32_t mSeed;
};

class Hasher_Default {
public:
    Hasher_Default(): mSeed(-1) {}
    Hasher_Default(uint32_t seed): mSeed(seed) { }
    uint32_t operator () (const void *_buf, int len) const {
        assert(mSeed != (uint32_t)-1);
        assert(_buf != 0 && len > 0);

        uint8_t *buf = (uint8_t*)_buf;
        uint32_t hash = mSeed;
        for (int i = 0; i < len; ++i) {
            hash ^= buf[i] + 0x9e3779b9 + (hash << 6) + (hash >> 2);
        }
        return hash;
    }
private:
    uint32_t mSeed;
};

template<typename HasherT>
class HasherSeq_List {
public:
    HasherSeq_List(int k, uint32_t seed) {
        minstd_rand0 rand(seed);
        for (int i = 0; i < k; ++i) {
            mHashers.push_back(HasherT(rand()));
        }
    }
    void operator () (uint32_t hashes[], const void *buf, int len) const {
        for (int i = 0; i < (int)mHashers.size(); ++i) {
            hashes[i] = mHashers[i](buf, len);
        }
    }
private:
    vector<HasherT> mHashers;
};

template<typename HasherT>
class HasherSeq_Double {
public:
    HasherSeq_Double(int k, uint32_t seed): mK(k) {
        minstd_rand0 rand(seed);
        mHasher1 = HasherT(rand());
        mHasher2 = HasherT(rand());
    }
    void operator () (uint32_t hashes[], const void *buf, int len) const {
        uint32_t h1 = mHasher1(buf, len), h2 = mHasher2(buf, len);
        for (int i = 0; i < mK; ++i) {
            hashes[i] = h1 + i * h2;
        }
    }
private:
    HasherT mHasher1, mHasher2;
    int mK;
};

template<typename T, typename HashSeqT>
static void hashWithHashSeq(uint32_t hashes[], const T &v, const HashSeqT &seq, typename enable_if<is_pod<T>::value, void>::type* =0) {
    seq(hashes, &v, sizeof(v));
}
template<typename HashSeqT>
static void hashWithHashSeq(uint32_t hashes[], const string &s, const HashSeqT &seq) {
    seq(hashes, s.c_str(), s.size() + 1);
}

//------------------------------
static int roundUp2Pow2(int i) {
    return 1 << (int)(ceil(log(i) / log(2)));
}

//------------------------------
template<typename HasherSeqT>
class BloomFilter {
public:
    BloomFilter(int maxSize, float maxLoadFactor = 0.1): 
    mN(maxSize), mM(roundUp2Pow2(int(maxSize / maxLoadFactor))), mK(computeK(mM, mN)),
    mHashMask(mM - 1), mBits(mM), mHasherSeq(mK, 0) {

        assert(maxLoadFactor <= 1);
    }

    template<typename T>
    void insert(const T &v) {
        uint32_t *hashes = (uint32_t*)alloca(sizeof(uint32_t) * mK);
        hashWithHashSeq(hashes, v, mHasherSeq);
        for (int i = 0; i < mK; ++i) {
            mBits.set(hashes[i] & mHashMask, true);
        }
    }

    template<typename T>
    uint32_t count(const T &v) const {
        uint32_t *hashes = (uint32_t*)alloca(sizeof(uint32_t) * mK);
        hashWithHashSeq(hashes, v, mHasherSeq);
        for (int i = 0; i < mK; ++i) {
            if (!mBits.get(hashes[i] & mHashMask)) return 0;
        }
        return 1;
    }

    int getMemoryUsage() const { return mBits.getMemoryUsage(); }
    int getK() const { return mK; }
    int getM() const { return mM; }
    int getN() const { return mN; }
private:
    static int computeK(int m, int n) {
        return (int)ceil(log(2) * m / n);
    }
private:
    int mN, mM, mK;
    int mHashMask;
    BitVector mBits;
    HasherSeqT mHasherSeq;
};

//------------------------------
template<typename BloomFilterT>
static void correctnessTest() {
    BloomFilterT bf(32);
    bf.insert(10);
    bf.insert(11);
    bf.insert(12);
    bf.insert("abcd");
    bf.insert("0123");
    assert(!bf.count(8));
    assert(!bf.count(9));
    assert(bf.count(10));
    assert(bf.count(11));
    assert(bf.count(12));
    assert(!bf.count(13));
    assert(!bf.count(1000));
    assert(bf.count("abcd"));
    assert(!bf.count("abcdf"));
    assert(!bf.count("abc"));
    assert(bf.count("0123"));
    assert(!bf.count("01234"));
    assert(!bf.count("012"));
    assert(!bf.count("0012"));
    assert(!bf.count(""));
}

template<typename BloomFilterT>
static void benchmarkSampleAndReport(const char *name, int n, int m, int seed) {
    unordered_set<int> ref;
    BloomFilterT bf(n, n / float(m));

    int sampleSpace = min(m, 4 * n);
    minstd_rand0 rand(seed);

    for (int i = 0; i < n; ++i) {
        uint32_t v = rand() % sampleSpace;
        ref.insert(v);
        bf.insert(v);
    }

    int fail = 0;
    for (int i = 0; i < sampleSpace; ++i) {
        if (ref.count(i) != bf.count(i)) ++fail;
    }

    printf("\tn=%d, m=%d, k=%d, memory=%.2fMB, fail=%%%g\n", bf.getN(), bf.getM(), bf.getK(), bf.getMemoryUsage() / (1024.0 * 1024), fail * 100.0 / sampleSpace);
}

template<typename BloomFilterT>
static void benchmark(const char *name) {
    printf("%s:\n", name);

    int n = 1000000;
    for (int i = 1; i < 64; i *= 2) {
        benchmarkSampleAndReport<BloomFilterT>(name, n, n * i, 0);
    }
}

#define doBenchmark(bloomType) correctnessTest<bloomType>(); benchmark<bloomType>(#bloomType)

int main() {
    doBenchmark(BloomFilter<HasherSeq_List<Hasher_Default>>);
    doBenchmark(BloomFilter<HasherSeq_Double<Hasher_Default>>);
    doBenchmark(BloomFilter<HasherSeq_List<Hasher_Jenkins>>);
    doBenchmark(BloomFilter<HasherSeq_Double<Hasher_Jenkins>>);
    doBenchmark(BloomFilter<HasherSeq_List<Hasher_FNV1a>>);
    doBenchmark(BloomFilter<HasherSeq_Double<Hasher_FNV1a>>);
    doBenchmark(BloomFilter<HasherSeq_List<Hasher_Murmur>>);
    doBenchmark(BloomFilter<HasherSeq_Double<Hasher_Murmur>>);
}
