#include "pch.h"

#include "BitVectorAllocator.h"

static bool isPower2(int n)
{
    return n != 0 && ((n - 1) & n) == 0;
}
static int maxlog2(int n)
{
    return (int)ceil(log(n) / log(2));
}
static int up2Power2(int n)
{
    return 1 << maxlog2(n);
}

class LayeredBitVector
{
public:
    LayeredBitVector();
    LayeredBitVector(int entryCnt);
    ~LayeredBitVector();
    bool hasMoreFree() const;
    void mask(int i, bool b);
    int getFreeIdx() const;
private:
    vector<int> m_bits;
};
const int INT_LOG2 = maxlog2(sizeof(int)) + 3;
const int INT_BIT_COUNT = sizeof(int) * 8;

LayeredBitVector::LayeredBitVector()
{
}
LayeredBitVector::LayeredBitVector(int entryCnt)
{
    assert(isPower2(entryCnt));
    int log2 = maxlog2(entryCnt);
    assert(log2 >= INT_LOG2);
    m_bits.resize(1 << (log2-INT_LOG2 + 1));
}
LayeredBitVector::~LayeredBitVector()
{
    if (!m_bits.empty()) assert(m_bits[1] == 0);
}
bool LayeredBitVector::hasMoreFree() const
{
    return m_bits[1] != -1;
}
void LayeredBitVector::mask(int i, bool b)
{
    int idx = m_bits.size() / 2 + i / INT_BIT_COUNT;
    int mask = 1 << (i % INT_BIT_COUNT);
    if (b) m_bits[idx] |= mask;
    else m_bits[idx] &= ~mask;
    while (idx > 1) {
        idx /= 2;
        m_bits[idx] = m_bits[idx * 2] & m_bits[idx * 2 + 1];
    }
}
int LayeredBitVector::getFreeIdx() const
{
    assert(hasMoreFree());
    int idx = 1;
    while (idx < m_bits.size() / 2) {
        idx *= 2;
        if (m_bits[idx] == -1) ++idx;
    }

    int bits = m_bits[idx];
    char *p = (char*)&bits;
    int i = 0;
    while (p[i] == -1) ++i;
    assert(i < 4);

    char cbits = p[i];
    for (int j = 0; j < 8; ++j) {
        if (((cbits >> j) & 1) == 0) {
            return (idx - m_bits.size() / 2) * INT_BIT_COUNT + i * 8 + j;
        }
    }
    assert(0);
    return 0;
}
//====================
class BitVectorBlock
{
public:
    BitVectorBlock();
    BitVectorBlock(int entrySize, int blockSize);
    ~BitVectorBlock();
    void* alloc();
    bool free(void *p);
private:
    LayeredBitVector m_bitvec;
    char *m_block;
    int m_blockByteSize;
    int m_entrySize, m_blockSize;
};

BitVectorBlock::BitVectorBlock():
    m_block(NULL), m_blockByteSize(0), m_entrySize(0), m_blockSize(0)
{
}
BitVectorBlock::BitVectorBlock(int entrySize, int blockSize):
    m_bitvec(blockSize), m_entrySize(entrySize), m_blockSize(blockSize)
{
    m_blockByteSize = m_entrySize * m_blockSize;
    m_block = (char*)malloc(m_blockByteSize);
}
BitVectorBlock::~BitVectorBlock()
{
    free(m_block);
}
void* BitVectorBlock::alloc()
{
    if (!m_bitvec.hasMoreFree()) return NULL;

    int idx = m_bitvec.getFreeIdx();
    m_bitvec.mask(idx, true);
    return m_block + idx * m_entrySize;
}
bool BitVectorBlock::free(void *p)
{
    int off = (char*)p - m_block;
    if (off < 0 || off >= m_blockByteSize) return false;

    int idx = ((char*)p - m_block) / m_entrySize;
    m_bitvec.mask(idx, false);
    return true;
}
//====================
class SizeNBitVectorAllocator
{
public:
    SizeNBitVectorAllocator(int entrySize, int blockSize);
    ~SizeNBitVectorAllocator();
    void* alloc();
    void free(void *p);
private:
    int m_entrySize, m_blockSize;
    vector<BitVectorBlock*> m_blocks;
};
SizeNBitVectorAllocator::SizeNBitVectorAllocator(int entrySize, int blockSize):
    m_entrySize(entrySize), m_blockSize(blockSize)
{
}
SizeNBitVectorAllocator::~SizeNBitVectorAllocator()
{
    for (auto &block : m_blocks) delete block;
}
void* SizeNBitVectorAllocator::alloc()
{
    void *p = NULL;
    for (auto &block : m_blocks) if (p = block->alloc()) break;
    if (p == NULL) {
        m_blocks.push_back(new BitVectorBlock(m_entrySize, m_blockSize));
        p = m_blocks.back()->alloc();
    }
    assert(p != NULL);
    return p;
}
void SizeNBitVectorAllocator::free(void *p)
{
    bool b = false;
    for (auto &block : m_blocks) if (b = block->free(p)) break;
    assert(b);
}
//====================
BitVectorAllocator::BitVectorAllocator()
{
    int sa[] = {4,8,16,24,32,40,48,64,80,96,128};
    for (auto sz : sa) {
        m_sizeNs.push_back(sz);
        m_sizeNAllocators.push_back(new SizeNBitVectorAllocator(sz, up2Power2((1<<12)/sz)));
    }
}
BitVectorAllocator::~BitVectorAllocator()
{
    for (auto &p : m_sizeNAllocators) delete p;
}
void* BitVectorAllocator::alloc(int size)
{
    size += 1;
    auto iter = lower_bound(m_sizeNs.begin(), m_sizeNs.end(), size);
    if (iter == m_sizeNs.end()) {
        char *p = (char*)::malloc(size);
        p[0] = -1;
        return p + 1;
    }
    else {
        int idx = int(iter - m_sizeNs.begin());
        char *p = (char*)m_sizeNAllocators[idx]->alloc();
        p[0] = idx;
        return p + 1;
    }
}
void BitVectorAllocator::free(void *_p)
{
    char *p = (char*)_p;
    if (p[-1] == -1) {
        ::free(p - 1);
    }
    else {
        m_sizeNAllocators[p[-1]]->free(p - 1);
    }
}
