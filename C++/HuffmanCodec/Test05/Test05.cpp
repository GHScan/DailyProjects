// Test05.cpp : 定义控制台应用程序的入口点。
//

#include "stdafx.h"

#include <algorithm>

#include "BitStream.h"

inline size_t getBitStreamBytes(BitStream& bs)
{
    return bs.getByteCount() + 4;
}
size_t loadBitStream(const void* src, size_t len, BitStream& bs)
{
    if (len < 4) return 0;
    size_t bitCnt = *((const size_t*&)src)++;
    if (len < (bitCnt + 7) / 8 + 4) return 0;
    bs.pushN0(bitCnt);
    memcpy(bs.getPtr(), src, bs.getByteCount());
    return bs.getByteCount() + 4;
}
size_t saveBitStream(void *dest, size_t len, const BitStream& bs)
{
    if (len < bs.getByteCount() + 4) return 0;
    *((size_t*&)dest)++ = bs.getBitCount();
    memcpy(dest, bs.getPtr(), bs.getByteCount());
    return bs.getByteCount() + 4;
}

struct HuffmanNode
{
    HuffmanNode(unsigned char _c = 0, HuffmanNode *_left = NULL, HuffmanNode *_right = NULL):
    c(_c), left(_left), right(_right){}

    void deleteChildren(bool cascade)
    {
        if (cascade)
        {
            if (left != NULL)  left->deleteChildren(true);
            if (right != NULL) right->deleteChildren(true);
        }

        delete left; left = NULL;
        delete right; right = NULL;
    }

    void assignBit(const BitStream *parentBS, bool b)
    {
        if (parentBS != NULL)
        {
            bs = *parentBS;
            bs.push_back(b);
        }
        if (left != NULL) left->assignBit(&bs, true);
        if (right != NULL) right->assignBit(&bs, false);
    }

    void fillTable(HuffmanNode* (&table)[256])
    {
        if (left == NULL && right == NULL) table[c] = this;
        if (left != NULL) left->fillTable(table);
        if (right != NULL) right->fillTable(table);
    }

    void addLeaf(HuffmanNode *leaf, size_t depth = 0)
    {
        bool b = leaf->bs.read(depth);
        size_t nextDepth = depth + 1;
        if (b)
        {
            if (leaf->bs.getBitCount() > nextDepth)
            {
                if (left == NULL) left = new HuffmanNode;
                left->addLeaf(leaf, nextDepth);
            }
            else
            {
                assert(left == NULL);
                left = leaf;
            }
        }
        else
        {
            if (leaf->bs.getBitCount() > nextDepth)
            {
                if (right == NULL) right = new HuffmanNode;
                right->addLeaf(leaf, nextDepth);
            }
            else
            {
                assert(right == NULL);
                right = leaf;
            }
        }
    }

    unsigned char c;
    BitStream     bs;
    HuffmanNode  *left, *right;
};

class HuffmanEncoder
{
public:
    void addUncompressedData(void *src, size_t len)
    {
        m_uncompressedData.insert(
            m_uncompressedData.end(), 
            static_cast<unsigned char*>(src),
            static_cast<unsigned char*>(src) + len);
    }
    const void* getCompressedData() const { return m_compressedData.empty() ? NULL : & m_compressedData[0];  }
    size_t getCompressedDataLength() const { return m_compressedData.size(); }

    void compress();

private:
    std::vector<unsigned char>  m_uncompressedData;
    std::vector<unsigned char>  m_compressedData;
};

void HuffmanEncoder::compress()
{
    if (m_uncompressedData.empty()) return;

    size_t bytesFreq[256] = {0};
    {
        unsigned char *p = &m_uncompressedData[0];
        size_t cnt = m_uncompressedData.size();
        while (cnt-- > 0)
        {
            ++bytesFreq[*p++];
        }
    }

    typedef std::vector<HuffmanNode*>   HuffmanNodeList;
    HuffmanNodeList nodes;
    for (size_t i = 0; i < 256; ++i)
    {
        if (bytesFreq[i] > 0) nodes.push_back(new HuffmanNode(i));
    }

    struct HuffmanNodePtrLess
    {
        HuffmanNodePtrLess(size_t *_bytesFreq): bytesFreq(_bytesFreq){}

        bool operator () (const HuffmanNode* lhs, const HuffmanNode* rhs) const
        {
            return bytesFreq[lhs->c] > bytesFreq[rhs->c];
        }
        size_t *bytesFreq;
    };
    std::sort(nodes.begin(), nodes.end(), HuffmanNodePtrLess(bytesFreq));

    while (nodes.size() > 1)
    {
        HuffmanNode* last = nodes.back();
        nodes.pop_back();
        HuffmanNode* last2 = nodes.back();
        nodes.pop_back();
        bytesFreq[last->c] += bytesFreq[last2->c];
        nodes.push_back(new HuffmanNode(last->c, last, last2));

        last = nodes.back();
        size_t test = nodes.size() - 1;
        while (test > 0 && bytesFreq[nodes[test - 1]->c] < bytesFreq[last->c])
        {
            nodes[test] = nodes[test - 1];
            --test;
        }
        nodes[test] = last;
    }

    HuffmanNode *huffmanTree = nodes.front();
    huffmanTree->assignBit(NULL, false);
    
    HuffmanNode *table[256] = {NULL};
    huffmanTree->fillTable(table);

    BitStream dataBS;
    for (size_t i = 0; i < m_uncompressedData.size(); ++i)
    {
        dataBS += table[m_uncompressedData[i]]->bs;
    }

    unsigned char bytesBitCnt[256] = {0};
    for (int i = 0; i < 256; ++i)
    {
        bytesBitCnt[i] = table[i] == NULL ? 0 : table[i]->bs.getBitCount();
    }
    BitStream byteBS;
    for (size_t i = 0; i < 256; ++i)
    {
        if (table[i] != NULL) byteBS += table[i]->bs;
    }

    size_t beginPos = 0;
    m_compressedData.assign(256, 0);
    memcpy(&m_compressedData[beginPos], bytesBitCnt, 256);
    beginPos += m_compressedData.size();
    m_compressedData.resize(m_compressedData.size() + getBitStreamBytes(byteBS));
    saveBitStream(&m_compressedData[beginPos], getBitStreamBytes(byteBS), byteBS);
    beginPos += getBitStreamBytes(byteBS);
    m_compressedData.resize(m_compressedData.size() + getBitStreamBytes(dataBS));
    saveBitStream(&m_compressedData[beginPos], getBitStreamBytes(dataBS), dataBS);
    
    huffmanTree->deleteChildren(true);
    delete huffmanTree;

    m_uncompressedData.clear();
}

class HuffmanDecoder
{
public:
    void addCompressedData(const void *src, size_t len) 
    {
        m_compressedData.insert(
            m_compressedData.end(),
            (const unsigned char*)src, 
            (const unsigned char*)src + len);
    }
    const void* getUncompressedData() const  { return m_uncompressedData.empty() ? NULL : &m_uncompressedData[0]; }
    size_t getUncompressDataLength() const { return m_uncompressedData.size(); }

    bool uncompress();

private:
    std::vector<unsigned char>  m_compressedData;
    std::vector<unsigned char>  m_uncompressedData;
};

bool HuffmanDecoder::uncompress()
{
    size_t readPtr = 0;

    unsigned char byteBitCnt[256] = {0};
    if (m_compressedData.size() - readPtr < 256) return false;
    memcpy(byteBitCnt, &m_compressedData[0], 256);
    readPtr += 256;

    BitStream byteBS;
    if (!loadBitStream(&m_compressedData[0] + readPtr, m_compressedData.size() - readPtr, byteBS)) return false;
    readPtr += getBitStreamBytes(byteBS);
    BitStream dataBS;
    if (!loadBitStream(&m_compressedData[0] + readPtr, m_compressedData.size() - readPtr, dataBS)) return false;

    BitStreamIterator byteBSIter(byteBS);
    std::auto_ptr<HuffmanNode> table[256];
    for (size_t i = 0; i < 256; ++i)
    {
        if (byteBitCnt[i] != 0)
        {
            HuffmanNode *node = new HuffmanNode(i);
            while (
                byteBitCnt[i] > 0
                && byteBSIter.hasMore())
            {
                --byteBitCnt[i];
                node->bs.push_back(byteBSIter.readNext());
            }
            if (byteBitCnt[i] > 0) 
            {
                delete node;
                return false;
            }

            table[i].reset(node);
        }
    }

    HuffmanNode *huffmanTree = new HuffmanNode;
    for (size_t i = 0; i < 256; ++i)
    {
        if (table[i].get() != NULL) huffmanTree->addLeaf(table[i].get());
    }

    bool succ = true;
    HuffmanNode *curNode = huffmanTree;
    BitStreamIterator dataBSIter(dataBS);
    while (dataBSIter.hasMore())
    {
        bool b = dataBSIter.readNext();

        if (b) curNode = curNode->left;
        else curNode = curNode->right;

        if (curNode == NULL)
        {
            succ = false;
            break;
        }

        if (curNode->left == NULL && curNode->right == NULL)
        {
            m_uncompressedData.push_back(curNode->c);
            curNode = huffmanTree;
        }
    }

    for (size_t i = 0; i < 256; ++i)
    {
        table[i].release();
    }
    huffmanTree->deleteChildren(true);
    delete huffmanTree;

    return succ;
}

#include <fstream>
#include <Windows.h>

class Timer
{
public:
    Timer() { QueryPerformanceCounter(&m_begin); }
    ~Timer()
    {
        LARGE_INTEGER end;
        QueryPerformanceCounter(&end);
        LARGE_INTEGER freq;
        QueryPerformanceFrequency(&freq);
        printf("耗时:%.3f\n", double(end.QuadPart - m_begin.QuadPart) / freq.QuadPart);  
    }

    LARGE_INTEGER m_begin;
};

int main(int argc, char *argv[])
{
    Timer t;

    if (argc == 1)  {cout << "没有文件" << endl; return 0; }

    setlocale(LC_ALL, "");

    {
        HuffmanEncoder encoder;

        std::ifstream fi(argv[1], std::ios::binary);    
        if (!fi) { cout << "文件不存在: " << argv[1] << endl; return 1; }
        char buf[1024];
        for (;;)
        {
            fi.read(buf, sizeof(buf));
            if (fi.gcount() == 0) break;
            encoder.addUncompressedData(buf, fi.gcount());
        }

        encoder.compress();
        if (encoder.getCompressedData() == NULL) {cout << "压缩失败" << endl; return 1;}

        std::ofstream fo((std::string(argv[1]) + ".huf").c_str(), std::ios::binary);
        fo.write((const char*)encoder.getCompressedData(), encoder.getCompressedDataLength());
    }

    {
        HuffmanDecoder decoder;

        std::ifstream fi((std::string(argv[1]) + ".huf").c_str(), std::ios::binary);
        char buf[1024];
        for (;;)
        {
            fi.read(buf, sizeof(buf));
            if (fi.gcount() == 0) break;
            decoder.addCompressedData(buf, fi.gcount());
        }

        if (!decoder.uncompress()) { cout << "解压缩失败" << endl; return 1; }

        std::ofstream fo((std::string(argv[1]) + ".src").c_str(), std::ios::binary);
        fo.write((const char*)decoder.getUncompressedData(), decoder.getUncompressDataLength());
    }
}