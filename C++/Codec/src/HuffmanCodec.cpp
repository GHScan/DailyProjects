#include <queue>
#include <algorithm>

#include "Utils.h"
#include "HuffmanCodec.h"


static const uint16_t kMaxBitCount = sizeof(uint16_t) * 8;

namespace
{

// only for encoding
struct HuffmanECNode
{
    uint32_t Freq;
    HuffmanECNode *Left;
    HuffmanECNode *Right;
    int Depth;
    int Height;

    explicit HuffmanECNode(
        uint32_t freq, int height,
        HuffmanECNode *left = nullptr, HuffmanECNode *right = nullptr)
        : Freq(freq)
        , Left(left)
        , Right(right)
        , Depth(0)
        , Height(height)
    {
    }

    void Destroy()
    {
        if (Left != nullptr)
            Left->Destroy();
        if (Right != nullptr)
            Right->Destroy();

        delete this;
    }

    void AssignDepth(int depth = 0)
    {
        Depth = depth;

        if (Left != nullptr)
            Left->AssignDepth(depth + 1);
        if (Right != nullptr)
            Right->AssignDepth(depth + 1);
    }
};


struct HuffmanECNodeComparer final
{
    bool operator() (HuffmanECNode const *a, HuffmanECNode const *b) const
    {
        if (a->Freq == b->Freq)
            return a->Height > b->Height;
        return a->Freq > b->Freq;
    }
};

}

static std::vector<uint8_t> CreateTable(uint16_t codeLength[256])
{
    std::vector<uint8_t> table(kMaxBitCount + 256);

    for (uint16_t length = 1; length <= kMaxBitCount; ++length)
    {
        int count = 0;
        for (int value = 0; value < 256; ++value)
        {
            if (codeLength[value] == length)
                ++count;
        }
        table[length - 1] = count;
    }

    size_t off = kMaxBitCount;
    for (uint16_t length = 1; length <= kMaxBitCount; ++length)
    {
        for (int value = 0; value < 256; ++value)
        {
            if (codeLength[value] == length)
                table[off++] = value;
        }
    }

    return table;
}

void HuffmanEncoder::BuildTableFromData(uint8_t const *data, size_t size)
{
    HuffmanECNode *nodes[256];
    for (int value = 0; value < 256; ++value)
        nodes[value] = new HuffmanECNode(0, 1);
    
    // 
    for (size_t i = 0; i < size; ++i)
        ++nodes[data[i]]->Freq;
    
    //
    std::priority_queue<HuffmanECNode*, std::vector<HuffmanECNode*>, HuffmanECNodeComparer> q;
    for (int value = 0; value < 256; ++value)
        q.push(nodes[value]);

    while (q.size() > 1)
    {
        HuffmanECNode *n1 = q.top();
        q.pop();
        HuffmanECNode *n2 = q.top();
        q.pop();

        HuffmanECNode *n = new HuffmanECNode(
            n1->Freq + n2->Freq,
            std::max(n1->Height, n2->Height) + 1,
            n1, n2);

        q.push(n);
    }

    HuffmanECNode *root = q.top();
    root->AssignDepth();

    {
        int maxDepth = 0;
        for (int value = 0; value < 256; ++value)
            maxDepth = std::max(maxDepth, nodes[value]->Depth);

        // scale the depth range to ensure no overflow
        if (maxDepth > kMaxBitCount)
        {
            float scale = float(kMaxBitCount) / maxDepth;
            for (int value = 0; value < 256; ++value)
                nodes[value]->Depth = int(nodes[value]->Depth * scale);
        }
    }

    // generate table
    uint16_t codeLength[256];
    for (int value = 0; value < 256; ++value)
    {
        codeLength[value] = nodes[value]->Depth;
        assert(codeLength[value] <= kMaxBitCount);
    }

    root->Destroy();

    // tricky
    if (std::all_of(
        codeLength, 
        codeLength + 256,
        [&](int len) { return codeLength[0] == len; }))
    {
        ++codeLength[0];
        assert(codeLength[0] <= kMaxBitCount);
    }
   
    //
    std::vector<uint8_t> table = CreateTable(codeLength);
    LoadTable(table.data(), table.size());
}

std::vector<uint8_t> HuffmanEncoder::SaveTable()
{
    uint16_t codeLength[256];
    for (int value = 0; value < 256; ++value)
        codeLength[value] = mNodes[value].Length;

    return CreateTable(codeLength);
}

void HuffmanEncoder::LoadTable(uint8_t const *buf, size_t size)
{
    std::memset(mNodes, 0, sizeof mNodes);

    uint16_t code = 0;
    
    size_t off = kMaxBitCount;
    for (uint16_t length = 1; length <= kMaxBitCount; ++length)
    {
        int count = buf[length - 1];
        for (int i = 0; i < count; ++i)
        {
            int idx = buf[off++];
            mNodes[idx].Length = length;
            mNodes[idx].Code = code++;
        }

        code <<= 1;
    }
}

static uint8_t gStandardTable[][kMaxBitCount + 256] =
{
    // JpegDCLuminance
    {
        0, 0, 7, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
        4, 5, 3, 2, 6, 1, 0, 7, 8, 9, 10, 11, 0, 0, 0, 0,
    },
    // JpegDCChrominance
    {
        0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 0, 0, 0, 0,
    },
    // JpegACLuminance
    {
        0, 2, 1, 3, 3, 2, 4, 3, 5, 5, 4, 4, 0, 0, 1, 0x7d,
        0x01, 0x02, 0x03, 0x00, 0x04, 0x11, 0x05, 0x12, 0x21, 0x31, 0x41, 0x06, 0x13, 0x51, 0x61, 0x07,
        0x22, 0x71, 0x14, 0x32, 0x81, 0x91, 0xa1, 0x08, 0x23, 0x42, 0xb1, 0xc1, 0x15, 0x52, 0xd1, 0xf0,
        0x24, 0x33, 0x62, 0x72, 0x82, 0x09, 0x0a, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x25, 0x26, 0x27, 0x28,
        0x29, 0x2a, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
        0x4a, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69,
        0x6a, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89,
        0x8a, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
        0xa8, 0xa9, 0xaa, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xc2, 0xc3, 0xc4, 0xc5,
        0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xe1, 0xe2,
        0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,
        0xf9, 0xfa,
    },
    // JpegACChrominance
    {
        0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, 0x77,
        0x00, 0x01, 0x02, 0x03, 0x11, 0x04, 0x05, 0x21, 0x31, 0x06, 0x12, 0x41, 0x51, 0x07, 0x61, 0x71,
        0x13, 0x22, 0x32, 0x81, 0x08, 0x14, 0x42, 0x91, 0xa1, 0xb1, 0xc1, 0x09, 0x23, 0x33, 0x52, 0xf0,
        0x15, 0x62, 0x72, 0xd1, 0x0a, 0x16, 0x24, 0x34, 0xe1, 0x25, 0xf1, 0x17, 0x18, 0x19, 0x1a, 0x26,
        0x27, 0x28, 0x29, 0x2a, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
        0x49, 0x4a, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68,
        0x69, 0x6a, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
        0x88, 0x89, 0x8a, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0xa2, 0xa3, 0xa4, 0xa5,
        0xa6, 0xa7, 0xa8, 0xa9, 0xaa, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xc2, 0xc3,
        0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda,
        0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,
        0xf9, 0xfa,
    },
};


void HuffmanEncoder::LoadStandardTable(HuffmanTableKind table)
{
    int idx = static_cast<int>(table);
    assert(idx >= 0 && idx < ARRAY_SIZE(gStandardTable));

    LoadTable(gStandardTable[idx], sizeof gStandardTable[idx]);
}

void HuffmanEncoder::Encode(OutputBitStream &stream, uint8_t byte)
{
    uint16_t code = mNodes[byte].Code;
    uint16_t length = mNodes[byte].Length;

    for (int i = length - 1; i >= 0; --i)
        stream.Write((code >> i) & 1);
}


void HuffmanDecoder::LoadTable(uint8_t const *buf, size_t size)
{
    HuffmanEncoder encoder;
    encoder.LoadTable(buf, size);

    uint16_t maxLength = 0;
    for (int value = 0; value < 256; ++value)
        maxLength = std::max(maxLength, encoder.mNodes[value].Length);

    mNodes.resize(size_t(1) << (maxLength + 1));

    for (int value = 0; value < 256; ++value)
    {
        uint16_t code = encoder.mNodes[value].Code;
        uint16_t length = encoder.mNodes[value].Length;

        int idx = 1;
        for (int i = length - 1; i >= 0; --i)
        {
            idx = (idx << 1) | ((code >> i) & 1);
        }

        mNodes[idx].Value = value;
        mNodes[idx].Valid = 1;
    }
}

void HuffmanDecoder::LoadStandardTable(HuffmanTableKind table)
{
    int idx = static_cast<int>(table);
    assert(idx >= 0 && idx < ARRAY_SIZE(gStandardTable));

    LoadTable(gStandardTable[idx], sizeof gStandardTable[idx]);
}

uint8_t HuffmanDecoder::Decode(InputBitStream &stream)
{
    int idx = 1;

    do
    {
        idx = (idx << 1) | stream.Read();
    } while (!mNodes[idx].Valid);

    return mNodes[idx].Value;
}