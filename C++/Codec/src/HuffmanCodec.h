#ifndef HUFFMAN_CODEC_H
#define HUFFMAN_CODEC_H

#include <vector>

#include "BitStream.h"


enum class HuffmanTableKind
{
    JpegDCLuminance,
    JpegDCChrominance,
    JpegACLuminance,
    JpegACChrominance,

    JpegFirst = JpegDCLuminance,
    JpegLast = JpegACChrominance,
};

class HuffmanEncoder
{
public:
    void BuildTableFromData(uint8_t const *data, size_t size);

    std::vector<uint8_t> SaveTable();

    void LoadTable(uint8_t const *buf, size_t size);
    void LoadStandardTable(HuffmanTableKind table);

    void Encode(OutputBitStream &stream, uint8_t byte);

private:
    struct Node
    {
        uint16_t Length;
        uint16_t Code;
    };
    Node mNodes[256];

    friend class HuffmanDecoder;
};


class HuffmanDecoder
{
public:
    void LoadTable(uint8_t const *buf, size_t size);
    void LoadStandardTable(HuffmanTableKind table);

    uint8_t Decode(InputBitStream &stream);

private:
    struct Node
    {
        uint8_t Value;
        uint8_t Valid;
    };
    std::vector<Node> mNodes;
};


#endif