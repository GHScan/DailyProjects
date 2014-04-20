#ifndef HUFFMAN_H
#define HUFFMAN_H

#include "compressor.h"

class HuffmanCompressor: public ICompressor {
public:
    HuffmanCompressor(int chunkSize);
    virtual void compress(IInputStream *si, IOutputStream *so);
    virtual void uncompress(IInputStream *si, IOutputStream *so);
private:
    int mChunkSize;
};

#endif
