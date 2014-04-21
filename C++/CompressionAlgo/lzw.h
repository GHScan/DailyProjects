#ifndef LZW_H
#define LZW_H

#include "compressor.h"

class LzwCompressor: public ICompressor {
public:
    LzwCompressor(int chunkSize);
    virtual void compress(IInputStream *si, IOutputStream *so);
    virtual void uncompress(IInputStream *si, IOutputStream *so);
private:
    int mChunkSize;
};

#endif
