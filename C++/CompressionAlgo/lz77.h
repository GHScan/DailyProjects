#ifndef LZ77_H
#define LZ77_H

#include "compressor.h"

class Lz77Compressor: public ICompressor {
public:
    Lz77Compressor(int chunkSize, int windowSize);
    virtual void compress(IInputStream *si, IOutputStream *so);
    virtual void uncompress(IInputStream *si, IOutputStream *so);
private:
    int mChunkSize;
    int mWindowSize;
};

#endif
