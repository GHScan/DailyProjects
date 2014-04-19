#ifndef RLE_H
#define RLE_H

#include "compressor.h"

class RleCompressor: public ICompressor {
public:
    RleCompressor(int blockSize);
    virtual void compress(IInputStream *si, IOutputStream *so);
    virtual void uncompress(IInputStream *si, IOutputStream *so);
private:
    int mBlockSize;
};

#endif
