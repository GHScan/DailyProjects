#ifndef COMPRESSOR_H
#define COMPRESSOR_H

struct IInputStream;
struct IOutputStream;

struct ICompressor {
    static ICompressor* create(const char *type);

    virtual ~ICompressor() {}
    virtual void compress(IInputStream *si, IOutputStream *so) = 0;
    virtual void uncompress(IInputStream *si, IOutputStream *so) = 0;
};

#endif
