#ifndef COMPRESSOR_H
#define COMPRESSOR_H

struct IInputStream;
struct IOutputStream;

struct ICompressor {
    static ICompressor* create(const char *type);
    static string compressString(const string& s, const char *type);
    static string uncompressString(const string &s, const char *type);
    static void compressFile(FILE *fi, FILE *fo, const char *type);
    static void uncompressFile(FILE *fi, FILE *fo, const char *type);

    virtual ~ICompressor() {}
    virtual void compress(IInputStream *si, IOutputStream *so) = 0;
    virtual void uncompress(IInputStream *si, IOutputStream *so) = 0;
};

#endif
