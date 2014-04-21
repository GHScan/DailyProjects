#include "pch.h"

#include <unistd.h>

#include "compressor.h"
#include "stream.h"

extern void test_rle();
extern void test_huffman();
extern void test_lzw();

static void runUnitTests() {
    test_rle();
    test_huffman();
    test_lzw();
}

int main(int argc, char *argv[]) {
    runUnitTests();

    FILE *fi = stdin;
    FILE *fo = stdout;
    const char *compressorType = nullptr;
    bool isUncompress = false;

    int opt;
    while ((opt = getopt(argc, argv, "a:i:o:x")) != -1) {
        switch (opt) {
            case 'a':
                compressorType = optarg;
                break;
            case 'i':
                if ((fi = fopen(optarg, "rb")) == nullptr) {
                    fprintf(stderr, "Failed to open file : %s\n", optarg);
                    return EXIT_FAILURE;
                }
                break;
            case 'o':
                if ((fo = fopen(optarg, "wb")) == nullptr) {
                    fprintf(stderr, "Failed to open file : %s\n", optarg);
                    return EXIT_FAILURE;
                }
                break;
            case 'x':
                isUncompress = true;
                break;
            default:
                fprintf(stderr, "Usage : %s [-x] [-a algo] [-i inputfile] [-o outputfile]", argv[0]);
                return EXIT_FAILURE;
        }
    }
    if (compressorType == nullptr) {
        fprintf(stderr, "Can't find compressor !");
        return EXIT_FAILURE;
    }

    {
        if (isUncompress) ICompressor::uncompressFile(fi, fo, compressorType);
        else ICompressor::compressFile(fi, fo, compressorType);
    }

    if (fi != stdin) fclose(fi);
    if (fo != stdout) fclose(fo);
}
