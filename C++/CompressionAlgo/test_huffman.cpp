#include "pch.h"

#include "compressor.h"
#include "stream.h"

void test_huffman() {
    {
        const char *type = "huff_1k";
        {
            string s0 = "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd";
            string s1 = ICompressor::compressString(s0, type);
            printf("%d,%d\n", s0.size(), s1.size());
            assert(s1.size() < s0.size());
            string s2 = ICompressor::uncompressString(s1, type);
            assert(s0 == s2);
        }
    }
}

