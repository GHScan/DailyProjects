
#include "pch.h"

#include "compressor.h"

void test_rle() {
    {
        const char *type = "rle_1";
        {
            string s0 = "abcdefghjk";
            string s1 = ICompressor::compressString(s0, type);
            assert(s1.size() > s0.size());
            string s2 = ICompressor::uncompressString(s1, type);
            assert(s0 == s2);
        }
        {
            string s0 = "aaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbcccccccccccccccccccccccccccccccccc";
            string s1 = ICompressor::compressString(s0, type);
            assert(s1.size() < s0.size());
            string s2 = ICompressor::uncompressString(s1, type);
            assert(s0 == s2);
        }
        {
            string s0 = "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd";
            string s1 = ICompressor::compressString(s0, type);
            assert(s1.size() > s0.size());
            string s2 = ICompressor::uncompressString(s1, type);
            assert(s0 == s2);
        }
    }
    {
        const char *type = "rle_4";
        {
            string s0 = "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd";
            string s1 = ICompressor::compressString(s0, type);
            assert(s1.size() < s0.size());
            string s2 = ICompressor::uncompressString(s1, type);
            assert(s0 == s2);
        }
    }
}
