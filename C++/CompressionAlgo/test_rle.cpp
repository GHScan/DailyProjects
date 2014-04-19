
#include "pch.h"

#include "compressor.h"
#include "stream.h"

static string compressString(ICompressor *c, const string& s) {
    string r;
    StringInputStream si(s);
    StringOutputStream so(r);
    c->compress(&si, &so);
    return r;
}
static string uncompressString(ICompressor *c, const string &s) {
    string r;
    StringInputStream si(s);
    StringOutputStream so(r);
    c->uncompress(&si, &so);
    return r;
}

void test_rle() {
    {
        ICompressor *c = ICompressor::create("rle_1");
        {
            string s0 = "abcdefghjk";
            string s1 = compressString(c, s0);
            assert(s1.size() > s0.size());
            string s2 = uncompressString(c, s1);
            assert(s0 == s2);
        }
        {
            string s0 = "aaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbcccccccccccccccccccccccccccccccccc";
            string s1 = compressString(c, s0);
            assert(s1.size() < s0.size());
            string s2 = uncompressString(c, s1);
            assert(s0 == s2);
        }
        {
            string s0 = "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd";
            string s1 = compressString(c, s0);
            assert(s1.size() > s0.size());
            string s2 = uncompressString(c, s1);
            assert(s0 == s2);
        }
        delete c;
    }
    {
        ICompressor *c = ICompressor::create("rle_4");
        {
            string s0 = "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd";
            string s1 = compressString(c, s0);
            assert(s1.size() < s0.size());
            string s2 = uncompressString(c, s1);
            assert(s0 == s2);
        }
        delete c;
    }
}
