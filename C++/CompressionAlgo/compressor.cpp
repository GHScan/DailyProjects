#include "pch.h"

#include "stream.h"
#include "compressor.h"
#include "rle.h"
#include "huffman.h"
#include "lzw.h"

ICompressor* ICompressor::create(const char *_type) {
    string type(_type);
    if (type == "rle") {
        return new RleCompressor(1);
    } else if (type == "rle_1") {
        return new RleCompressor(1);
    } else if (type == "rle_2") {
        return new RleCompressor(2);
    } else if (type == "rle_4") {
        return new RleCompressor(4);
    } else if (type == "rle_8") {
        return new RleCompressor(8);
    } else if (type == "huff") {
        return new HuffmanCompressor(4 * 1024);
    } else if (type == "huff_4k") {
        return new HuffmanCompressor(4 * 1024);
    } else if (type == "huff_16k") {
        return new HuffmanCompressor(16 * 1024);
    } else if (type == "huff_64k") {
        return new HuffmanCompressor(64 * 1024);
    } else if (type == "lzw") {
        return new LzwCompressor(16 * 1024);
    } else if (type == "lzw_16k") {
        return new LzwCompressor(16 * 1024);
    } else if (type == "lzw_128k") {
        return new LzwCompressor(128 * 1024);
    } else if (type == "lzw_1024k") {
        return new LzwCompressor(1024 * 1024);
    }
    return nullptr;
}


string ICompressor::compressString(const string& s, const char *type) {
    string r;
    StringInputStream si(s);
    StringOutputStream so(r);

    auto c = create(type);
    c->compress(&si, &so);
    delete c;

    return r;
}

string ICompressor::uncompressString(const string &s, const char *type) {
    string r;
    StringInputStream si(s);
    StringOutputStream so(r);

    auto c = create(type);
    c->uncompress(&si, &so);
    delete c;

    return r;
}

void ICompressor::compressFile(FILE *fi, FILE *fo, const char *type) {
    FileInputStream si(fi);
    FileOutputStream so(fo);

    auto c = create(type);
    c->compress(&si, &so);
    delete c;
}

void ICompressor::uncompressFile(FILE *fi, FILE *fo, const char *type) {
    FileInputStream si(fi);
    FileOutputStream so(fo);

    auto c = create(type);
    c->uncompress(&si, &so);
    delete c;
}
