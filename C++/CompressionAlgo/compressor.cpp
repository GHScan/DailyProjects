#include "pch.h"

#include "compressor.h"
#include "rle.h"

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
    }
    return nullptr;
}
