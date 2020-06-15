#ifndef IMAGE_H
#define IMAGE_H

#include <vector>
#include <memory>

enum class ImageFormatKind
{
    RGB,
    RGBA,
};

struct Image final
{
    int Width;
    int Height;
    ImageFormatKind Format;
    std::vector<uint8_t> Data;

    bool LoadBmp(char const *fname);
    bool SaveBmp(char const *fname) const;
};


#endif