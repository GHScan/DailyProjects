#include <cassert>

#include <fstream>
#include <vector>

#include "Image.h"

#pragma pack(push, 2)
struct BITMAPFILEHEADER 
{
    unsigned short  bfType;
    unsigned int    bfSize;
    unsigned short  bfReserved1;
    unsigned short  bfReserved2;
    unsigned int    bfOffBits;
} ;

struct BITMAPINFOHEADER
{
    unsigned int    biSize;
    int             biWidth;
    int             biHeight;
    unsigned short  biPlanes;
    unsigned short  biBitCount;
    unsigned int    biCompression;
    unsigned int    biSizeImage;
    int             biXPelsPerMeter;
    int             biYPelsPerMeter;
    unsigned int    biClrUsed;
    unsigned int    biClrImportant;
};
#pragma pack(pop)


bool Image::LoadBmp(char const *fname)
{
    std::ifstream fi(fname, std::ios::binary);


    BITMAPFILEHEADER fileHeader;
    BITMAPINFOHEADER infoHeader;

    if (!fi.read(reinterpret_cast<char*>(&fileHeader), sizeof fileHeader))
        return false;
    if (fileHeader.bfType != 'MB')
        return false;

    if (!fi.read(reinterpret_cast<char *>(&infoHeader), sizeof infoHeader))
        return false;
    if (infoHeader.biBitCount != 24 || infoHeader.biCompression != 0)
        return false;
    Format = ImageFormatKind::RGB;

    Width = infoHeader.biWidth;
    Height = infoHeader.biHeight < 0 ? (-infoHeader.biHeight) : infoHeader.biHeight;



    fi.seekg(fileHeader.bfOffBits);

    size_t pixelSize = 3;
    size_t lineSize = pixelSize * Width;
    size_t imageSize = lineSize * Height;

    Data.resize(imageSize);

    if (infoHeader.biHeight > 0)
    {
        size_t paddingSize = (4 - (Width * pixelSize) % 4) % 4;
        char padding[3];

        for (int i = 0; i < Height; i++)
        {
            if (!fi.read(reinterpret_cast<char *>(Data.data() + i * lineSize), lineSize))
                return false;

            if (!fi.read(padding, paddingSize))
                return false;
        }
    }
    else
    {
        if (!fi.read(reinterpret_cast<char*>(Data.data()), imageSize))
            return false;
    }


    // BGR -> RGB
    for (size_t i = 0; i < size_t(Height) * Width; ++i)
        std::swap(Data[i * 3 + 0], Data[i * 3 + 2]);


    return true;
}

bool Image::SaveBmp(char const *fname) const
{
    assert(Format == ImageFormatKind::RGB);

    std::ofstream fo(fname, std::ios::binary);



    BITMAPFILEHEADER fileHeader = {0};
    BITMAPINFOHEADER infoHeader = {0};

    size_t pixelSize = Format == ImageFormatKind::RGB ? 3 : 4;
    size_t paddingSize = (4 - (Width * pixelSize) % 4) % 4;
    size_t lineSize = pixelSize * Width;
    char padding[3] = { 0, 0, 0 };
    size_t fileSize = sizeof fileHeader + sizeof infoHeader + (lineSize + paddingSize) * Height;
    assert(fileSize <= std::numeric_limits<uint32_t>::max());

    fileHeader.bfType = 'MB';
    fileHeader.bfSize = static_cast<uint32_t>(fileSize);
    fileHeader.bfOffBits = sizeof fileHeader + sizeof infoHeader;

    infoHeader.biSize = sizeof infoHeader;
    infoHeader.biWidth = Width;
    infoHeader.biHeight = Height;
    infoHeader.biPlanes = 1;
    infoHeader.biBitCount = static_cast<uint16_t>(pixelSize * 8);
    infoHeader.biSizeImage = (lineSize + paddingSize) * Height;

    if (!fo.write(reinterpret_cast<char const *>(&fileHeader), sizeof fileHeader))
        return false;

    if (!fo.write(reinterpret_cast<char const *>(&infoHeader), sizeof infoHeader))
        return false;



    // RGB -> BGR
    std::vector<uint8_t> bgrData(Data);
    for (size_t i = 0; i < size_t(Height) * Width; ++i)
        std::swap(bgrData[i * 3 + 0], bgrData[i * 3 + 2]);

    for (int i = 0; i < Height; ++i) 
    {
        if (!fo.write(reinterpret_cast<char const *>(bgrData.data() + i * lineSize), lineSize))
            return false;

        if (!fo.write(padding, paddingSize))
            return false;
    }

    return true;
}
