#include <iostream>
#include <numeric>

#include "Test.h"
#include "Utils.h"
#include "Dct.h"
#include "ColorSpace.h"
#include "Image.h"
#include "SJpeg.h"
#include "HuffmanCodec.h"

static void TestDct1D()
{
    float values[64];
    std::iota(values, values + 64, 0.f);

    float freqs[64];
    Dct1D(freqs, 1, values, 1, 64);

    float values2[64];
    IDct1D(values2, 1, freqs, 1, 64);

    for (int i = 0; i < 64; ++i)
        assert(FloatEquals(values[i], values2[i]));
}

static void TestDct2D()
{
    float values[64];
    std::iota(values, values + 64, 0.f);

    float freqs[64];
    Dct2D(freqs, 8, values, 8, 8, 8);

    float values2[64];
    IDct2D(values2, 8, freqs, 8, 8, 8);

    for (int i = 0; i < 64; ++i)
        assert(FloatEquals(values[i], values2[i]));
}

static void TestRGB2YUVConversion()
{
    uint8_t rgb[4 * 2 * 3];
    for (int i = 0; i < sizeof(rgb); ++i)
        rgb[i] = i;

    int8_t y[8], u[8], v[8];

    ConvertRGB2YUV(y, u, v, rgb, 2 * 3, 4, 2);

    uint8_t rgb2[4 * 2 * 3];
    ConvertYUV2RGB(rgb2, 2 * 3, y, u, v, 4, 2);

    for (int i = 0; i < ARRAY_SIZE(rgb); ++i)
        assert(std::abs(rgb[i] - rgb2[i]) < 3);
}

static void TestStandardHuffmanTable()
{
    for (HuffmanTableKind huffmanTable = HuffmanTableKind::JpegFirst;
        huffmanTable <= HuffmanTableKind::JpegLast;
        huffmanTable = HuffmanTableKind((int)huffmanTable + 1))
    {
        int const len = 11;
        uint8_t data[len];
        for (int i = 0; i < len; ++i)
            data[i] = i;

        uint8_t compressed[len] = { 0 };
        {
            OutputBitStream stream(compressed, sizeof compressed);

            HuffmanEncoder encoder;
            encoder.LoadStandardTable(huffmanTable);

            for (int i = 0; i < len; ++i)
                encoder.Encode(stream, data[i]);
        }

        uint8_t data2[len] = { 0 };
        {
            InputBitStream stream(compressed, sizeof compressed);

            HuffmanDecoder decoder;
            decoder.LoadStandardTable(huffmanTable);

            for (int i = 0; i < len; ++i)
                data2[i] = decoder.Decode(stream);
        }

        for (int i = 0; i < len; ++i)
            assert(data[i] == data2[i]);
    }
}

static void TestCustomHuffmanTable()
{
    for (int turn = 0; turn < 2; ++turn)
    {
        int const len = 256;
        uint8_t data[len];
        if (turn == 0)
        {
            for (int i = 0; i < len; ++i)
                data[i] = i;
        }
        else
        {
            for (int i = 0; i < len; ++i)
                data[i] = rand();
        }

        std::vector<uint8_t> table;

        uint8_t compressed[len + 1] = { 0 };
        {
            OutputBitStream stream(compressed, sizeof compressed);

            HuffmanEncoder encoder;
            encoder.BuildTableFromData(data, sizeof data);
            table = encoder.SaveTable();

            for (int i = 0; i < len; ++i)
                encoder.Encode(stream, data[i]);
        }

        uint8_t data2[len] = { 0 };
        {
            InputBitStream stream(compressed, sizeof compressed);

            HuffmanDecoder decoder;
            decoder.LoadTable(table.data(), table.size());

            for (int i = 0; i < len; ++i)
                data2[i] = decoder.Decode(stream);
        }

        for (int i = 0; i < len; ++i)
            assert(data[i] == data2[i]);
    }
}

static void TestSJpeg()
{
    uint16_t quality = 50;

    Image img;
    img.LoadBmp("Lenna.bmp");

    std::vector<uint8_t> buf(img.Data.size());
    size_t compressedSize = buf.size();
    EncodeSJpeg(img, buf.data(), compressedSize, quality);

    printf("SJpeg Compression : %.1f KB ==> %.1f KB (Q %d, %.2f%%)\n", 
        buf.size() / 1024.f,
        compressedSize / 1024.f,
        quality,
        100.f * compressedSize / buf.size());

    Image img2 = img;
    img2.Data.assign(img2.Data.size(), 0);

    DecodeSJpeg(img2, buf.data(), compressedSize, quality);

    img2.SaveBmp("LennaCopy.bmp");
}

static void BenchSJpeg()
{
    uint16_t quality = 50;

    Image img;
    img.LoadBmp("Lenna.bmp");

    std::vector<uint8_t> buf(img.Data);
    size_t compressedSize;

    double encodeTime = TimeInDuration([&]() 
        {
            compressedSize = buf.size();
            EncodeSJpeg(img, buf.data(), compressedSize, quality);
        });
    double decodeTime = TimeInDuration([&]()
    {
        DecodeSJpeg(img, buf.data(), compressedSize, quality);
    });

    printf("Encode/Decode : %.1f FPS, %1.f FPS\n", 1 / encodeTime, 1 / decodeTime);
}

void Test()
{
    TestDct1D();
    TestDct2D();
    TestRGB2YUVConversion();
    TestStandardHuffmanTable();
    TestCustomHuffmanTable();
    TestSJpeg();
    BenchSJpeg();
}