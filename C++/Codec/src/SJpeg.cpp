#include <algorithm>

#include "Utils.h"
#include "SJpeg.h"
#include "DCT.h"
#include "BitStream.h"
#include "Sampler.h"
#include "HuffmanCodec.h"
#include "ColorSpace.h"


static const uint16_t kMaxBitCount = sizeof(uint16_t) * 8;

static uint8_t const gLuminanceQuatTable[64] =
{
    16,  11,  10,  16,  24,  40,  51,  61,
    12,  12,  14,  19,  26,  58,  60,  55,
    14,  13,  16,  24,  40,  57,  69,  56,
    14,  17,  22,  29,  51,  87,  80,  62,
    18,  22,  37,  56,  68, 109, 103,  77,
    24,  35,  55,  64,  81, 104, 113,  92,
    49,  64,  78,  87, 103, 121, 120, 101,
    72,  92,  95,  98, 112, 100, 103,  99
};
static uint8_t const gChrominanceQuatTable[16] =
{
    17,  24,  99,  99,
    24,  56,  99,  99,
    99,  99,  99,  99,
    99,  99,  99,  99,
};


static uint8_t const gZigZag8x8[64] = 
{
     0,  1,  8, 16,  9,  2,  3, 10,
    17, 24, 32, 25, 18, 11,  4,  5,
    12, 19, 26, 33, 40, 48, 41, 34,
    27, 20, 13,  6,  7, 14, 21, 28,
    35, 42, 49, 56, 57, 50, 43, 36,
    29, 22, 15, 23, 30, 37, 44, 51,
    58, 59, 52, 45, 38, 31, 39, 46,
    53, 60, 61, 54, 47, 55, 62, 63,
};
static uint8_t const gZigZag4x4[16] =
{
     0,  1,  4,  8,
     5,  2,  3,  6,
     9, 12, 13, 10,
     7, 11, 14, 15,
};

static void EncodeQuatFreq(
    uint16_t &length,
    uint16_t &code,
    int16_t freq)
{
    int absFreq = freq;
    if (absFreq < 0)
        absFreq = -absFreq;

    length = 0;
    while ((1 << length) <= absFreq)
        ++length;
    assert(length <= kMaxBitCount);

    code = freq > 0 ? (freq ^ (1 << (length - 1))) : -freq;
}

static void DecodeQuatFreq(
    int16_t &freq,
    uint16_t length,
    uint16_t code)
{
    assert(code == (code & ((1 << length) - 1)));

    if (code >> (length - 1))
    {
        freq = -code;
    }
    else
    {
        freq = code ^ (1 << (length - 1));
    }
}

static void CompressFreq(
    int16_t &lastDC,
    int16_t const *freq, int n,
    uint8_t const *acOrder,
    OutputBitStream &stream,
    HuffmanEncoder &dcEncoder, 
    HuffmanEncoder &acEncoder)
{
    // dc
    {
        int16_t dcDiff = freq[0] - lastDC;
        lastDC = freq[0];

        uint16_t length, code;
        EncodeQuatFreq(length, code, dcDiff);
        
        dcEncoder.Encode(stream, int8_t(length));
        stream.Write(code, length);
    }

    // ac
    int ei = n;
    for (; ei > 1 && freq[acOrder[ei - 1]] == 0; --ei);

    for (int i = 1; i < ei;)
    {
        int zeroCount = 0;
        int maxZc = std::min(ei - i - 1, 15);
        for (; zeroCount < maxZc && freq[acOrder[i + zeroCount]] == 0;)
            ++zeroCount;

        uint16_t length, code;
        EncodeQuatFreq(length, code, freq[acOrder[i + zeroCount]]);

        acEncoder.Encode(stream, (zeroCount << 4) | length);
        stream.Write(code, length);

        i += zeroCount + 1;
    }

    if (ei < n)
        acEncoder.Encode(stream, 0);
}

static void DecompressFreq(
    int16_t &lastDC,
    int16_t *freq, int n,
    uint8_t const *acOrder,
    InputBitStream &stream,
    HuffmanDecoder &dcDecoder,
    HuffmanDecoder &acDecoder)
{
    // dc
    {
        uint16_t length = dcDecoder.Decode(stream);
        uint16_t code;
        stream.Read(code, length);

        int16_t dcDiff;
        DecodeQuatFreq(dcDiff, length, code);

        freq[0] = lastDC + dcDiff;
        lastDC = freq[0];
    }

    // ac
    int i = 1;
    while (i < n)
    {
        uint8_t byte = acDecoder.Decode(stream);
        if (byte == 0) break;

        int zeroCount = byte >> 4;
        for (int j = 0; j < zeroCount; ++j, ++i)
            freq[acOrder[i]] = 0;

        uint16_t length = byte & 0xf;
        uint16_t code;
        stream.Read(code, length);

        DecodeQuatFreq(freq[acOrder[i]], length, code);
        ++i;
    }
    for (; i < n; ++i)
    {
        freq[acOrder[i]] = 0;
    }
}

void EncodeSJpeg(Image const &image, uint8_t *data, size_t &size, uint16_t quality)
{
    uint8_t const *src = image.Data.data();
    size_t bytesPerPixel = image.Format == ImageFormatKind::RGB ? 3 : 4;
    int h = image.Height, w = image.Width;
    int hb = DivideUp(h, 8);
    int wb = DivideUp(w, 8);

    OutputBitStream stream(data, size);
    HuffmanEncoder dcLEncoder, dcCEncoder, acLEncoder, acCEncoder;
    dcLEncoder.LoadStandardTable(HuffmanTableKind::JpegDCLuminance);
    dcCEncoder.LoadStandardTable(HuffmanTableKind::JpegDCChrominance);
    acLEncoder.LoadStandardTable(HuffmanTableKind::JpegACLuminance);
    acCEncoder.LoadStandardTable(HuffmanTableKind::JpegACChrominance);

    int16_t lastYDc = 0;
    int16_t lastUDc = 0;
    int16_t lastVDc = 0;

    for (int hb0 = 0; hb0 < hb; ++hb0)
    {
        for (int wb0 = 0; wb0 < wb; ++wb0)
        {
            int h0 = hb0 * 8;
            int w0 = wb0 * 8;

            int8_t y[64] = { 0 }, u[64] = { 0 }, v[64] = { 0 };
            ConvertRGB2YUV(
                y, u, v, 
                src + (h0 * w + w0) * bytesPerPixel, w * bytesPerPixel, 
                std::min(8, h - h0), std::min(8, w - w0));

            float fy[64], fu[16], fv[16];
            Copy(fy, y, 64);
            Sample2D(fu, 4, 4, 4, u, 8, 8, 8, SampleMethodKind::Nearest);
            Sample2D(fv, 4, 4, 4, v, 8, 8, 8, SampleMethodKind::Nearest);

            float freqY[64], freqU[16], freqV[16];
            Dct2D(freqY, 8, fy, 8, 8, 8);
            Dct2D(freqU, 4, fu, 4, 4, 4);
            Dct2D(freqV, 4, fv, 4, 4, 4);

            int16_t quatY[64], quatU[16], quatV[16];
            for (int i = 0; i < 64; ++i)
            {
                int q = (gLuminanceQuatTable[i] * quality + 50) / 100;
                quatY[i] = Convert<int16_t>(freqY[i] / q);
            }
            for (int i = 0; i < 16; ++i)
            {
                int q = (gChrominanceQuatTable[i] * quality + 50) / 100;
                quatU[i] = Convert<int16_t>(freqU[i] / q);
                quatV[i] = Convert<int16_t>(freqV[i] / q);
            }

            CompressFreq(
                lastYDc, quatY, 64, gZigZag8x8, stream, dcLEncoder, acLEncoder);
            CompressFreq(
                lastUDc, quatU, 16, gZigZag4x4, stream, dcCEncoder, acCEncoder);
            CompressFreq(
                lastVDc, quatV, 16, gZigZag4x4, stream, dcCEncoder, acCEncoder);
        }
    }

    size = stream.OffsetInBytes();
}

void DecodeSJpeg(Image &image, uint8_t const *data, size_t size, uint16_t quality)
{
    uint8_t *dst = image.Data.data();
    size_t bytesPerPixel = image.Format == ImageFormatKind::RGB ? 3 : 4;
    int h = image.Height, w = image.Width;
    int hb = DivideUp(h, 8);
    int wb = DivideUp(w, 8);

    InputBitStream stream(data, size);
    HuffmanDecoder dcLDecoder, dcCDecoder, acLDecoder, acCDecoder;
    dcLDecoder.LoadStandardTable(HuffmanTableKind::JpegDCLuminance);
    dcCDecoder.LoadStandardTable(HuffmanTableKind::JpegDCChrominance);
    acLDecoder.LoadStandardTable(HuffmanTableKind::JpegACLuminance);
    acCDecoder.LoadStandardTable(HuffmanTableKind::JpegACChrominance);

    int16_t lastYDc = 0;
    int16_t lastUDc = 0;
    int16_t lastVDc = 0;

    for (int hb0 = 0; hb0 < hb; ++hb0)
    {
        for (int wb0 = 0; wb0 < wb; ++wb0)
        {
            int16_t quatY[64], quatU[16], quatV[16];
            DecompressFreq(
                lastYDc, quatY, 64, gZigZag8x8, stream, dcLDecoder, acLDecoder);
            DecompressFreq(
                lastUDc, quatU, 16, gZigZag4x4, stream, dcCDecoder, acCDecoder);
            DecompressFreq(
                lastVDc, quatV, 16, gZigZag4x4, stream, dcCDecoder, acCDecoder);


            float freqY[64], freqU[16], freqV[16];
            for (int i = 0; i < 64; ++i)
            {
                int q = (gLuminanceQuatTable[i] * quality + 50) / 100;
                freqY[i] = float(quatY[i]) * q;
            }
            for (int i = 0; i < 16; ++i)
            {
                int q = (gChrominanceQuatTable[i] * quality + 50) / 100;
                freqU[i] = float(quatU[i]) * q;
                freqV[i] = float(quatV[i]) * q;
            }

            float fy[64], fu[16], fv[16];
            IDct2D(fy, 8, freqY, 8, 8, 8);
            IDct2D(fu, 4, freqU, 4, 4, 4);
            IDct2D(fv, 4, freqV, 4, 4, 4);


            for (int i = 0; i < 64; ++i)
            {
                fy[i] = Clamp(fy[i], -128.f, 127.f);
            }

            int8_t y[64] = { 0 }, u[64] = { 0 }, v[64] = { 0 };
            Copy(y, fy, 64);
            Sample2D(u, 8, 8, 8, fu, 4, 4, 4, SampleMethodKind::Nearest);
            Sample2D(v, 8, 8, 8, fv, 4, 4, 4, SampleMethodKind::Nearest);


            int h0 = hb0 * 8;
            int w0 = wb0 * 8;
            ConvertYUV2RGB(
                dst + (h0 * w + w0) * bytesPerPixel, w * bytesPerPixel, 
                y, u, v, 
                std::min(8, h - h0), std::min(8, w - w0));
        }
    }
}