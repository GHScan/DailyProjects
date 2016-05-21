#ifndef MEMCPY_IMP_H
#define MEMCPY_IMP_H

#include <cstdint>
#include <emmintrin.h>

static void* memcpy_for(void* destination, const void* source, size_t num) {
    auto dst = static_cast<uint8_t*>(destination);
    auto src = static_cast<uint8_t const*>(source);

    for (size_t i = 0; i < num; ++i)
        dst[i] = src[i];

    return destination;
}

static void* memcpy_sse2(void* destination, const void* source, size_t num) {
    auto dst = static_cast<uint8_t*>(destination);
    auto src = static_cast<uint8_t const*>(source);

_label_switch:
    switch (num) {
    default:
    {
        auto alignStep = 16 - reinterpret_cast<uintptr_t>(dst) & 0xf;

        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), _mm_loadu_si128(reinterpret_cast<__m128i const*>(src)));

        src += alignStep;
        dst += alignStep;
        num -= alignStep;

        if ((reinterpret_cast<uintptr_t>(src) & 0xf) == 0) {
            for (; num >= 64; num -= 64) {
                auto a = _mm_load_si128(reinterpret_cast<__m128i const*>(src));
                auto b = _mm_load_si128(reinterpret_cast<__m128i const*>(src) + 1);
                auto c = _mm_load_si128(reinterpret_cast<__m128i const*>(src) + 2);
                auto d = _mm_load_si128(reinterpret_cast<__m128i const*>(src) + 3);
                _mm_stream_si128(reinterpret_cast<__m128i*>(dst), a);
                _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 1, b);
                _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 2, c);
                _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 3, d);
                src += 64;
                dst += 64;
            }
        }
        else {
            for (; num >= 64; num -= 64) {
                auto a = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src));
                auto b = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 1);
                auto c = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 2);
                auto d = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 3);
                _mm_stream_si128(reinterpret_cast<__m128i*>(dst), a);
                _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 1, b);
                _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 2, c);
                _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 3, d);
                src += 64;
                dst += 64;
            }
        }
        _mm_sfence();
    }
    goto _label_switch;
    case 128:
    {
        auto a = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src));
        auto b = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 1);
        auto c = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 2);
        auto d = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 3);
        auto e = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 4);
        auto f = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 5);
        auto g = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 6);
        auto h = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 7);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), a);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 1, b);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 2, c);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 3, d);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 4, e);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 5, f);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 6, g);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 7, h);
    }
    break;
    case 127:
    case 126:
    case 125:
    case 124:
    case 123:
    case 122:
    case 121:
    case 120:
    case 119:
    case 118:
    case 117:
    case 116:
    case 115:
    case 114:
    case 113:
    case 112:
    case 111:
    case 110:
    case 109:
    case 108:
    case 107:
    case 106:
    case 105:
    case 104:
    case 103:
    case 102:
    case 101:
    case 100:
    case 99:
    case 98:
    case 97:
    case 96:
    case 95:
    case 94:
    case 93:
    case 92:
    case 91:
    case 90:
    case 89:
    case 88:
    case 87:
    case 86:
    case 85:
    case 84:
    case 83:
    case 82:
    case 81:
    case 80:
    case 79:
    case 78:
    case 77:
    case 76:
    case 75:
    case 74:
    case 73:
    case 72:
    case 71:
    case 70:
    case 69:
    case 68:
    case 67:
    case 66:
    case 65:
    {
        auto a = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src));
        auto b = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 1);
        auto c = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 2);
        auto d = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 3);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), a);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 1, b);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 2, c);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 3, d);
        dst += 64;
        src += 64;
        num -= 64;
        goto _label_switch;
    }
    break;
    case 64:
    {
        auto a = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src));
        auto b = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 1);
        auto c = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 2);
        auto d = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 3);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), a);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 1, b);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 2, c);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 3, d);
    }
    break;
    case 63:
    case 62:
    case 61:
    case 60:
    case 59:
    case 58:
    case 57:
    case 56:
    case 55:
    case 54:
    case 53:
    case 52:
    case 51:
    case 50:
    case 49:
    {
        auto a = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src));
        auto b = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 1);
        auto c = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 2);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), a);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 1, b);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 2, c);
        dst += 48;
        src += 48;
        num -= 48;
        goto _label_switch;
    }
    break;
    case 48:
    {
        auto a = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src));
        auto b = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 1);
        auto c = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 2);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), a);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 1, b);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 2, c);
    }
    break;
    case 47:
    case 46:
    case 45:
    case 44:
    case 43:
    case 42:
    case 41:
    case 40:
    case 39:
    case 38:
    case 37:
    case 36:
    case 35:
    case 34:
    case 33:
    {
        auto a = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src));
        auto b = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 1);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), a);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 1, b);
        dst += 32;
        src += 32;
        num -= 32;
        goto _label_switch;
    }
    break;
    case 32:
    {
        auto a = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src));
        auto b = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 1);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), a);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 1, b);
    }
    break;
    case 31:
    case 30:
    case 29:
    case 28:
    case 27:
    case 26:
    case 25:
    case 24:
    case 23:
    case 22:
    case 21:
    case 20:
    case 19:
    case 18:
    case 17:
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), _mm_loadu_si128(reinterpret_cast<__m128i const*>(src)));
        dst += 16;
        src += 16;
        num -= 16;
        goto _label_switch;
    case 16:
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), _mm_loadu_si128(reinterpret_cast<__m128i const*>(src)));
        break;
    case 15:
        dst[14] = src[14];
    case 14:
        reinterpret_cast<uint64_t*>(dst)[0] = reinterpret_cast<uint64_t const*>(src)[0];
        reinterpret_cast<uint32_t*>(dst)[2] = reinterpret_cast<uint32_t const*>(src)[2];
        reinterpret_cast<uint16_t*>(dst)[6] = reinterpret_cast<uint16_t const*>(src)[6];
        break;
    case 13:
        dst[12] = src[12];
    case 12:
        reinterpret_cast<uint64_t*>(dst)[0] = reinterpret_cast<uint64_t const*>(src)[0];
        reinterpret_cast<uint32_t*>(dst)[2] = reinterpret_cast<uint32_t const*>(src)[2];
        break;
    case 11:
        dst[10] = src[10];
    case 10:
        reinterpret_cast<uint64_t*>(dst)[0] = reinterpret_cast<uint64_t const*>(src)[0];
        reinterpret_cast<uint16_t*>(dst)[4] = reinterpret_cast<uint16_t const*>(src)[4];
        break;
    case 9:
        dst[8] = src[8];
    case 8:
        reinterpret_cast<uint64_t*>(dst)[0] = reinterpret_cast<uint64_t const*>(src)[0];
        break;
    case 7:
        dst[6] = src[6];
    case 6:
        reinterpret_cast<uint32_t*>(dst)[0] = reinterpret_cast<uint32_t const*>(src)[0];
        reinterpret_cast<uint16_t*>(dst)[2] = reinterpret_cast<uint16_t const*>(src)[2];
        break;
    case 5:
        dst[4] = src[4];
    case 4:
        reinterpret_cast<uint32_t*>(dst)[0] = reinterpret_cast<uint32_t const*>(src)[0];
        break;
    case 3:
        dst[2] = src[2];
    case 2:
        reinterpret_cast<uint16_t*>(dst)[0] = reinterpret_cast<uint16_t const*>(src)[0];
        break;
    case 1:
        dst[0] = src[0];
        break;
    case 0:
        break;
    }

    return destination;
}

#endif
