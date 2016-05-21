#ifndef MEMCPY_IMP_H
#define MEMCPY_IMP_H

#include <cstdint>

#include <emmintrin.h>

namespace scan {

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

        for (; num >= 64; num -= 64, src += 64, dst += 64) {
            auto a = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src));
            auto b = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 1);
            auto c = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 2);
            auto d = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 3);
            _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), a);
            _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 1, b);
            _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 2, c);
            _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 3, d);
        }

        for (; num >= 16; num -= 16, src += 16, dst += 16) {
            _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), _mm_loadu_si128(reinterpret_cast<__m128i const*>(src)));
        }

        for (size_t i = 0; i < num; ++i)
            dst[i] = src[i];

        return destination;
    }

    static void* memcpy_sse2_prefetch(void* destination, const void* source, size_t num) {
        auto dst = static_cast<uint8_t*>(destination);
        auto src = static_cast<uint8_t const*>(source);

        _mm_prefetch(reinterpret_cast<char const *>(src), _MM_HINT_T0);
        for (; num >= 64; num -= 64, src += 64, dst += 64) {
            _mm_prefetch(reinterpret_cast<char const *>(src + 64), _MM_HINT_T0);
            auto a = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src));
            auto b = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 1);
            auto c = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 2);
            auto d = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 3);
            _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), a);
            _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 1, b);
            _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 2, c);
            _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 3, d);
        }

        for (; num >= 16; num -= 16, src += 16, dst += 16) {
            _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), _mm_loadu_si128(reinterpret_cast<__m128i const*>(src)));
        }

        for (size_t i = 0; i < num; ++i)
            dst[i] = src[i];

        return destination;
    }

    static void* memcpy_sse2_prefetch_2line(void* destination, const void* source, size_t num) {
        auto dst = static_cast<uint8_t*>(destination);
        auto src = static_cast<uint8_t const*>(source);

        _mm_prefetch(reinterpret_cast<char const *>(src), _MM_HINT_T0);
        _mm_prefetch(reinterpret_cast<char const *>(src + 64), _MM_HINT_T0);
        for (; num >= 128; num -= 128, src += 128, dst += 128) {
            _mm_prefetch(reinterpret_cast<char const *>(src + 128), _MM_HINT_T0);
            _mm_prefetch(reinterpret_cast<char const *>(src + 192), _MM_HINT_T0);
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

        for (; num >= 16; num -= 16, src += 16, dst += 16) {
            _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), _mm_loadu_si128(reinterpret_cast<__m128i const*>(src)));
        }

        for (size_t i = 0; i < num; ++i)
            dst[i] = src[i];

        return destination;
    }

    static void* memcpy_sse2_stream(void* destination, const void* source, size_t num) {
        auto dst = static_cast<uint8_t*>(destination);
        auto src = static_cast<uint8_t const*>(source);

        if (num >= 64 + 16) {
            auto alignStep = (16 - (reinterpret_cast<uintptr_t>(dst) & 0xf)) & 0xf;
            for (size_t i = 0; i < alignStep; ++i)
                dst[i] = src[i];
            src += alignStep;
            dst += alignStep;
            num -= alignStep;

            if ((reinterpret_cast<uintptr_t>(src) & 0xf) == 0) {
                for (; num >= 64; num -= 64, src += 64, dst += 64) {
                    auto a = _mm_load_si128(reinterpret_cast<__m128i const*>(src));
                    auto b = _mm_load_si128(reinterpret_cast<__m128i const*>(src) + 1);
                    auto c = _mm_load_si128(reinterpret_cast<__m128i const*>(src) + 2);
                    auto d = _mm_load_si128(reinterpret_cast<__m128i const*>(src) + 3);
                    _mm_stream_si128(reinterpret_cast<__m128i*>(dst), a);
                    _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 1, b);
                    _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 2, c);
                    _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 3, d);
                }
            }
            else {
                for (; num >= 64; num -= 64, src += 64, dst += 64) {
                    auto a = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src));
                    auto b = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 1);
                    auto c = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 2);
                    auto d = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 3);
                    _mm_stream_si128(reinterpret_cast<__m128i*>(dst), a);
                    _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 1, b);
                    _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 2, c);
                    _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 3, d);
                }
            }
            _mm_sfence();
        }

        for (; num >= 16; num -= 16, src += 16, dst += 16) {
            _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), _mm_loadu_si128(reinterpret_cast<__m128i const*>(src)));
        }

        for (size_t i = 0; i < num; ++i)
            dst[i] = src[i];

        return destination;
    }

    __forceinline static void _memcpy32_sse2(uint8_t* dst, uint8_t const* src) {
        auto a = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src));
        auto b = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 1);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), a);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 1, b);
    }

    __forceinline static void _memcpy16_sse2(uint8_t* dst, uint8_t const* src) {
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), _mm_loadu_si128(reinterpret_cast<__m128i const*>(src)));
    }

    static void* memcpy_sse2_fast(void* destination, const void* source, size_t num) {
        auto dst = static_cast<uint8_t*>(destination);
        auto src = static_cast<uint8_t const*>(source);

    _label_switch:
        switch (num) {
        default:
        {
            if (num >= 512 + 16) {
                auto alignStep = (16 - (reinterpret_cast<uintptr_t>(dst) & 0xf)) & 0xf;

                if (alignStep > 0) {
                    _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), _mm_loadu_si128(reinterpret_cast<__m128i const*>(src)));
                }

                src += alignStep;
                dst += alignStep;
                num -= alignStep;

                _mm_prefetch(reinterpret_cast<char const *>(src), _MM_HINT_NTA);
                _mm_prefetch(reinterpret_cast<char const *>(src + 64), _MM_HINT_NTA);
                if ((reinterpret_cast<uintptr_t>(src) & 0xf) == 0) {
                    for (; num >= 128; num -= 128, src += 128, dst += 128) {
                        _mm_prefetch(reinterpret_cast<char const *>(src + 128), _MM_HINT_NTA);
                        _mm_prefetch(reinterpret_cast<char const *>(src + 192), _MM_HINT_NTA);
                        auto a = _mm_load_si128(reinterpret_cast<__m128i const*>(src));
                        auto b = _mm_load_si128(reinterpret_cast<__m128i const*>(src) + 1);
                        auto c = _mm_load_si128(reinterpret_cast<__m128i const*>(src) + 2);
                        auto d = _mm_load_si128(reinterpret_cast<__m128i const*>(src) + 3);
                        auto e = _mm_load_si128(reinterpret_cast<__m128i const*>(src) + 4);
                        auto f = _mm_load_si128(reinterpret_cast<__m128i const*>(src) + 5);
                        auto g = _mm_load_si128(reinterpret_cast<__m128i const*>(src) + 6);
                        auto h = _mm_load_si128(reinterpret_cast<__m128i const*>(src) + 7);
                        _mm_stream_si128(reinterpret_cast<__m128i*>(dst), a);
                        _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 1, b);
                        _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 2, c);
                        _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 3, d);
                        _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 4, e);
                        _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 5, f);
                        _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 6, g);
                        _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 7, h);
                    }
                }
                else {
                    for (; num >= 128; num -= 128, src += 128, dst += 128) {
                        _mm_prefetch(reinterpret_cast<char const *>(src + 128), _MM_HINT_NTA);
                        _mm_prefetch(reinterpret_cast<char const *>(src + 192), _MM_HINT_NTA);
                        auto a = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src));
                        auto b = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 1);
                        auto c = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 2);
                        auto d = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 3);
                        auto e = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 4);
                        auto f = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 5);
                        auto g = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 6);
                        auto h = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 7);
                        _mm_stream_si128(reinterpret_cast<__m128i*>(dst), a);
                        _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 1, b);
                        _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 2, c);
                        _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 3, d);
                        _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 4, e);
                        _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 5, f);
                        _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 6, g);
                        _mm_stream_si128(reinterpret_cast<__m128i*>(dst) + 7, h);
                    }
                }
                _mm_sfence();
            }

            for (; num >= 64; num -= 64, src += 64, dst += 64) {
                auto a = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src));
                auto b = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 1);
                auto c = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 2);
                auto d = _mm_loadu_si128(reinterpret_cast<__m128i const*>(src) + 3);
                _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), a);
                _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 1, b);
                _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 2, c);
                _mm_storeu_si128(reinterpret_cast<__m128i*>(dst) + 3, d);
            }
        }
        goto _label_switch;
        case 63:
            dst[62] = src[62];
        case 62:
            *reinterpret_cast<uint16_t*>(dst + 60) = *reinterpret_cast<uint16_t const*>(src + 60);
        case 60:
            *reinterpret_cast<uint32_t*>(dst + 56) = *reinterpret_cast<uint32_t const*>(src + 56);
        case 56:
            *reinterpret_cast<uint64_t*>(dst + 48) = *reinterpret_cast<uint64_t const*>(src + 48);
        case 48:
            _memcpy16_sse2(dst + 32, src + 32);
        case 32:
            _memcpy32_sse2(dst + 0, src + 0);
        case 0:
            break;

        case 61:
            *reinterpret_cast<uint32_t*>(dst + 57) = *reinterpret_cast<uint32_t const*>(src + 57);
        case 57:
            *reinterpret_cast<uint64_t*>(dst + 49) = *reinterpret_cast<uint64_t const*>(src + 49);
        case 49:
            _memcpy16_sse2(dst + 33, src + 33);
        case 33:
            _memcpy32_sse2(dst + 1, src + 1);
        case 1:
            dst[0] = src[0];
            break;

        case 59:
            dst[58] = src[58];
        case 58:
            *reinterpret_cast<uint64_t*>(dst + 50) = *reinterpret_cast<uint64_t const*>(src + 50);
        case 50:
            _memcpy16_sse2(dst + 34, src + 34);
        case 34:
            _memcpy32_sse2(dst + 2, src + 2);
        case 2:
            *reinterpret_cast<uint16_t*>(dst + 0) = *reinterpret_cast<uint16_t const*>(src + 0);
            break;

        case 55:
            dst[54] = src[54];
        case 54:
            *reinterpret_cast<uint16_t*>(dst + 52) = *reinterpret_cast<uint16_t const*>(src + 52);
        case 52:
            _memcpy16_sse2(dst + 36, src + 36);
        case 36:
            _memcpy32_sse2(dst + 4, src + 4);
        case 4:
            *reinterpret_cast<uint32_t*>(dst + 0) = *reinterpret_cast<uint32_t const*>(src + 0);
            break;

        case 53:
            _memcpy16_sse2(dst + 37, src + 37);
        case 37:
            _memcpy32_sse2(dst + 5, src + 5);
        case 5:
            *reinterpret_cast<uint32_t*>(dst + 0) = *reinterpret_cast<uint32_t const*>(src + 0);
            dst[4] = src[4];
            break;

        case 51:
            _memcpy16_sse2(dst + 35, src + 35);
        case 35:
            _memcpy32_sse2(dst + 3, src + 3);
        case 3:
            *reinterpret_cast<uint16_t*>(dst + 0) = *reinterpret_cast<uint16_t const*>(src + 0);
            dst[2] = src[2];
            break;

        case 47:
            dst[46] = src[46];
        case 46:
            *reinterpret_cast<uint16_t*>(dst + 44) = *reinterpret_cast<uint16_t const*>(src + 44);
        case 44:
            *reinterpret_cast<uint32_t*>(dst + 40) = *reinterpret_cast<uint32_t const*>(src + 40);
        case 40:
            _memcpy32_sse2(dst + 8, src + 8);
        case 8:
            *reinterpret_cast<uint64_t*>(dst + 0) = *reinterpret_cast<uint64_t const*>(src + 0);
            break;

        case 45:
            *reinterpret_cast<uint32_t*>(dst + 41) = *reinterpret_cast<uint32_t const*>(src + 41);
        case 41:
            _memcpy32_sse2(dst + 9, src + 9);
        case 9:
            *reinterpret_cast<uint64_t*>(dst + 0) = *reinterpret_cast<uint64_t const*>(src + 0);
            dst[8] = src[8];
            break;

        case 43:
            dst[42] = src[42];
        case 42:
            _memcpy32_sse2(dst + 10, src + 10);
        case 10:
            *reinterpret_cast<uint64_t*>(dst + 0) = *reinterpret_cast<uint64_t const*>(src + 0);
            *reinterpret_cast<uint16_t*>(dst + 8) = *reinterpret_cast<uint16_t const*>(src + 8);
            break;

        case 39:
            dst[38] = src[38];
        case 38:
            _memcpy32_sse2(dst + 6, src + 6);
        case 6:
            *reinterpret_cast<uint32_t*>(dst + 0) = *reinterpret_cast<uint32_t const*>(src + 0);
            *reinterpret_cast<uint16_t*>(dst + 4) = *reinterpret_cast<uint16_t const*>(src + 4);
            break;

        case 31:
            dst[30] = src[30];
        case 30:
            *reinterpret_cast<uint16_t*>(dst + 28) = *reinterpret_cast<uint16_t const*>(src + 28);
        case 28:
            *reinterpret_cast<uint32_t*>(dst + 24) = *reinterpret_cast<uint32_t const*>(src + 24);
        case 24:
            *reinterpret_cast<uint64_t*>(dst + 16) = *reinterpret_cast<uint64_t const*>(src + 16);
        case 16:
            _memcpy16_sse2(dst + 0, src + 0);
            break;

        case 29:
            *reinterpret_cast<uint32_t*>(dst + 25) = *reinterpret_cast<uint32_t const*>(src + 25);
        case 25:
            *reinterpret_cast<uint64_t*>(dst + 17) = *reinterpret_cast<uint64_t const*>(src + 17);
        case 17:
            _memcpy16_sse2(dst + 0, src + 0);
            dst[16] = src[16];
            break;

        case 27:
            dst[26] = src[26];
        case 26:
            *reinterpret_cast<uint64_t*>(dst + 18) = *reinterpret_cast<uint64_t const*>(src + 18);
        case 18:
            _memcpy16_sse2(dst + 0, src + 0);
            *reinterpret_cast<uint16_t*>(dst + 16) = *reinterpret_cast<uint16_t const*>(src + 16);
            break;

        case 23:
            dst[22] = src[22];
        case 22:
            *reinterpret_cast<uint16_t*>(dst + 20) = *reinterpret_cast<uint16_t const*>(src + 20);
        case 20:
            _memcpy16_sse2(dst + 0, src + 0);
            *reinterpret_cast<uint32_t*>(dst + 16) = *reinterpret_cast<uint32_t const*>(src + 16);
            break;

        case 21:
            _memcpy16_sse2(dst + 0, src + 0);
            *reinterpret_cast<uint32_t*>(dst + 16) = *reinterpret_cast<uint32_t const*>(src + 16);
            dst[20] = src[20];
            break;

        case 19:
            _memcpy16_sse2(dst + 0, src + 0);
            *reinterpret_cast<uint16_t*>(dst + 16) = *reinterpret_cast<uint16_t const*>(src + 16);
            dst[18] = src[18];
            break;

        case 15:
            dst[14] = src[14];
        case 14:
            *reinterpret_cast<uint16_t*>(dst + 12) = *reinterpret_cast<uint16_t const*>(src + 12);
        case 12:
            *reinterpret_cast<uint64_t*>(dst + 0) = *reinterpret_cast<uint64_t const*>(src + 0);
            *reinterpret_cast<uint32_t*>(dst + 8) = *reinterpret_cast<uint32_t const*>(src + 8);
            break;

        case 13:
            *reinterpret_cast<uint64_t*>(dst + 0) = *reinterpret_cast<uint64_t const*>(src + 0);
            *reinterpret_cast<uint32_t*>(dst + 8) = *reinterpret_cast<uint32_t const*>(src + 8);
            dst[12] = src[12];
            break;

        case 11:
            *reinterpret_cast<uint64_t*>(dst + 0) = *reinterpret_cast<uint64_t const*>(src + 0);
            *reinterpret_cast<uint16_t*>(dst + 8) = *reinterpret_cast<uint16_t const*>(src + 8);
            dst[10] = src[10];
            break;

        case 7:
            *reinterpret_cast<uint32_t*>(dst + 0) = *reinterpret_cast<uint32_t const*>(src + 0);
            *reinterpret_cast<uint16_t*>(dst + 4) = *reinterpret_cast<uint16_t const*>(src + 4);
            dst[6] = src[6];
            break;
        }

        return destination;
    }

    __forceinline static void _memcpy32_avx(uint8_t* dst, uint8_t const* src) {
        _mm256_storeu_si256(reinterpret_cast<__m256i*>(dst), _mm256_loadu_si256(reinterpret_cast<__m256i const*>(src)));
    }

    static void* memcpy_avx_fast(void* destination, const void* source, size_t num) {
        auto dst = static_cast<uint8_t*>(destination);
        auto src = static_cast<uint8_t const*>(source);

    _label_switch:
        switch (num) {
        default:
        {
            if (num >= 512 + 32) {
                auto alignStep = (32 - (reinterpret_cast<uintptr_t>(dst) & 0x1f)) & 0x1f;

                if (alignStep > 0) {
                    _mm256_storeu_si256(reinterpret_cast<__m256i*>(dst), _mm256_loadu_si256(reinterpret_cast<__m256i const*>(src)));
                }

                src += alignStep;
                dst += alignStep;
                num -= alignStep;

                _mm_prefetch(reinterpret_cast<char const *>(src), _MM_HINT_NTA);
                _mm_prefetch(reinterpret_cast<char const *>(src + 64), _MM_HINT_NTA);
                if ((reinterpret_cast<uintptr_t>(src) & 0x1f) == 0) {
                    for (; num >= 128; num -= 128, src += 128, dst += 128) {
                        _mm_prefetch(reinterpret_cast<char const *>(src + 128), _MM_HINT_NTA);
                        _mm_prefetch(reinterpret_cast<char const *>(src + 192), _MM_HINT_NTA);
                        auto a = _mm256_stream_load_si256(reinterpret_cast<__m256i const*>(src));
                        auto b = _mm256_stream_load_si256(reinterpret_cast<__m256i const*>(src) + 1);
                        auto c = _mm256_stream_load_si256(reinterpret_cast<__m256i const*>(src) + 2);
                        auto d = _mm256_stream_load_si256(reinterpret_cast<__m256i const*>(src) + 3);
                        _mm256_stream_si256(reinterpret_cast<__m256i*>(dst), a);
                        _mm256_stream_si256(reinterpret_cast<__m256i*>(dst) + 1, b);
                        _mm256_stream_si256(reinterpret_cast<__m256i*>(dst) + 2, c);
                        _mm256_stream_si256(reinterpret_cast<__m256i*>(dst) + 3, d);
                    }
                }
                else {
                    for (; num >= 128; num -= 128, src += 128, dst += 128) {
                        _mm_prefetch(reinterpret_cast<char const *>(src + 128), _MM_HINT_NTA);
                        _mm_prefetch(reinterpret_cast<char const *>(src + 192), _MM_HINT_NTA);
                        auto a = _mm256_loadu_si256(reinterpret_cast<__m256i const*>(src));
                        auto b = _mm256_loadu_si256(reinterpret_cast<__m256i const*>(src) + 1);
                        auto c = _mm256_loadu_si256(reinterpret_cast<__m256i const*>(src) + 2);
                        auto d = _mm256_loadu_si256(reinterpret_cast<__m256i const*>(src) + 3);
                        _mm256_stream_si256(reinterpret_cast<__m256i*>(dst), a);
                        _mm256_stream_si256(reinterpret_cast<__m256i*>(dst) + 1, b);
                        _mm256_stream_si256(reinterpret_cast<__m256i*>(dst) + 2, c);
                        _mm256_stream_si256(reinterpret_cast<__m256i*>(dst) + 3, d);
                    }
                }
                _mm_sfence();
            }

            for (; num >= 64; num -= 64, src += 64, dst += 64) {
                auto a = _mm256_loadu_si256(reinterpret_cast<__m256i const*>(src));
                auto b = _mm256_loadu_si256(reinterpret_cast<__m256i const*>(src) + 1);
                _mm256_storeu_si256(reinterpret_cast<__m256i*>(dst), a);
                _mm256_storeu_si256(reinterpret_cast<__m256i*>(dst) + 1, b);
            }
        }
        goto _label_switch;
        case 63:
            dst[62] = src[62];
        case 62:
            *reinterpret_cast<uint16_t*>(dst + 60) = *reinterpret_cast<uint16_t const*>(src + 60);
        case 60:
            *reinterpret_cast<uint32_t*>(dst + 56) = *reinterpret_cast<uint32_t const*>(src + 56);
        case 56:
            *reinterpret_cast<uint64_t*>(dst + 48) = *reinterpret_cast<uint64_t const*>(src + 48);
        case 48:
            _memcpy16_sse2(dst + 32, src + 32);
        case 32:
            _memcpy32_avx(dst + 0, src + 0);
        case 0:
            break;

        case 61:
            *reinterpret_cast<uint32_t*>(dst + 57) = *reinterpret_cast<uint32_t const*>(src + 57);
        case 57:
            *reinterpret_cast<uint64_t*>(dst + 49) = *reinterpret_cast<uint64_t const*>(src + 49);
        case 49:
            _memcpy16_sse2(dst + 33, src + 33);
        case 33:
            _memcpy32_avx(dst + 1, src + 1);
        case 1:
            dst[0] = src[0];
            break;

        case 59:
            dst[58] = src[58];
        case 58:
            *reinterpret_cast<uint64_t*>(dst + 50) = *reinterpret_cast<uint64_t const*>(src + 50);
        case 50:
            _memcpy16_sse2(dst + 34, src + 34);
        case 34:
            _memcpy32_avx(dst + 2, src + 2);
        case 2:
            *reinterpret_cast<uint16_t*>(dst + 0) = *reinterpret_cast<uint16_t const*>(src + 0);
            break;

        case 55:
            dst[54] = src[54];
        case 54:
            *reinterpret_cast<uint16_t*>(dst + 52) = *reinterpret_cast<uint16_t const*>(src + 52);
        case 52:
            _memcpy16_sse2(dst + 36, src + 36);
        case 36:
            _memcpy32_avx(dst + 4, src + 4);
        case 4:
            *reinterpret_cast<uint32_t*>(dst + 0) = *reinterpret_cast<uint32_t const*>(src + 0);
            break;

        case 53:
            _memcpy16_sse2(dst + 37, src + 37);
        case 37:
            _memcpy32_avx(dst + 5, src + 5);
        case 5:
            *reinterpret_cast<uint32_t*>(dst + 0) = *reinterpret_cast<uint32_t const*>(src + 0);
            dst[4] = src[4];
            break;

        case 51:
            _memcpy16_sse2(dst + 35, src + 35);
        case 35:
            _memcpy32_avx(dst + 3, src + 3);
        case 3:
            *reinterpret_cast<uint16_t*>(dst + 0) = *reinterpret_cast<uint16_t const*>(src + 0);
            dst[2] = src[2];
            break;

        case 47:
            dst[46] = src[46];
        case 46:
            *reinterpret_cast<uint16_t*>(dst + 44) = *reinterpret_cast<uint16_t const*>(src + 44);
        case 44:
            *reinterpret_cast<uint32_t*>(dst + 40) = *reinterpret_cast<uint32_t const*>(src + 40);
        case 40:
            _memcpy32_avx(dst + 8, src + 8);
        case 8:
            *reinterpret_cast<uint64_t*>(dst + 0) = *reinterpret_cast<uint64_t const*>(src + 0);
            break;

        case 45:
            *reinterpret_cast<uint32_t*>(dst + 41) = *reinterpret_cast<uint32_t const*>(src + 41);
        case 41:
            _memcpy32_avx(dst + 9, src + 9);
        case 9:
            *reinterpret_cast<uint64_t*>(dst + 0) = *reinterpret_cast<uint64_t const*>(src + 0);
            dst[8] = src[8];
            break;

        case 43:
            dst[42] = src[42];
        case 42:
            _memcpy32_avx(dst + 10, src + 10);
        case 10:
            *reinterpret_cast<uint64_t*>(dst + 0) = *reinterpret_cast<uint64_t const*>(src + 0);
            *reinterpret_cast<uint16_t*>(dst + 8) = *reinterpret_cast<uint16_t const*>(src + 8);
            break;

        case 39:
            dst[38] = src[38];
        case 38:
            _memcpy32_avx(dst + 6, src + 6);
        case 6:
            *reinterpret_cast<uint32_t*>(dst + 0) = *reinterpret_cast<uint32_t const*>(src + 0);
            *reinterpret_cast<uint16_t*>(dst + 4) = *reinterpret_cast<uint16_t const*>(src + 4);
            break;

        case 31:
            dst[30] = src[30];
        case 30:
            *reinterpret_cast<uint16_t*>(dst + 28) = *reinterpret_cast<uint16_t const*>(src + 28);
        case 28:
            *reinterpret_cast<uint32_t*>(dst + 24) = *reinterpret_cast<uint32_t const*>(src + 24);
        case 24:
            *reinterpret_cast<uint64_t*>(dst + 16) = *reinterpret_cast<uint64_t const*>(src + 16);
        case 16:
            _memcpy16_sse2(dst + 0, src + 0);
            break;

        case 29:
            *reinterpret_cast<uint32_t*>(dst + 25) = *reinterpret_cast<uint32_t const*>(src + 25);
        case 25:
            *reinterpret_cast<uint64_t*>(dst + 17) = *reinterpret_cast<uint64_t const*>(src + 17);
        case 17:
            _memcpy16_sse2(dst + 0, src + 0);
            dst[16] = src[16];
            break;

        case 27:
            dst[26] = src[26];
        case 26:
            *reinterpret_cast<uint64_t*>(dst + 18) = *reinterpret_cast<uint64_t const*>(src + 18);
        case 18:
            _memcpy16_sse2(dst + 0, src + 0);
            *reinterpret_cast<uint16_t*>(dst + 16) = *reinterpret_cast<uint16_t const*>(src + 16);
            break;

        case 23:
            dst[22] = src[22];
        case 22:
            *reinterpret_cast<uint16_t*>(dst + 20) = *reinterpret_cast<uint16_t const*>(src + 20);
        case 20:
            _memcpy16_sse2(dst + 0, src + 0);
            *reinterpret_cast<uint32_t*>(dst + 16) = *reinterpret_cast<uint32_t const*>(src + 16);
            break;

        case 21:
            _memcpy16_sse2(dst + 0, src + 0);
            *reinterpret_cast<uint32_t*>(dst + 16) = *reinterpret_cast<uint32_t const*>(src + 16);
            dst[20] = src[20];
            break;

        case 19:
            _memcpy16_sse2(dst + 0, src + 0);
            *reinterpret_cast<uint16_t*>(dst + 16) = *reinterpret_cast<uint16_t const*>(src + 16);
            dst[18] = src[18];
            break;

        case 15:
            dst[14] = src[14];
        case 14:
            *reinterpret_cast<uint16_t*>(dst + 12) = *reinterpret_cast<uint16_t const*>(src + 12);
        case 12:
            *reinterpret_cast<uint64_t*>(dst + 0) = *reinterpret_cast<uint64_t const*>(src + 0);
            *reinterpret_cast<uint32_t*>(dst + 8) = *reinterpret_cast<uint32_t const*>(src + 8);
            break;

        case 13:
            *reinterpret_cast<uint64_t*>(dst + 0) = *reinterpret_cast<uint64_t const*>(src + 0);
            *reinterpret_cast<uint32_t*>(dst + 8) = *reinterpret_cast<uint32_t const*>(src + 8);
            dst[12] = src[12];
            break;

        case 11:
            *reinterpret_cast<uint64_t*>(dst + 0) = *reinterpret_cast<uint64_t const*>(src + 0);
            *reinterpret_cast<uint16_t*>(dst + 8) = *reinterpret_cast<uint16_t const*>(src + 8);
            dst[10] = src[10];
            break;

        case 7:
            *reinterpret_cast<uint32_t*>(dst + 0) = *reinterpret_cast<uint32_t const*>(src + 0);
            *reinterpret_cast<uint16_t*>(dst + 4) = *reinterpret_cast<uint16_t const*>(src + 4);
            dst[6] = src[6];
            break;
        }

        return destination;
    }

}

#endif
