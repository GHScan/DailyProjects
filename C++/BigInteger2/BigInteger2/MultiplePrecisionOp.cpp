
#include "MultiplePrecisionOp.h"
#include "FFT.h"
#include "NTT.h"
#include "NTT2.h"


namespace MultiplePrecisionOp {

    static size_t gMultiplicationSwitchingThreashold[4];

    static class Initializer final {
    public:
        Initializer() {
            ResetConfigurations();
        }
    } gInitializer;

    void ResetConfigurations() {
        gMultiplicationSwitchingThreashold[MA_Karatsuba] = 80;
        gMultiplicationSwitchingThreashold[MA_FFT] = 3000;
        gMultiplicationSwitchingThreashold[MA_NTT] = 3000;
        gMultiplicationSwitchingThreashold[MA_NTT2] = 10000;
    }

    void SetAlgorithmSwitchingThreashold(MultiplicationAlgorithm algorithm, size_t threashold) {
        gMultiplicationSwitchingThreashold[algorithm] = threashold;
    }

    uint32_t DecimalsToDisplayDigit(char const *decimals, size_t size) {
        uint32_t digit = 0;
        for (size_t i = 0; i < size; ++i) {
            ASSERT(decimals[i] >= '0' && decimals[i] <= '9');
            digit = digit * 10 + (decimals[i] - '0');
        }
        return digit;
    }

    size_t DisplayDigitToDecimals(uint32_t digit, char *decimals, size_t size, bool fillZero) {
        auto space = size;
        do {
            decimals[--space] = '0' + digit % 10;
            digit /= 10;
        } while (digit > 0);
        if (fillZero) {
            while (space > 0) decimals[--space] = '0';
        } else {
            Memcpy(decimals, decimals + space, size - space);
        }
        return size - space;
    }

    size_t DetermineSize(uint32_t const *digits, size_t size) {
        for (; size > 1 && digits[size - 1] == 0; --size);
        return size;
    }

    std::vector<uint32_t>& ShrinkToFit(std::vector<uint32_t> &digits) {
        digits.resize(DetermineSize(&digits[0], digits.size()));
        return digits;
    }

    static uint64_t Carry(uint32_t *digits, size_t size, uint64_t carry) {
        for (size_t i = 0; carry != 0 && i < size; ++i) {
            carry = carry + digits[i];
            digits[i] = static_cast<uint32_t>(carry);
            carry >>= kInternalBaseBits;
        }
        return carry;
    }

    static uint64_t Carry_B(uint32_t *digits, size_t size, uint64_t carry, uint64_t base) {
        for (size_t i = 0; carry != 0 && i < size; ++i) {
            carry = carry + digits[i];
            if (carry >= base) {
                digits[i] = static_cast<uint32_t>(carry - base);
                carry = 1;
            } else {
                digits[i] = static_cast<uint32_t>(carry);
                carry = 0;
            }
        }
        return carry;
    }

    static uint64_t Borrow(uint32_t *digits, size_t size, uint64_t carry) {
        for (size_t i = 0; carry != kInternalBase && i < size; ++i) {
            carry = carry + digits[i];
            digits[i] = static_cast<uint32_t>(carry);
            carry = kInternalBase - 1 + (carry >> kInternalBaseBits);
        }
        return carry;
    }

    static uint64_t Borrow_B(uint32_t *digits, size_t size, uint64_t carry, uint64_t base) {
        for (size_t i = 0; carry != 0 && i < size; ++i) {
            carry = carry + digits[i];
            if (carry >= base) {
                digits[i] = static_cast<uint32_t>(carry - base);
                carry = base;
            } else {
                digits[i] = static_cast<uint32_t>(carry);
                carry = base - 1;
            }
        }
        return carry;
    }

    bool Add(
        uint32_t *digits0, size_t size0,
        uint32_t const *digits1, size_t size1) {

        ASSERT(size0 >= size1);

        uint64_t carry = 0;
        for (size_t i = 0; i < size1; ++i) {
            carry = carry + digits0[i] + digits1[i];
            digits0[i] = static_cast<uint32_t>(carry);
            carry >>= kInternalBaseBits;
        }

        return Carry(digits0 + size1, size0 - size1, carry) == 1;
    }

    bool Add_B(
        uint32_t *digits0, size_t size0,
        uint32_t const *digits1, size_t size1,
        uint64_t base) {

        ASSERT(size0 >= size1);

        uint64_t carry = 0;
        for (size_t i = 0; i < size1; ++i) {
            carry = carry + digits0[i] + digits1[i];
            if (carry >= base) {
                digits0[i] = static_cast<uint32_t>(carry - base);
                carry = 1;
            } else {
                digits0[i] = static_cast<uint32_t>(carry);
                carry = 0;
            }
        }

        return Carry_B(digits0 + size1, size0 - size1, carry, base) == 1;
    }

    std::vector<uint32_t> Add(std::vector<uint32_t> const &digits0, std::vector<uint32_t> const &digits1) {
        if (digits0.size() < digits1.size()) return Add(digits1, digits0);

        std::vector<uint32_t> output;
        output.reserve(digits0.size() + 1);
        output.assign(digits0.begin(), digits0.end());
        if (Add(&output[0], output.size(), &digits1[0], digits1.size()))
            output.push_back(1);

        return output;
    }

    bool Substract(
        uint32_t *digits0, size_t size0,
        uint32_t const *digits1, size_t size1) {

        ASSERT(size0 >= size1);

        uint64_t carry = kInternalBase;
        for (size_t i = 0; i < size1; ++i) {
            carry = carry - digits1[i] + digits0[i];
            digits0[i] = static_cast<uint32_t>(carry);
            carry = kInternalBase - 1 + (carry >> kInternalBaseBits);
        }
        return Borrow(digits0 + size1, size0 - size1, carry) == kInternalBase - 1;
    }

    bool Substract_B(
        uint32_t *digits0, size_t size0,
        uint32_t const *digits1, size_t size1,
        uint64_t base) {

        ASSERT(size0 >= size1);

        uint64_t carry = base;
        for (size_t i = 0; i < size1; ++i) {
            carry = carry - digits1[i] + digits0[i];
            if (carry >= base) {
                digits0[i] = static_cast<uint32_t>(carry - base);
                carry = base;
            }
            else {
                digits0[i] = static_cast<uint32_t>(carry);
                carry = base - 1;
            }
        }
        return Borrow_B(digits0 + size1, size0 - size1, carry, base) == base - 1;
    }

    std::vector<uint32_t> Substract(std::vector<uint32_t> const &digits0, std::vector<uint32_t> const &digits1) {
        std::vector<uint32_t> output(digits0);

        if (Substract(&output[0], output.size(), &digits1[0], digits1.size()))
            ASSERT(0);
        return ShrinkToFit(output);
    }

    inline uint64_t Multiply4(uint32_t output[4], uint32_t const digits0[4], uint64_t digit1, uint64_t carry) {
        uint64_t am[] = {
            digits0[0] * digit1 + output[0],
            digits0[1] * digit1 + output[1],
            digits0[2] * digit1 + output[2],
            digits0[3] * digit1 + output[3],
        };

        carry += am[0];
        output[0] = static_cast<uint32_t>(carry);
        carry >>= kInternalBaseBits;

        carry += am[1];
        output[1] = static_cast<uint32_t>(carry);
        carry >>= kInternalBaseBits;

        carry += am[2];
        output[2] = static_cast<uint32_t>(carry);
        carry >>= kInternalBaseBits;

        carry += am[3];
        output[3] = static_cast<uint32_t>(carry);
        carry >>= kInternalBaseBits;

        return carry;
    }

    static void Multiply(
        uint32_t *output, size_t osize,
        uint32_t const *digits0, size_t size0,
        uint32_t const *digits1, size_t size1) {

        if (size0 < size1) 
            return Multiply(output, osize, digits1, size1, digits0, size0);

        ASSERT(osize >= size0 + size1);

        if (size1 == 1 && digits1[0] == 0) return;

        size_t i1 = 0;

        for (; i1 + 4 <= size1; i1 += 4) {
            
            uint64_t carry0 = output[i1 + 0];
            uint64_t carry1 = output[i1 + 1];
            uint64_t carry2 = output[i1 + 2];
            uint64_t carry3 = output[i1 + 3];

            size_t io = i1;
            for (size_t i0 = 0; i0 < size0; ++i0, ++io) {
                uint64_t d0 = digits0[i0];

                carry0 += d0 * digits1[i1 + 0];
                carry1 += d0 * digits1[i1 + 1];
                carry2 += d0 * digits1[i1 + 2];
                carry3 += d0 * digits1[i1 + 3];

                uint64_t carryLow0 = static_cast<uint32_t>(carry0);
                uint64_t carryLow1 = static_cast<uint32_t>(carry1);
                uint64_t carryLow2 = static_cast<uint32_t>(carry2);
                uint64_t carryLow3 = static_cast<uint32_t>(carry3);

                uint64_t carryHi0 = carry0 >> kInternalBaseBits;
                uint64_t carryHi1 = carry1 >> kInternalBaseBits;
                uint64_t carryHi2 = carry2 >> kInternalBaseBits;
                uint64_t carryHi3 = carry3 >> kInternalBaseBits;

                output[io] = static_cast<uint32_t>(carryLow0);
                carry0 = carryHi0 + carryLow1;
                carry1 = carryHi1 + carryLow2;
                carry2 = carryHi2 + carryLow3;
                carry3 = carryHi3 + output[io + 4];
            }

            uint64_t carry = 0;
            carry += carry0;
            output[io++] = static_cast<uint32_t>(carry);
            carry >>= kInternalBaseBits;
            carry += carry1;
            output[io++] = static_cast<uint32_t>(carry);
            carry >>= kInternalBaseBits;
            carry += carry2;
            output[io++] = static_cast<uint32_t>(carry);
            carry >>= kInternalBaseBits;
            carry += carry3;
            output[io++] = static_cast<uint32_t>(carry);
            carry >>= kInternalBaseBits;

            carry = Carry(output + io, osize - io, carry);
            ASSERT(carry == 0);
        }

        for (; i1 < size1; ++i1) {
            uint64_t digit1 = digits1[i1];
            uint64_t carry = 0;

            size_t i0 = 0, io = i1;
            for (; i0 + 4 <= size0; i0 += 4, io += 4)
                carry = Multiply4(output + io, digits0 + i0, digit1, carry);
            for (; i0 < size0; ++i0, ++io) {
                carry = carry + digit1 * digits0[i0] + output[io];
                output[io] = static_cast<uint32_t>(carry);
                carry >>= kInternalBaseBits;
            }

            carry = Carry(output + io, osize - io, carry);
            ASSERT(carry == 0);
        }
    }

    std::vector<uint32_t> Multiply(
        std::vector<uint32_t> const &digits0, std::vector<uint32_t> const &digits1) {

        std::vector<uint32_t> output(digits0.size() + digits1.size());
        Multiply(&output[0], output.size(), &digits0[0], digits0.size(), &digits1[0], digits1.size());
        return ShrinkToFit(output);
    }

    static void Multiply_B(
        uint32_t *output, size_t osize,
        uint32_t const *digits0, size_t size0,
        uint32_t const *digits1, size_t size1,
        uint64_t base) {

        if (size0 < size1) 
            return Multiply_B(output, osize, digits1, size1, digits0, size0, base);

        ASSERT(osize >= size0 + size1);

        if (size1 == 1 && digits1[0] == 0) return;

        for (size_t i1 = 0; i1 < size1; ++i1) {
            uint64_t digit1 = digits1[i1];
            uint64_t carry = 0;

            size_t io = i1;
            for (size_t i0 = 0; i0 < size0; ++i0, ++io) {
                carry = carry + digit1 * digits0[i0] + output[io];
                output[io] = static_cast<uint32_t>(carry % base);
                carry /= base;
            }

            carry = Carry_B(output + io, osize - io, carry, base);
            ASSERT(carry == 0);
        }
    }

    static void Multiply_Karatsuba(
        uint32_t *output, size_t osize,
        uint32_t const *digits0, size_t size0,
        uint32_t const *digits1, size_t size1) {

        if (size0 < size1) 
            return Multiply_Karatsuba(output, osize, digits1, size1, digits0, size0);

        ASSERT(osize >= size0 + size1);

        if (size1 < gMultiplicationSwitchingThreashold[MA_Karatsuba]) {
            return Multiply(output, osize, digits0, size0, digits1, size1);
        }

        size_t half = (size0 + 1) / 2;

        std::vector<uint32_t> bufVec((half + 1) * 4);
        uint32_t *buf0 = &bufVec[0], *buf1 = &bufVec[0] + half + 1, *buf2 = &bufVec[0] + 2 * half + 2;
        uint32_t zeroDigits[] = { 0 };

        auto digits0Low = digits0, digits0Hi = digits0 + half;
        auto size0Low = half, size0Hi = size0 - half;
        auto digits1Low = digits1, digits1Hi = size1 <= half ? zeroDigits : digits1 + half;
        auto size1Low = std::min(half, size1), size1Hi = size1 <= half ? 1 : size1 - half;


        Memcpy(buf0, digits0Low, size0Low);
        if (Add(buf0, size0Low, digits0Hi, size0Hi))
            buf0[size0Low] = 1;
        else
            buf0[size0Low] = 0;
        Memcpy(buf1, digits1Low, size1Low);
        if (Add(buf1, size1Low, digits1Hi, size1Hi))
            buf1[size1Low] = 1;
        else
            buf1[size1Low] = 0;
        Multiply_Karatsuba(buf2, 2 * half + 2, buf0, size0Low + 1, buf1, size1Low + 1);


        Memset(buf0, 0, size0Low + size1Low);
        Multiply_Karatsuba(buf0, size0Low + size1Low, digits0Low, size0Low, digits1Low, size1Low);
        if (Add(output, osize, buf0, size0Low + size1Low))
            ASSERT(0);
        if (Substract(buf2, 2 * half + 2, buf0, size0Low + size1Low))
            ASSERT(0);


        if (digits1Hi != zeroDigits)
        {
            Memset(buf0, 0, size0Hi + size1Hi);
            Multiply_Karatsuba(buf0, size0Hi + size1Hi, digits0Hi, size0Hi, digits1Hi, size1Hi);
            if (Add(output + 2 * half, osize - 2 * half, buf0, size0Hi + size1Hi))
                ASSERT(0);
            if (Substract(buf2, 2 * half + 2, buf0, size0Hi + size1Hi))
                ASSERT(0);
        }
        

        if (Add(output + half, osize - half, buf2, DetermineSize(buf2, 2 * half + 2)))
            ASSERT(0);
    }

    std::vector<uint32_t> Multiply_Karatsuba(
        std::vector<uint32_t> const &digits0, std::vector<uint32_t> const &digits1) {

        std::vector<uint32_t> output(digits0.size() + digits1.size());
        Multiply_Karatsuba(&output[0], output.size(), &digits0[0], digits0.size(), &digits1[0], digits1.size());
        return ShrinkToFit(output);
    }

    static void Multiply_Karatsuba_B(
        uint32_t *output, size_t osize,
        uint32_t const *digits0, size_t size0,
        uint32_t const *digits1, size_t size1,
        uint64_t base) {

        if (size0 < size1) 
            return Multiply_Karatsuba_B(output, osize, digits1, size1, digits0, size0, base);

        ASSERT(osize >= size0 + size1);

        if (size1 < gMultiplicationSwitchingThreashold[MA_Karatsuba]) {
            return Multiply_B(output, osize, digits0, size0, digits1, size1, base);
        }

        size_t half = (size0 + 1) / 2;

        std::vector<uint32_t> bufVec((half + 1) * 4);
        uint32_t *buf0 = &bufVec[0], *buf1 = &bufVec[0] + half + 1, *buf2 = &bufVec[0] + 2 * half + 2;
        uint32_t zeroDigits[] = { 0 };

        auto digits0Low = digits0, digits0Hi = digits0 + half;
        auto size0Low = half, size0Hi = size0 - half;
        auto digits1Low = digits1, digits1Hi = size1 <= half ? zeroDigits : digits1 + half;
        auto size1Low = std::min(half, size1), size1Hi = size1 <= half ? 1 : size1 - half;

        
        Memcpy(buf0, digits0Low, size0Low);
        if (Add_B(buf0, size0Low, digits0Hi, size0Hi, base))
            buf0[size0Low] = 1;
        else
            buf0[size0Low] = 0;
        Memcpy(buf1, digits1Low, size1Low);
        if (Add_B(buf1, size1Low, digits1Hi, size1Hi, base))
            buf1[size1Low] = 1;
        else
            buf1[size1Low] = 0;
        Multiply_Karatsuba_B(buf2, 2 * half + 2, buf0, size0Low + 1, buf1, size1Low + 1, base);


        Memset(buf0, 0, size0Low + size1Low);
        Multiply_Karatsuba_B(buf0, size0Low + size1Low, digits0Low, size0Low, digits1Low, size1Low, base);
        if (Add_B(output, osize, buf0, size0Low + size1Low, base))
            ASSERT(0);
        if (Substract_B(buf2, 2 * half + 2, buf0, size0Low + size1Low, base))
            ASSERT(0);


        if (digits1Hi != zeroDigits)
        {
            Memset(buf0, 0, size0Hi + size1Hi);
            Multiply_Karatsuba_B(buf0, size0Hi + size1Hi, digits0Hi, size0Hi, digits1Hi, size1Hi, base);
            if (Add_B(output + 2 * half, osize - 2 * half, buf0, size0Hi + size1Hi, base))
                ASSERT(0);
            if (Substract_B(buf2, 2 * half + 2, buf0, size0Hi + size1Hi, base))
                ASSERT(0);
        }


        if (Add_B(output + half, osize - half, buf2, DetermineSize(buf2, 2 * half + 2), base))
            ASSERT(0);
    }

    static void Multiply_FFT_u16(
        uint32_t *output, size_t osize,
        uint32_t const *digits0, size_t size0,
        uint32_t const *digits1, size_t size1) {

        ASSERT(osize >= size0 + size1);

        std::vector<uint64_t> convolveOutput((size0 + size1) * 2);

        Convolve_FFT(
            &convolveOutput[0], convolveOutput.size(),
            reinterpret_cast<uint16_t const*>(digits0), size0 * 2,
            reinterpret_cast<uint16_t const*>(digits1), size1 * 2,
            [](uint16_t v) { return v; },
            [](std::complex<FFTFloat> &v)
        {
            auto re = llround(v.real());
            ASSERT(re >= 0 && llround(v.imag()) == 0);
            return re;
        });

        uint64_t carry = 0;
        for (size_t i = 0; i < convolveOutput.size(); i += 2) {
            carry += (convolveOutput[i] & 0xffffffff) + ((convolveOutput[i + 1] & 0xffff) << 16) + output[i >> 1];
            output[i >> 1] = static_cast<uint32_t>(carry);
            carry >>= kInternalBaseBits;
            carry += (convolveOutput[i] >> 32) + (convolveOutput[i + 1] >> 16);
        }

        carry = Carry(output + convolveOutput.size() / 2, osize - convolveOutput.size() / 2, carry);
        ASSERT(carry == 0);
    }

    static void Multiply_FFT_u8(
        uint32_t *output, size_t osize,
        uint32_t const *digits0, size_t size0,
        uint32_t const *digits1, size_t size1) {

        ASSERT(osize >= size0 + size1);

        std::vector<uint64_t> convolveOutput((size0 + size1) * 4);

        Convolve_FFT(
            &convolveOutput[0], convolveOutput.size(),
            reinterpret_cast<uint8_t const*>(digits0), size0 * 4,
            reinterpret_cast<uint8_t const*>(digits1), size1 * 4,
            [](uint8_t v) { return v; },
            [](std::complex<FFTFloat> &v)
        {
            auto re = llround(v.real());
            ASSERT(re >= 0 && llround(v.imag()) == 0);
            return re;
        });


        uint64_t carry = 0;
        for (size_t i = 0; i < convolveOutput.size(); i += 4) {
            carry += output[i >> 2];
            carry += (convolveOutput[i + 0] & 0xffffffff) << 0;
            carry += (convolveOutput[i + 1] & 0x00ffffff) << 8;
            carry += (convolveOutput[i + 2] & 0x0000ffff) << 16;
            carry += (convolveOutput[i + 3] & 0x000000ff) << 24;
            output[i >> 2] = static_cast<uint32_t>(carry);
            carry >>= kInternalBaseBits;
            carry += (convolveOutput[i + 0] >> 32);
            carry += (convolveOutput[i + 1] >> 24);
            carry += (convolveOutput[i + 2] >> 16);
            carry += (convolveOutput[i + 3] >> 8);
        }

        carry = Carry(output + convolveOutput.size() / 4, osize - convolveOutput.size() / 4, carry);
        ASSERT(carry == 0);
    }

    static void Multiply_FFT(
        uint32_t *output, size_t osize,
        uint32_t const *digits0, size_t size0,
        uint32_t const *digits1, size_t size1) {

        if (CanConvolveIntegersAccurately(
            reinterpret_cast<uint16_t const*>(digits0), size0 * 2,
            reinterpret_cast<uint16_t const*>(digits1), size1 * 2)) {
            return Multiply_FFT_u16(output, osize, digits0, size0, digits1, size1);
        }

        ASSERT(CanConvolveIntegersAccurately(
            reinterpret_cast<uint8_t const*>(digits0), size0 * 4,
            reinterpret_cast<uint8_t const*>(digits1), size1 * 4));
        return Multiply_FFT_u8(output, osize, digits0, size0, digits1, size1);
    }

    std::vector<uint32_t> Multiply_FFT(
        std::vector<uint32_t> const &digits0, std::vector<uint32_t> const &digits1) {

        std::vector<uint32_t> output(digits0.size() + digits1.size());
        Multiply_FFT(&output[0], output.size(), &digits0[0], digits0.size(), &digits1[0], digits1.size());
        return ShrinkToFit(output);
    }

    static void Multiply_NTT(
        uint32_t *output, size_t osize,
        uint32_t const *digits0, size_t size0,
        uint32_t const *digits1, size_t size1) {

        ASSERT(osize >= size0 + size1);

        std::vector<uint64_t> convolveOutput((size0 + size1) * 2);

        Convolve_NTT(
            &convolveOutput[0], convolveOutput.size(),
            reinterpret_cast<uint16_t const*>(digits0), size0 * 2,
            reinterpret_cast<uint16_t const*>(digits1), size1 * 2,
            [](uint16_t v) { return v; },
            [](uint64_t &v) { return v; });

        uint64_t carry = 0;
        for (size_t i = 0; i < convolveOutput.size(); i += 2) {
            carry += (convolveOutput[i] & 0xffffffff) + ((convolveOutput[i + 1] & 0xffff) << 16) + output[i >> 1];
            output[i >> 1] = static_cast<uint32_t>(carry);
            carry >>= kInternalBaseBits;
            carry += (convolveOutput[i] >> 32) + (convolveOutput[i + 1] >> 16);
        }

        carry = Carry(output + convolveOutput.size() / 2, osize - convolveOutput.size() / 2, carry);
        ASSERT(carry == 0);
    }

    std::vector<uint32_t> Multiply_NTT(
        std::vector<uint32_t> const &digits0, std::vector<uint32_t> const &digits1) {

        std::vector<uint32_t> output(digits0.size() + digits1.size());
        Multiply_NTT(&output[0], output.size(), &digits0[0], digits0.size(), &digits1[0], digits1.size());
        return ShrinkToFit(output);
    }

    void Multiply_NTT2(
        uint32_t *output, size_t osize, 
        uint32_t const *digits0, size_t size0, 
        uint32_t const *digits1, size_t size1) {

        if (size0 < size1) 
            return Multiply_NTT2(output, osize, digits1, size1, digits0, size0);

        ASSERT(osize >= size0 + size1);

        if (size1 < gMultiplicationSwitchingThreashold[MA_NTT2]) 
            return Multiply_Karatsuba(output, osize, digits0, size0, digits1, size1);

        size_t rawNumberSize, ringNumberSize;
        if (!EstimateNTT2NumberSize(size0, size1, rawNumberSize, ringNumberSize))
            return Multiply_Karatsuba(output, osize, digits0, size0, digits1, size1);

        std::vector<uint32_t> convolveOutput(
            ((size0 + rawNumberSize - 1) / rawNumberSize + (size1 + rawNumberSize - 1) / rawNumberSize) * ringNumberSize);

        Convolve_NTT2(
            &convolveOutput[0], convolveOutput.size(), 
            digits0, size0, digits1, size1, 
            rawNumberSize, ringNumberSize);

        for (size_t i = 0, io = 0; i < convolveOutput.size() && io < osize; i += ringNumberSize, io += rawNumberSize) {
            if (Add(output + io, osize - io, &convolveOutput[i], DetermineSize(&convolveOutput[i], ringNumberSize)))
                ASSERT(0);
        }
    }

    std::vector<uint32_t> Multiply_NTT2(std::vector<uint32_t> const &digits0, std::vector<uint32_t> const &digits1) {

        std::vector<uint32_t> output(digits0.size() + digits1.size());
        Multiply_NTT2(&output[0], output.size(), &digits0[0], digits0.size(), &digits1[0], digits1.size());
        return ShrinkToFit(output);
    }

    std::vector<uint32_t> FastMultiply(
        std::vector<uint32_t> const &digits0, std::vector<uint32_t> const &digits1) {

        if (digits0.size() < digits1.size()) 
            return FastMultiply(digits1, digits0);

        if (digits1.size() >= gMultiplicationSwitchingThreashold[MA_NTT2])
            return Multiply_NTT2(digits0, digits1);
//#ifdef _MSC_VER
//        if (digits1.size() > gMultiplicationSwitchingThreashold[MA_FFT])
//            return Multiply_FFT(digits0, digits1);
//#else
//        if (digits1.size() > gMultiplicationSwitchingThreashold[MA_NTT])
//            return Multiply_NTT(digits0, digits1);
//#endif
        if (digits1.size() >= gMultiplicationSwitchingThreashold[MA_Karatsuba])
            return Multiply_Karatsuba(digits0, digits1);

        return Multiply(digits0, digits1);
    }

    static std::vector<uint32_t> ChangeBase_Classic(std::vector<uint32_t> srcDigits, uint64_t srcBase, uint64_t destBase) {

        std::vector<uint32_t> destDigits;
        destDigits.reserve(static_cast<size_t>(ceil(log(double(srcBase)) / log(double(destBase)) * srcDigits.size())));

        do {
            uint64_t remainder = 0;
            for (auto i = srcDigits.size(); i > 0; --i) {
                remainder = remainder * srcBase + srcDigits[i - 1];
                srcDigits[i - 1] = static_cast<uint32_t>(remainder / destBase);
                remainder = remainder % destBase;
            }

            destDigits.push_back(static_cast<uint32_t>(remainder));

            for (; srcDigits.size() > 1 && srcDigits.back() == 0; srcDigits.pop_back());
            if (srcDigits.size() == 1 && srcDigits.back() == 0) break;

        } while (true);

        return destDigits;
    }

    std::vector<uint32_t> ChangeBase(std::vector<uint32_t> srcDigits, uint64_t srcBase, uint64_t destBase) {
        if (srcDigits.size() < 2 * gMultiplicationSwitchingThreashold[MA_Karatsuba]) 
            return ChangeBase_Classic(move(srcDigits), srcBase, destBase);

        auto half = (srcDigits.size() + 1) / 2;

        std::vector<uint32_t> temp(half + 1);
        temp.assign(srcDigits.begin() + half, srcDigits.end());
        auto hiResult = ChangeBase(temp, srcBase, destBase);

        temp.assign(half, 0);
        temp.push_back(1);
        auto hiBaseResult = ChangeBase(move(temp), srcBase, destBase);

        srcDigits.resize(half);
        auto loResult = ChangeBase(srcDigits, srcBase, destBase);

        std::vector<uint32_t> destDigits(hiResult.size() + hiBaseResult.size() + 1);

        Multiply_Karatsuba_B(
            &destDigits[0], destDigits.size(), &hiResult[0], hiResult.size(), &hiBaseResult[0], hiBaseResult.size(), destBase);

        if (Add_B(&destDigits[0], destDigits.size(), &loResult[0], loResult.size(), destBase))
            ASSERT(0);

        return ShrinkToFit(destDigits);
    }
}