#ifndef MULTIPLE_PRECISION_OP_H
#define MULTIPLE_PRECISION_OP_H


#include <cstdint>

#include <vector>


size_t constexpr kInternalBaseBits = 32;
uint64_t constexpr kInternalBase = 1ULL << kInternalBaseBits;


namespace MultiplePrecisionOp {

    enum MultiplicationAlgorithm : uint8_t {
        MA_Karatsuba,
        MA_FFT,
        MA_NTT,
        MA_NTT2,
    };

    void ResetConfigurations();
    void SetAlgorithmSwitchingThreashold(MultiplicationAlgorithm algorithm, size_t threashold);

    uint32_t DecimalsToDisplayDigit(char const *decimals, size_t size);
    size_t DisplayDigitToDecimals(uint32_t digit, char *decimals, size_t size, bool fillZero);

    size_t DetermineSize(uint32_t const *digits, size_t size);
    std::vector<uint32_t>& ShrinkToFit(std::vector<uint32_t> &digits);

    bool Add(uint32_t *digits0, size_t size0, uint32_t const *digits1, size_t size1);
    std::vector<uint32_t> Add(std::vector<uint32_t> const &digits0, std::vector<uint32_t> const &digits1);

    bool Substract(uint32_t *digits0, size_t size0, uint32_t const *digits1, size_t size1);
    std::vector<uint32_t> Substract(std::vector<uint32_t> const &digits0, std::vector<uint32_t> const &digits1);

    std::vector<uint32_t> Multiply_Karatsuba(std::vector<uint32_t> const &digits0, std::vector<uint32_t> const &digits1);
    std::vector<uint32_t> Multiply_FFT(std::vector<uint32_t> const &digits0, std::vector<uint32_t> const &digits1);
    std::vector<uint32_t> Multiply_NTT(std::vector<uint32_t> const &digits0, std::vector<uint32_t> const &digits1);
    void Multiply_NTT2(uint32_t *output, size_t osize, uint32_t const *digits0, size_t size0, uint32_t const *digits1, size_t size1);
    std::vector<uint32_t> Multiply_NTT2(std::vector<uint32_t> const &digits0, std::vector<uint32_t> const &digits1);
    std::vector<uint32_t> FastMultiply(std::vector<uint32_t> const &digits0, std::vector<uint32_t> const &digits1);

    std::vector<uint32_t> ChangeBase(std::vector<uint32_t> srcDigits, uint64_t srcBase, uint64_t destBase);
}


#endif