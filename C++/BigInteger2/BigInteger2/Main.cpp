#include <vector>
#include <algorithm>
#include <sstream>


#define USE_GMP 1

#if USE_GMP
#include <mpirxx.h>
#endif


#include "BigInteger.h"
#include "FFT.h"
#include "NTT.h"
#include "NTT2.h"


//------------------------------------------------------------------------------

static void TestFFT() {
    {
        std::vector<std::vector<std::complex<FFTFloat>>> inputs = {
            { 1, 2, 3, 4 },
            { 1, 2, 3, 4, 5, 6, 7, 8 },
        };
        std::vector<std::vector<std::complex<FFTFloat>>> expects = {
            { { 10, 0 },{ -2,-2 },{ -2,0 },{ -2,2 } },
#if USE_SINGLE_FLOAT_FFT
            { { 36,0 },{ -4,-9.65685f },{ -4,-4 },{ -4,-1.65685f },{ -4,0 },{ -4, 1.65685f },{ -4, 4 },{ -4, 9.65685f }, },
#else
            { { 36,0 },{ -4,-9.65685 },{ -4,-4 },{ -4,-1.65685 },{ -4,0 },{ -4, 1.65685 },{ -4, 4 },{ -4, 9.65685 }, },
#endif
        };

        for (size_t i = 0; i < inputs.size(); ++i) {
            std::vector<std::complex<FFTFloat>> output(inputs[i].size());

            FastFourierTransform(&output[0], &inputs[i][0], inputs[i].size());
            for (size_t j = 0; j < output.size(); ++j) {
                ASSERT(Equals(output[j], expects[i][j]));
            }
            InverseFastFourierTransform(&output[0], &expects[i][0], expects[i].size());
            for (size_t j = 0; j < output.size(); ++j) {
                ASSERT(Equals(output[j], inputs[i][j]));
            }
        }
    }
    {
        std::vector<std::vector<uint64_t>> inputs = {
            { 1, 2, 3, 4 },
            { 536870909, 536870910, 536870911, 536870912 },
        };

        for (auto &input : inputs) {
            std::vector<uint64_t> output(input.size());
            NumberTheoreticTransform(&output[0], &input[0], input.size());

            std::vector<uint64_t> outputInverse(input.size());
            InverseNumberTheoreticTransform(&outputInverse[0], &output[0], output.size());
            for (size_t j = 0; j < outputInverse.size(); ++j) {
                ASSERT(outputInverse[j] == input[j]);
            }
        }
    }
    {
        std::vector<std::vector<uint32_t>> inputs = {
            { 1, 2, 3, 4 },
            { 536870909, 536870910, 536870911, 536870912 },
            { 1, 2, 3, 4, 1, 2, 3, 4, },
        };

        for (auto &input : inputs) {
            std::vector<uint64_t> output(input.size());

            size_t rawNumberSize, ringNumberSize;
            EstimateNTT2NumberSize(input.size(), input.size(), rawNumberSize, ringNumberSize);

            size_t nttSize = NextPowerOf2((input.size() + rawNumberSize - 1) / rawNumberSize * 2);

            std::vector<uint32_t> inputNums(nttSize * ringNumberSize);
            for (size_t i = 0, io = 0; i < input.size(); i += rawNumberSize, io += ringNumberSize)
                Memcpy(&inputNums[io], &input[i], std::min(rawNumberSize, input.size() - i));
            std::vector<uint32_t> outputNums(inputNums.size());

            NumberTheoreticTransform2(&outputNums[0], &inputNums[0], nttSize, ringNumberSize);

            std::vector<uint32_t> inputNums2(inputNums.size());
            InverseNumberTheoreticTransform2(&inputNums2[0], &outputNums[0], nttSize, ringNumberSize);
            ASSERT(equal(inputNums.begin(), inputNums.end(), inputNums2.begin()));
        }
    }
}

static void TestConvolve() {
    {
        std::vector<std::vector<uint32_t>> inputs = {
            { 1, 2, 3, 4 },
            { 16777213, 16777214, 16777215, 16777216 }, // upper bound, 2^24
                                                        // { 33554429, 33554430, 33554431, 33554432 },  overflow, 2^25
        };

        for (auto &input : inputs) {
            std::vector<uint64_t> expectedOutput(input.size() * 2 - 1);
            Convolve_Classic(
                &expectedOutput[0], expectedOutput.size(),
                &input[0], input.size(), &input[0], input.size(),
                [](auto v) {return v; }, [](uint64_t v) {return v; });

            std::vector<uint64_t> output(input.size() * 2 - 1);
            Convolve_FFT(
                &output[0], output.size(),
                &input[0], input.size(), &input[0], input.size(),
                [](auto v) { return v; },
                [](std::complex<FFTFloat> &v)
            {
                auto re = llround(v.real());
                ASSERT(re >= 0 && llround(v.imag()) == 0);
                return re;
            });
            ASSERT(equal(output.begin(), output.end(), expectedOutput.begin()));
        }
    }
    {
        std::vector<std::vector<uint32_t>> inputs = {
            { 1, 2, 3, 4 },
            { 536870909, 536870910, 536870911, 536870912 },   // upper bound,  2^29
                                                              // { 1073741821, 1073741822, 1073741823, 1073741824 }, overflow, 2^30
        };

        for (auto &input : inputs) {
            std::vector<uint64_t> expectedOutput(input.size() * 2 - 1);
            Convolve_Classic(
                &expectedOutput[0], expectedOutput.size(),
                &input[0], input.size(), &input[0], input.size(),
                [](auto v) {return v; }, [](uint64_t v) {return v; });

            std::vector<uint64_t> output(input.size() * 2 - 1);
            Convolve_NTT(
                &output[0], output.size(),
                &input[0], input.size(), &input[0], input.size(),
                [](auto v) { return v; },
                [](auto v) { return v; });
            ASSERT(equal(output.begin(), output.end(), expectedOutput.begin()));
        }
    }
}

static void TestBigInteger() {
#if USE_GMP
    std::vector<std::string> randomStrs(16);
    std::generate(randomStrs.begin(), randomStrs.end(), []()
    {
        std::ostringstream so;
        for (int len = rand() % 30 + 10; len > 0; --len)
            so << rand();
        return so.str();
    });

    for (auto &a : randomStrs) {
        for (auto &b : randomStrs) {
            auto s1 = mpz_class(mpz_class(a) + mpz_class(b)).get_str();
            auto s2 = (BigInteger(a) + BigInteger(b)).ToString();
            ASSERT(s1 == s2);
        }
    }
    for (auto &a : randomStrs) {
        for (auto &b : randomStrs) {
            auto s1 = mpz_class(mpz_class(a) - mpz_class(b)).get_str();
            if (s1[0] == '-') continue;
            auto s2 = (BigInteger(a) - BigInteger(b)).ToString();
            ASSERT(s1 == s2);
        }
    }
    for (auto &a : randomStrs) {
        SetAlgorithmSwitchingThreashold(MultiplePrecisionOp::MA_NTT2, 8);
        for (auto &b : randomStrs) {
            auto expected = mpz_class(mpz_class(a) * mpz_class(b)).get_str();
            std::string strs[] = {
                (BigInteger(a) * BigInteger(b)).ToString(),
                BigInteger(MultiplePrecisionOp::Multiply_Karatsuba(BigInteger(a).Digits(), BigInteger(b).Digits())).ToString(),
                BigInteger(MultiplePrecisionOp::Multiply_FFT(BigInteger(a).Digits(), BigInteger(b).Digits())).ToString(),
                BigInteger(MultiplePrecisionOp::Multiply_NTT(BigInteger(a).Digits(), BigInteger(b).Digits())).ToString(),
                BigInteger(MultiplePrecisionOp::Multiply_NTT2(BigInteger(a).Digits(), BigInteger(b).Digits())).ToString(),
            };
            for (auto &s : strs)
                ASSERT(expected == s);
        }
        MultiplePrecisionOp::ResetConfigurations();
    }
    for (auto &a : randomStrs) {
        auto b = rand() & 0xf;
        mpz_class c;
        mpz_pow_ui(c.get_mpz_t(), mpz_class(a).get_mpz_t(), b);
        auto s1 = c.get_str();
        auto s2 = Power(BigInteger(a), b).ToString();
        ASSERT(s1 == s2);
    }
#ifndef _DEBUG
    for (auto i = 10; i < 16; ++i) {
        auto a = 37;
        auto b = 1 << i;
        mpz_class c;
        mpz_pow_ui(c.get_mpz_t(), mpz_class(a).get_mpz_t(), b);
        auto s1 = c.get_str();
        auto s2 = Power(BigInteger(a), b).ToString();
        ASSERT(s1 == s2);
    }
    for (auto i = 0; i < 1; ++i) {
        auto a = (1 << 14) + i;
        mpz_class c;
        mpz_fac_ui(c.get_mpz_t(), a);
        auto s1 = c.get_str();
        auto s2 = Factorial(a).ToString();
        ASSERT(s1 == s2);
    }
#endif
#endif

    ASSERT(Fibonacci(100).ToString() == "573147844013817084101");
    ASSERT(Fibonacci(1000).ToString() == "70330367711422815821835254877183549770181269836358732742604905087154537118196933579742249494562611733487750449241765991088186363265450223647106012053374121273867339111198139373125598767690091902245245323403501");
}

static void Test() {
    TestFFT();
    TestConvolve();
    TestBigInteger();
}

static void Benchmark_BigInteger() {
    puts("factorial:");
    for (auto n = 10000; n <= 160000; n <<= 1) {
        printf("\t%-10d: %.6f ms\n", n, Timing([=]() { Factorial(n); }, 1) * 1000);
    }
    puts("fibonacci:");
    for (auto n = 10000; n <= 320000; n <<= 1) {
        printf("\t%-10d: %.6f ms\n", n, Timing([=]() { Fibonacci(n); }, 1) * 1000);
    }
    puts("power (37^(2^n)):");
    for (auto n = 10; n <= 18; ++n) {
        printf("\t%-10d: %.6f ms\n", n, Timing([=]() { Power(BigInteger(37), 1UL << n); }, 1) * 1000);
    }
}

static void Benchmark() {
    Benchmark_BigInteger();
}

int main() {
#if USE_SCANFFT
#ifdef NDEBUG
    ScanFFT::Setup(23);
#else
    ScanFFT::Setup(10);
#endif
#endif


    Test();
#ifndef _DEBUG
    Benchmark();
#endif


#if USE_SCANFFT
    ScanFFT::Cleanup();
#endif
}
