#include "stdafx.h"

#include <vector>
#include <algorithm>
#include <sstream>


#define USE_GMP 1
#define USE_FFTW 0

#if USE_GMP
#include <mpirxx.h>
#endif
#if USE_FFTW
#include <fftw3.h>
#endif


#include "BigInteger.h"
#include "FFT.h"
#include "NTT.h"
#include "NTT2.h"
#include "FastestFT.h"


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
#if ENABLE_FASTEST_FT
    {
#if USE_SINGLE_FLOAT_FFT
        for (auto bits = 0; bits <= 12; ++bits) {
#else
        for (auto bits = 0; bits <= 20; ++bits) {
#endif
            auto len = 1 << bits;
            auto input = AlignedAlloc<std::complex<FFTFloat>>(len);
            iota(input, input + len, FFTFloat(0));

            auto output = AlignedAlloc<std::complex<FFTFloat>>(len);
            FastestFourierTransform(output, input, len);

            if (bits <= 10) {
                auto output2 = AlignedAlloc<std::complex<FFTFloat>>(len);
                FastFourierTransform(output2, input, len);
                for (size_t i = 0; i < len; ++i)
#if USE_SINGLE_FLOAT_FFT
                    ASSERT(Equals(output[i], output2[i], 1e1));
#else
                    ASSERT(Equals(output[i], output2[i]));
#endif
                AlignedFree(output2);
            }

            auto input2 = AlignedAlloc<std::complex<FFTFloat>>(len);
            InverseFastestFourierTransform(input2, output, len);
            for (size_t i = 0; i < len; ++i) 
#if USE_SINGLE_FLOAT_FFT
                ASSERT(Equals(input[i], input2[i], 1e0));
#else
                ASSERT(Equals(input[i], input2[i], 1e-3));
#endif

            AlignedFree(input2);
            AlignedFree(output);
            AlignedFree(input);
        }
    }
#endif
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
                [](auto v) { return llround(v.real()); });
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


static void Benchmark_FFT() {
    size_t kMaxBits = 20;
    size_t kMaxLoopBits = 22;

    puts("fft\n");
    for (size_t bits = 2; bits <= kMaxBits; ++bits) {
        std::vector<std::complex<FFTFloat>> input(1ULL << bits);
        iota(input.begin(), input.end(), FFTFloat(0));
        auto output(input);

        auto loop = 1 << (bits > kMaxLoopBits ? 0 : kMaxLoopBits - bits);
        printf("\t2^%-llu: %f us\n", bits, Timing([&]()
        {
            for (auto i = 0; i < loop; ++i)
                FastFourierTransform(&output[0], &input[0], input.size());
        }) * 1000000 / loop);
    }

#if ENABLE_FASTEST_FT
    puts("fastestft\n");
    for (size_t bits = 2; bits <= kMaxBits; ++bits) {
        auto len = 1ULL << bits;
        auto input = AlignedAlloc<std::complex<FFTFloat>>(len);
        iota(input, input + len, FFTFloat(0));
        auto output = AlignedAlloc<std::complex<FFTFloat>>(len);

        auto loop = 1 << (bits > kMaxLoopBits ? 0 : kMaxLoopBits - bits);
        printf("\t2^%-llu: %f us\n", bits, Timing([&]()
        {
            for (auto i = 0; i < loop; ++i)
                FastestFourierTransform(output, input, len);
        }) * 1000000 / loop);

        AlignedFree(output);
        AlignedFree(input);
    }
#endif

#if USE_FFTW
#if USE_SINGLE_FLOAT_FFT
    puts("fftwf3 estimate\n");
    for (size_t bits = 2; bits <= kMaxBits; ++bits) {
        auto len = 1 << bits;
        auto in = static_cast<fftwf_complex*>(fftw_malloc(sizeof(fftwf_complex) * len));
        auto out = static_cast<fftwf_complex*>(fftw_malloc(sizeof(fftwf_complex) * len));
        // auto p = fftwf_plan_dft_1d(len, in, out, FFTW_FORWARD, FFTW_ESTIMATE);
        auto p = fftwf_plan_dft_1d(len, in, out, FFTW_FORWARD, FFTW_MEASURE);
        for (auto i = 0; i < len; ++i)
            in[i][0] = FFTFloat(i), in[i][1] = FFTFloat(0);

        auto loop = 1 << (bits > kMaxLoopBits ? 0 : kMaxLoopBits - bits);
        printf("\t2^%-llu: %f us\n", bits, Timing([&]()
        {
            for (auto i = 0; i < loop; ++i)
                fftwf_execute(p);
        }) * 1000000 / loop);

        fftwf_destroy_plan(p);
        fftwf_free(in); fftwf_free(out);
    }
#else
    puts("fftw3 estimate\n");
    for (size_t bits = 2; bits <= kMaxBits; ++bits) {
        auto len = 1 << bits;
        auto in = static_cast<fftw_complex*>(fftw_malloc(sizeof(fftw_complex) * len));
        auto out = static_cast<fftw_complex*>(fftw_malloc(sizeof(fftw_complex) * len));
        auto p = fftw_plan_dft_1d(len, in, out, FFTW_FORWARD, FFTW_ESTIMATE);
        // auto p = fftw_plan_dft_1d(len, in, out, FFTW_FORWARD, FFTW_MEASURE);
        for (auto i = 0; i < len; ++i)
            in[i][0] = i, in[i][1] = 0;

        auto loop = 1 << (bits > kMaxLoopBits ? 0 : kMaxLoopBits - bits);
        printf("\t2^%-llu: %f us\n", bits, Timing([&]()
        {
            for (auto i = 0; i < loop; ++i)
                fftw_execute(p);
        }) * 1000000 / loop);

        fftw_destroy_plan(p);
        fftw_free(in); fftw_free(out);
    }
#endif
#endif
}

static void Benchmark_BigInteger() {
    printf("%-20s: %f s\n", "factorial", Timing([]() { Factorial(320000); }, 1));
    printf("%-20s: %f s\n", "fibonacci", Timing([]() { Fibonacci(320000); }, 1));
    printf("%-20s: %f s\n", "power", Timing([]() { Power(BigInteger(37), 1 << 17); }, 1));
}

static void Benchmark() {
    // Benchmark_FFT();
    Benchmark_BigInteger();
}

int main() {
    Test();
#ifndef _DEBUG
    Benchmark();
#endif
} 
