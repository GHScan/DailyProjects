#include "stdafx.h"

#include <array>
#include <vector>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <chrono>

#define USE_GMP

#ifdef USE_GMP
#include <mpirxx.h>
#endif


#include "BigInteger.h"


template <typename TFunc>
static void Timing(char const* name, TFunc func, int times = 3) {
    using namespace std::chrono;

    if (times > 1) func();

    auto t = std::numeric_limits<double>::max();
    for (auto i = 0; i < times; ++i) {
        auto start = high_resolution_clock::now();
        func();
        auto end = high_resolution_clock::now();
        t = std::min(t, duration<double>(end - start).count());
    }

    std::cout << name << " : " << t << " s" << std::endl;
}


//------------------------------------------------------------------------------

static void TestFFT() {
    {
        std::vector<std::vector<std::complex<double>>> inputs = {
            { 1, 2, 3, 4 },
            { 1, 2, 3, 4, 5, 6, 7, 8 },
        };
        std::vector<std::vector<std::complex<double>>> expects = {
            { { 10, 0 },{ -2,-2 },{ -2,0 },{ -2,2 } },
            { { 36,0 },{ -4,-9.65685 },{ -4,-4 },{ -4,-1.65685 },{ -4,0 },{ -4, 1.65685 },{ -4, 4 },{ -4, 9.65685 }, },
        };

        for (size_t i = 0; i < inputs.size(); ++i) {
            std::vector<std::complex<double>> output(inputs[i].size());

            FFT(&output[0], &inputs[i][0], inputs[i].size());
            for (size_t j = 0; j < output.size(); ++j) {
                ASSERT(Equals(output[j], expects[i][j]));
            }
            InverseFFT(&output[0], &expects[i][0], expects[i].size());
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

        for (size_t i = 0; i < inputs.size(); ++i) {
            std::vector<uint64_t> output(inputs[i].size());
            NTT(&output[0], &inputs[i][0], inputs[i].size());

            std::vector<uint64_t> outputInverse(inputs[i].size());
            InverseNTT(&outputInverse[0], &output[0], output.size());
            for (size_t j = 0; j < outputInverse.size(); ++j) {
                ASSERT(outputInverse[j] == inputs[i][j]);
            }
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
            std::vector<uint64_t> expectedResult(input.size() * 2 - 1);
            ClassicConvolve(
                &input[0], input.size(), &input[0], input.size(),
                &expectedResult[0], expectedResult.size(),
                [](auto v) {return v; }, [](uint64_t v) {return v; });

            std::vector<uint64_t> result(input.size() * 2 - 1);
            Convolve(
                &input[0], input.size(), &input[0], input.size(),
                &result[0], result.size(),
                [](auto v) { return v; },
                [](auto v) { return llround(v.real()); });
                ASSERT(equal(result.begin(), result.end(), expectedResult.begin()));
        }
    }
    {
        std::vector<std::vector<uint32_t>> inputs = {
            { 1, 2, 3, 4 },
            { 536870909, 536870910, 536870911, 536870912 },   // upper bound,  2^29
            // { 1073741821, 1073741822, 1073741823, 1073741824 }, overflow, 2^30
        };

        for (auto &input : inputs) {
            std::vector<uint64_t> expectedResult(input.size() * 2 - 1);
            ClassicConvolve(
                &input[0], input.size(), &input[0], input.size(),
                &expectedResult[0], expectedResult.size(),
                [](auto v) {return v; }, [](uint64_t v) {return v; });

            std::vector<uint64_t> result(input.size() * 2 - 1);
            NTConvolve(
                &input[0], input.size(), &input[0], input.size(),
                &result[0], result.size(),
                [](auto v) { return v; },
                [](auto v) { return v; });
            ASSERT(equal(result.begin(), result.end(), expectedResult.begin()));
        }
    }
}

static void TestBigInteger() {
#ifdef USE_GMP
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
        for (auto &b : randomStrs) {
            auto s1 = mpz_class(mpz_class(a) * mpz_class(b)).get_str();
            auto s2 = (BigInteger(a) * BigInteger(b)).ToString();
            auto s3 = BigInteger(MultiplePrecisionOp::Multiply_Karatsuba(BigInteger(a).Digits(), BigInteger(b).Digits())).ToString();
            auto s4 = BigInteger(MultiplePrecisionOp::Multiply_FFT(BigInteger(a).Digits(), BigInteger(b).Digits())).ToString();
            auto s5 = BigInteger(MultiplePrecisionOp::Multiply_NTT(BigInteger(a).Digits(), BigInteger(b).Digits())).ToString();
            ASSERT(s1 == s2);
            ASSERT(s1 == s2);
            ASSERT(s1 == s3);
            ASSERT(s1 == s4);
            ASSERT(s1 == s5);
        }
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

static void Benchmark() {
    Timing("factorial", []()
    {
        Factorial(160000);
    });
    Timing("fibonacci", []()
    {
        Fibonacci(160000);
    });
    Timing("power", []()
    {
        Power(BigInteger(37), 1 << 18);
    });
}

int main() {
    Test();
#ifndef _DEBUG
    Benchmark();
#endif
} 