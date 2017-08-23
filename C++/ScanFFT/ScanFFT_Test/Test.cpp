
#include <numeric>
#include <cassert>

#include <ScanFFT.h>


#ifdef NDEBUG
#define FFT_LOG2_OF_MAX_SIZE 24
#else
#define FFT_LOG2_OF_MAX_SIZE 16
#endif

//----------------------------------------------------------------------

static void TestFFT() {
    using namespace ScanFFT;

#if SCANFFT_SINGLE_PRECISION_FLOAT
    for (uint8_t log2OfSize = 0; log2OfSize <= 11; ++log2OfSize) {
#else
    for (uint8_t log2OfSize = 0; log2OfSize <= 16; ++log2OfSize) {
#endif
        size_t size = 1ULL << log2OfSize;
        SCANFFT_ALLOC(in, size);
        SCANFFT_ALLOC(out, size);
        SCANFFT_ALLOC(out2, size);

        std::iota(inReals, inReals + size, Float(0));
        std::fill(inImags, inImags + size, Float(0));
        SCANFFT_TRANSFORM(out, in, log2OfSize);
        SCANFFT_INVERSE_TRANSFORM(out2, out, log2OfSize);

        for (size_t i = 0; i < size; ++i) {
#if SCANFFT_SINGLE_PRECISION_FLOAT
            Float epsilon = 1e-1f;
#else
            Float epsilon = 1e-4;
#endif
            if (!FEquals(inReals[i], out2Reals[i], epsilon) || !FEquals(inImags[i], out2Imags[i], epsilon))
                assert(0);
        }

        SCANFFT_FREE(out2);
        SCANFFT_FREE(out);
        SCANFFT_FREE(in);
    }
}

static void BenchmarkFFT() {
    using namespace ScanFFT;

    puts("ScanFFT\n");
    for (uint8_t log2OfSize = 2; log2OfSize <= FFT_LOG2_OF_MAX_SIZE; ++log2OfSize) {
        auto size = 1ULL << log2OfSize;
        SCANFFT_ALLOC(in, size);
        std::iota(inReals, inReals + size, Float(0));
        std::fill(inImags, inImags + size, Float(0));
        SCANFFT_ALLOC(out, size);

        auto loop = 1 << (FFT_LOG2_OF_MAX_SIZE - log2OfSize);
        printf("\t2^%d: %f us\n", log2OfSize, Timing([&]()
        {
            for (auto i = 0; i < loop; ++i)
                Transform(outReals, outImags, inReals, inImags, log2OfSize);
        }) * 1000000 / loop);

        SCANFFT_FREE(out);
        SCANFFT_FREE(in);
    }
}

//----------------------------------------------------------------------

int main()
{
    ScanFFT::Setup(FFT_LOG2_OF_MAX_SIZE);

    TestFFT();
#ifdef NDEBUG
    BenchmarkFFT();
#endif

    ScanFFT::Cleanup();
}