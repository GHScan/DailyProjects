// Cpp2013.cpp : 定义控制台应用程序的入口点。
//
#include "stdafx.h"

#include <functional>
#include <ppl.h>
#include <omp.h>
#include <emmintrin.h>
#include <immintrin.h>
#include <windows.h>
using namespace concurrency;

static inline int getProcessorCount()
{
	SYSTEM_INFO sysinfo;
	GetSystemInfo(&sysinfo);
	return sysinfo.dwNumberOfProcessors;
}

static inline double getTime()
{
	long long time, freq;
	::QueryPerformanceFrequency((LARGE_INTEGER*)&freq);
	::QueryPerformanceCounter((LARGE_INTEGER*)&time);
	return double(time) / freq;
}

static void timeIt(const char *name, int times, function<void()> f)
{
	if (times > 1) f();

	auto start = getTime();
	for (auto i = 0; i < times; ++i) f();
	printf("%-24s => %.12g\n", name, (getTime() - start) / times);
}

static const int kSampleCount = 1024 * 1024 * 60;
static const int kThreadCount = getProcessorCount();
static const int kSSEPackSize = sizeof(__m128) / sizeof(float);
static const int kAVXPackSize = sizeof(__m256) / sizeof(float);

class LCG
{
public:
	LCG(uint32_t seed = 0) : mSeed(seed) {}
	float operator()()
	{
		mSeed = mSeed * 214013 + 2531011;
		union
		{
			uint32_t u;
			float f;
		}u = { (mSeed >> 9) | 0x3F800000 };
		return u.f - 1.0f;
	}
private:
	uint32_t mSeed;
};

static void milo_naive()
{
	unsigned totalCount = 0;

	LCG random(0);
	for (auto i = 1; i < kSampleCount; i++){
		float a = random();
		float b = random();

		if (a * a + b * b < 1.0f)
			totalCount++;
	}

#ifdef _DEBUG
	printf("%.16g\n", totalCount * 4.0 / kSampleCount);
#else
	volatile int result = totalCount;
#endif
}

static const __m128i cLCG1 = _mm_set1_epi32(214013);
static const __m128i cLCG2 = _mm_set1_epi32(2531011);
static const __m128i cLCGmask = _mm_set1_epi32(0x3F800000);
static const __m128i cOnei = _mm_set1_epi32(1);
static const __m128 cOne = _mm_set1_ps(1.0f);

static inline __m128i mul_uint32(__m128i a, __m128i b) {
	const __m128i tmp1 = _mm_mul_epu32(a, b);
	const __m128i tmp2 = _mm_mul_epu32(_mm_srli_si128(a, 4), _mm_srli_si128(b, 4));
	return _mm_unpacklo_epi32(_mm_shuffle_epi32(tmp1, _MM_SHUFFLE(0, 0, 2, 0)), _mm_shuffle_epi32(tmp2, _MM_SHUFFLE(0, 0, 2, 0)));
}

class LCGSSE
{
public:
	LCGSSE(__m128i seed) : mSeed(seed) {}
	__m128 operator()()
	{
		mSeed = _mm_add_epi32(mul_uint32(mSeed, cLCG1), cLCG2);
		const __m128i u = _mm_or_si128(_mm_srli_epi32(mSeed, 9), cLCGmask);
		return _mm_sub_ps(_mm_castsi128_ps(u), cOne);
	}
private:
	__m128i mSeed;
};

static void milo_openmpSSE()
{
	auto totalCount = 0;

#pragma omp parallel num_threads(kThreadCount) reduction(+ : totalCount)
	{
		auto threadCount = omp_get_num_threads();

		__m128i counters = _mm_setzero_si128();

		int j = omp_get_thread_num() * kSSEPackSize * 2;
		LCGSSE random1(_mm_setr_epi32(j + 0, j + 1, j + 2, j + 3));
		LCGSSE random2(_mm_setr_epi32(j + 4, j + 5, j + 6, j + 7));

		for (auto i = 0; i < kSampleCount / kSSEPackSize / threadCount; i++) {
			const __m128 a = random1();
			const __m128 b = random2();

			const __m128 r1 = _mm_cmplt_ps(_mm_add_ps(_mm_mul_ps(a, a), _mm_mul_ps(b, b)), cOne);
			counters = _mm_add_epi32(counters, _mm_and_si128(_mm_castps_si128(r1), cOnei));
		}

		for (auto i = 0; i < kSSEPackSize; i++)
			totalCount += counters.m128i_u32[i];
	}

#ifdef _DEBUG
	printf("%.16g\n", totalCount * 4.0 / kSampleCount);
#endif
}

static void my_naive()
{
	LCG random(0);

	auto totalCount = 0;
	for (auto i = 0; i < kSampleCount; ++i)
	{
		auto x = random();
		auto y = random();
		if (x * x + y * y < 1) ++totalCount;
	}

#ifdef _DEBUG
	printf("%.16g\n", totalCount * 4.0 / kSampleCount);
#else
	volatile int result = totalCount;
#endif
}

struct PPLThreadLocal
{
	LCG random;
	int count;
	PPLThreadLocal(int seed) : random(seed), count(0) {}
};

static void my_ppl()
{
	combinable<PPLThreadLocal> combinableLocal([&]() { return PPLThreadLocal(GetCurrentThreadId()); });

	static const auto kChunkSize = 1024;
	parallel_for(0, kSampleCount / kChunkSize, [&combinableLocal](int _)
	{
		auto &local = combinableLocal.local();
		for (auto i = 0; i < kChunkSize; ++i)
		{
			auto x = local.random();
			auto y = local.random();
			if (x * x + y * y < 1) ++local.count;
		}
	});

#ifdef _DEBUG
	printf("%.16g\n",
		combinableLocal.combine([](const PPLThreadLocal &a, const PPLThreadLocal &b)
	{
		PPLThreadLocal r(0);
		r.count = a.count + b.count;
		return r;
	}).count * 4.0 / kSampleCount);
#endif
}

static void my_openmp()
{
	auto totalCount = 0;
#pragma omp parallel reduction(+:totalCount)
	{
		LCG random(omp_get_thread_num());
#pragma omp for
		for (auto i = 0; i < kSampleCount; ++i)
		{
			auto x = random();
			auto y = random();
			if (x * x + y * y < 1) ++totalCount;
		}
	}

#ifdef _DEBUG
	printf("%.16g\n", totalCount * 4.0 / kSampleCount);
#endif
}

class LCGAVX
{
public:
	LCGAVX(int seed) :
		mSeed(_mm256_set_epi32(seed, seed + 1, seed + 2, seed + 3, seed + 4, seed + 5, seed + 6, seed + 7)) {}
	__m256 operator() ()
	{
		mSeed = _mm256_add_epi32(_mm256_mullo_epi32(mSeed, _mm256_set1_epi32(214013)), _mm256_set1_epi32(2531011));
		auto temp = _mm256_or_si256(_mm256_srli_epi32(mSeed, 9), _mm256_set1_epi32(0x3F800000));
		return _mm256_sub_ps(_mm256_castsi256_ps(temp), _mm256_set1_ps(1));
	}
private:
	__m256i mSeed;
};

static void my_openmpAVX()
{
	auto totalCount = 0;
#pragma omp parallel num_threads(kThreadCount) reduction(+:totalCount)
	{
		auto threadCount = omp_get_num_threads();

		__m256i counters = _mm256_setzero_si256();
		LCGAVX random1(omp_get_thread_num() * kAVXPackSize * 2);
		LCGAVX random2(omp_get_thread_num() * kAVXPackSize * 2 + kAVXPackSize);

		for (auto i = 0; i < kSampleCount / kAVXPackSize / threadCount; ++i)
		{
			auto x = random1();
			auto y = random2();
			auto xxyy = _mm256_add_ps(_mm256_mul_ps(x, x), _mm256_mul_ps(y, y));
			auto lt = _mm256_cmp_ps(xxyy, _mm256_set1_ps(1), _CMP_LT_OQ);
			auto result = _mm256_and_si256(_mm256_castps_si256(lt), _mm256_set1_epi32(1));
			counters = _mm256_add_epi32(counters, result);
		}

		for (auto i = 0; i < kAVXPackSize; ++i)
			totalCount += counters.m256i_i32[i];
	}

#ifdef _DEBUG
	printf("%.16g\n", totalCount * 4.0 / kSampleCount);
#endif
}

#ifdef _DEBUG
#define TIMEIT(func) timeIt(#func, 1, func);
#else
#define TIMEIT(func) timeIt(#func, 5, func);
#endif

int main()
{
	TIMEIT(milo_naive);
	TIMEIT(milo_openmpSSE);
	TIMEIT(my_naive);
	TIMEIT(my_ppl);
	TIMEIT(my_openmp);
	TIMEIT(my_openmpAVX);
}