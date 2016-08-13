#include "stdafx.h"


#include <cstdint>
#include <vector>
#include <utility>
#include <random>
#include <iostream>
#include <algorithm>
#include <fstream>
#include <unordered_set>
#include <chrono>
using namespace std;


constexpr uint32_t kSeed_PrimalityTester = 5489U;
constexpr uint32_t kSeed_PerfectHashBuilder = kSeed_PrimalityTester + 1;
constexpr uint32_t kSeed_Test = kSeed_PrimalityTester + 2;
constexpr uint32_t kSeed_Benchmark = kSeed_PrimalityTester + 3;
constexpr uint32_t kMultiplierSearchDepth = 32;
using TKey = int;


template <typename TFunc>
static double TimeIt(int times, TFunc func) {
    if (times > 1)
        func();

    auto start = chrono::high_resolution_clock::now();
    for (auto i = 0; i < times; ++i)
        func();
    auto end = chrono::high_resolution_clock::now();

    return chrono::duration<double>(end - start).count() / times;
}


template<typename T>
class Random
{
public:

    Random(uint32_t seed) :
        mEngine(seed)
    {
    }

    T operator () ()
    {
        return mDistribution(mEngine);
    }

private:
    default_random_engine mEngine;
    uniform_int_distribution<T> mDistribution;
};


namespace HashFunctions
{
    inline uint64_t Djb2(uint8_t const *bytes, size_t length)
    {
        uint64_t hashCode = 5381;
        for (size_t i = 0; i < length; ++i)
            hashCode = ((hashCode << 5) + hashCode) + bytes[i];
        return hashCode;
    }

    inline uint64_t Fnv(uint8_t const *bytes, size_t length)
    {
        auto fnvBasis = 14695981039346656037ULL;
        auto fnvPrime = 1099511628211ULL;
        auto hashCode = fnvBasis;
        for (size_t i = 0; i < length; ++i)
            hashCode = (hashCode ^ bytes[i]) * fnvPrime;
        return hashCode;
    }
}


class MillerRabinPrimalityTester final
{
public:
    MillerRabinPrimalityTester(uint32_t seed, int iteration = 5) :
        mRandom(seed), mIteration(iteration)
    {
    }

    bool operator() (uint64_t v)
    {
        if (v < 2) return false;
        if (v != 2 && (v & 1) == 0) return false;

        auto s = v - 1;
        for (; (s & 1) == 0; s >>= 1);

        for (auto i = 0; i < mIteration; ++i)
        {
            auto a = mRandom() % (v - 1) + 1, temp = s;
            auto mod = ModularExponentiation(a, temp, v);
            while (temp != v - 1 && mod != 1 && mod != v - 1)
            {
                mod = MultiplyModulo(mod, mod, v);
                temp <<= 1;
            }
            if (mod != v - 1 && (temp & 1) == 0)
                return false;
        }
        return true;
    }

private:
    static uint64_t MultiplyModulo(uint64_t a, uint64_t b, uint64_t m)
    {
        uint64_t r = 0;
        for (a %= m; b > 0; b >>= 1)
        {
            if ((b & 1) == 1)
                r = (r + a) % m;
            a = (a << 1) % m;
        }
        return r;
    }

    static uint64_t ModularExponentiation(uint64_t b, uint64_t e, uint64_t m)
    {
        uint64_t r = 1;
        for (b %= m; e > 0; e >>= 1)
        {
            if ((e & 1) == 1)
                r = r * b % m;
            b = b * b % m;
        }
        return r;
    }

private:
    Random<uint64_t> mRandom;
    int mIteration;
};


namespace PerfectHashUtils
{
    static MillerRabinPrimalityTester gPrimalityTester(kSeed_PrimalityTester);

    inline size_t NextPrime(size_t v)
    {
        for (; !gPrimalityTester(v); ++v);
        return v;
    }

    inline uint64_t ComputeHashCode1(TKey const &key)
    {
        return HashFunctions::Djb2(reinterpret_cast<uint8_t const *>(&key), sizeof(key));
    }

    inline uint64_t ComputeHashCode2(TKey const &key)
    {
        return HashFunctions::Fnv(reinterpret_cast<uint8_t const *>(&key), sizeof(key));
    }

    inline size_t ComputeBucketId(uint64_t hashCode1, size_t bucketCount)
    {
        return hashCode1 % bucketCount;
    }

    inline size_t ComputeSlotId(uint64_t hashCode1, uint64_t hashCode2, uint32_t alpha, uint32_t beta, size_t slotCount)
    {
        return (hashCode1 + hashCode2 * alpha + beta) % slotCount;
    }
}


class PerfectHashFunction final
{
public:
    PerfectHashFunction(
        size_t slotCount,
        vector<uint8_t> buckets,
        vector<pair<uint32_t, uint32_t>> multipliers) :
        mSlotCount(slotCount), mBucketId2MultiplierIdMap(move(buckets)), mMultipliers(move(multipliers))
    {
    }

    PerfectHashFunction()
        : mSlotCount(0)
    {
    }

    size_t operator () (TKey const &key) const
    {
        if (mSlotCount == 0) throw runtime_error("hasn't been initialized");

        auto hashCode1 = PerfectHashUtils::ComputeHashCode1(key);
        auto hashCode2 = PerfectHashUtils::ComputeHashCode2(key);
        auto multiplierId = mBucketId2MultiplierIdMap[PerfectHashUtils::ComputeBucketId(hashCode1, GetBucketCount())];
        auto &multiplier = mMultipliers[multiplierId];
        return PerfectHashUtils::ComputeSlotId(hashCode1, hashCode2, multiplier.first, multiplier.second, GetSlotCount());
    }

    size_t GetSlotCount() const { return mSlotCount; }
    size_t GetBucketCount() const { return mBucketId2MultiplierIdMap.size(); }
    size_t GetMultiplierCount() const { return mMultipliers.size(); }

    void Save(char const *fileName) const
    {
        ofstream fo(fileName, ios::binary);

        fo.write(reinterpret_cast<char const*>(&mSlotCount), sizeof(mSlotCount));

        size_t size;

        size = mBucketId2MultiplierIdMap.size();
        fo.write(reinterpret_cast<char const*>(&size), sizeof(size));
        fo.write(
            reinterpret_cast<char const*>(&mBucketId2MultiplierIdMap[0]),
            mBucketId2MultiplierIdMap.size() * sizeof(mBucketId2MultiplierIdMap[0]));

        size = mMultipliers.size();
        fo.write(reinterpret_cast<char const*>(&size), sizeof(size));
        fo.write(
            reinterpret_cast<char const*>(&mMultipliers[0]),
            mMultipliers.size() * sizeof(mMultipliers[0]));
    }

    void Load(char const *fileName)
    {
        ifstream fi(fileName, ios::binary);

        fi.read(reinterpret_cast<char *>(&mSlotCount), sizeof(mSlotCount));

        size_t size;
        streamsize byteCount;

        fi.read(reinterpret_cast<char *>(&size), sizeof(size));
        byteCount = size * sizeof(mBucketId2MultiplierIdMap[0]);
        mBucketId2MultiplierIdMap.resize(size);
        fi.read(reinterpret_cast<char *>(&mBucketId2MultiplierIdMap[0]), byteCount);
        if (fi.gcount() < byteCount)
            throw runtime_error("found EOF during read bucket id to multiplier id map");

        fi.read(reinterpret_cast<char *>(&size), sizeof(size));
        byteCount = size * sizeof(mMultipliers[0]);
        mMultipliers.resize(size);
        fi.read(reinterpret_cast<char *>(&mMultipliers[0]), byteCount);
        if (fi.gcount() < byteCount)
            throw runtime_error("found EOF during read multipliers");
    }

private:
    size_t mSlotCount;
    vector<uint8_t> mBucketId2MultiplierIdMap;
    vector<pair<uint32_t, uint32_t>> mMultipliers;
};

class PerfectHashFunctionBuilder final
{
public:
    PerfectHashFunctionBuilder(
        double loadFactor, uint8_t associativity, uint32_t seed) :
        mLoadFactor(loadFactor), mAssociativity(associativity), mRandom(seed)
    {
    }

    double GetLoadFactor() const { return mLoadFactor; }
    uint8_t GetAssociativity() const { return mAssociativity; }

    void AddKey(TKey const &key)
    {
        mKeys.push_back(key);
    }

    void ClearKeys()
    {
        mKeys.clear();
    }

    size_t GetKeyCount() const { return mKeys.size(); }

    PerfectHashFunction CreateFunction()
    {
        using namespace PerfectHashUtils;

        size_t slotCount = NextPrime(static_cast<size_t>(ceil(mKeys.size() / mLoadFactor)));
        size_t bucketCount = NextPrime(static_cast<size_t>(ceil(double(slotCount) / mAssociativity)));
        if (slotCount == 0 || bucketCount == 0)
            throw invalid_argument("slot count/bucket count should not be 0");

        GroupKeysIntoOrderedBuckets(bucketCount);

        vector<uint8_t> bucketId2MultiplierIdMap(bucketCount);
        vector<pair<uint32_t, uint32_t>> multipliers;
        GenerateMultipliers(slotCount, bucketId2MultiplierIdMap, multipliers);

        return PerfectHashFunction(slotCount, move(bucketId2MultiplierIdMap), move(multipliers));
    }

private:
    void GroupKeysIntoOrderedBuckets(size_t bucketCount)
    {
        using namespace PerfectHashUtils;

        vector<uint8_t> bucketId2SlotCountMap(bucketCount);
        for (auto &key : mKeys)
        {
            auto hashCode1 = ComputeHashCode1(key);
            auto bucketId = ComputeBucketId(hashCode1, bucketCount);
            if (++bucketId2SlotCountMap[bucketId] == 0)
                throw runtime_error("couldn't assign more than 255 slots for one bucket");
        }

        sort(mKeys.begin(), mKeys.end(), [&bucketId2SlotCountMap](auto &a, auto &b)
        {
            auto bucketIdA = ComputeBucketId(ComputeHashCode1(a), bucketId2SlotCountMap.size());
            auto bucketIdB = ComputeBucketId(ComputeHashCode1(b), bucketId2SlotCountMap.size());
            if (bucketId2SlotCountMap[bucketIdA] != bucketId2SlotCountMap[bucketIdB])
                return bucketId2SlotCountMap[bucketIdA] > bucketId2SlotCountMap[bucketIdB];
            if (bucketIdA != bucketIdB)
                return bucketIdA > bucketIdB;
            return a > b;
        });
    }

    void GenerateMultipliers(size_t slotCount, vector<uint8_t> &bucketId2MultiplierIdMap, vector<pair<uint32_t, uint32_t>> &multipliers)
    {
        using namespace PerfectHashUtils;

        vector<bool> slotOccupies(slotCount);
        vector<size_t> tempSlotIds;

        for (auto it = mKeys.begin(); it != mKeys.end();)
        {
            auto bucketId = ComputeBucketId(ComputeHashCode1(*it), bucketId2MultiplierIdMap.size());
            auto end = it + 1;
            while (end != mKeys.end() && bucketId == ComputeBucketId(ComputeHashCode1(*end), bucketId2MultiplierIdMap.size()))
                ++end;

            auto multiplierId = AllocateMultiplierForBucket(
                multipliers, slotOccupies, slotCount,
                it, end,
                mRandom,
                tempSlotIds);

            bucketId2MultiplierIdMap[bucketId] = multiplierId;

            it = end;
        }
    }

    static uint8_t AllocateMultiplierForBucket(
        vector<pair<uint32_t, uint32_t>> &multipliers,
        vector<bool> &slotOccupies,
        size_t slotCount,
        vector<TKey>::const_iterator keyBegin, vector<TKey>::const_iterator keyEnd,
        Random<uint32_t> &random,
        vector<size_t> &tempSlotIds)
    {
        for (size_t i = 0, count = multipliers.size() + kMultiplierSearchDepth; i < count; ++i)
        {
            uint32_t alpha, beta;
            if (i < multipliers.size())
            {
                alpha = multipliers[i].first;
                beta = multipliers[i].second;
            }
            else
            {
                alpha = random();
                beta = random();
                if (find(multipliers.begin(), multipliers.end(), make_pair(alpha, beta)) != multipliers.end())
                    continue;
            }

            if (ValidateMultiplier(alpha, beta, slotOccupies, slotCount, keyBegin, keyEnd, tempSlotIds))
            {
                tempSlotIds.clear();

                if (i >= multipliers.size())
                {
                    i = multipliers.size();
                    multipliers.push_back(make_pair(alpha, beta));
                }

                auto multiplierId = static_cast<uint8_t>(i);
                if (i > multiplierId)
                    throw overflow_error("multiplier id overflow detected");

                return multiplierId;
            }
        }

        throw runtime_error("couldn't allocate multiplier for bucket");
    }

    static bool ValidateMultiplier(
        uint32_t alpha, uint32_t beta,
        vector<bool> &slotOccupies,
        size_t slotCount,
        vector<TKey>::const_iterator keyBegin, vector<TKey>::const_iterator keyEnd,
        vector<size_t> &tempSlotIds)
    {
        using namespace PerfectHashUtils;

        for (auto it = keyBegin; it != keyEnd; ++it)
        {
            auto hashCode1 = ComputeHashCode1(*it);
            auto hashCode2 = ComputeHashCode2(*it);
            auto slotId = ComputeSlotId(hashCode1, hashCode2, alpha, beta, slotCount);

            if (slotOccupies[slotId])
            {
                for (auto id : tempSlotIds)
                    slotOccupies[id] = false;

                tempSlotIds.clear();
                return false;
            }

            slotOccupies[slotId] = true;
            tempSlotIds.push_back(slotId);
        }
        return true;
    }

private:
    double const mLoadFactor;
    uint8_t const mAssociativity;
    vector<TKey> mKeys;
    Random<uint32_t> mRandom;
};


static void GenerateUniqueRandoms(
    vector<TKey> &keys, Random<TKey> &random)
{
    generate(keys.begin(), keys.end(), random);
    sort(keys.begin(), keys.end());
    keys.erase(unique(keys.begin(), keys.end()), keys.end());
}

static void Test()
{
    Random<TKey> random(kSeed_Test);

    PerfectHashFunctionBuilder builder(0.5, 8, kSeed_PerfectHashBuilder);

    for (auto len = 1; len < 300; ++len)
    {
        vector<TKey> keys(len);
        GenerateUniqueRandoms(keys, random);

        for (auto& key : keys)
            builder.AddKey(key);
        auto func = builder.CreateFunction();
        builder.ClearKeys();

        unordered_set<size_t> set;
        for (auto& key : keys)
        {
            auto slotId = func(key);
            if (slotId >= func.GetSlotCount())
                throw out_of_range("invalid slot id");
            if (!set.insert(slotId).second)
                throw runtime_error("duplicated hash code detected");
        }
    }
}

static void Benchmark(size_t lengthScale)
{
    Random<TKey> random(kSeed_Benchmark);

    PerfectHashFunctionBuilder builder(0.5, 8, kSeed_PerfectHashBuilder);

    for (auto rawLength : { 1, 4, 8, 16 })
    {
        auto length = rawLength * lengthScale;

        vector<TKey> keys(length);
        GenerateUniqueRandoms(keys, random);

        for (auto& key : keys)
            builder.AddKey(key);

        auto time = TimeIt(1, [&builder]()
        {
            auto func = builder.CreateFunction();
        });
        cout << "Len=" << length << "," << time << "s" << endl;

        builder.ClearKeys();
    }
}

int main()
{
    Test();

#ifdef NDEBUG
    Benchmark(1000 * 1000);
#else
    Benchmark(1000);
#endif

}