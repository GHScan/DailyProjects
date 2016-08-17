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
constexpr uint32_t kRandomStringLength = 32;
constexpr uint32_t kMultiplierSearchDepth = 64;


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
template<>
class Random<string>
{
public:

    Random(uint32_t seed) :
        mEngine(seed)
    {
    }

    string operator () ()
    {
        string s;
        for (uint32_t i = 0, length = mDistribution(mEngine) % kRandomStringLength;
            i < length;
            ++i)
        {
            s.push_back('a' + mDistribution(mEngine) % 26);
        }
        return s;
    }

private:
    default_random_engine mEngine;
    uniform_int_distribution<uint32_t> mDistribution;
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

    template<typename T>
    inline uint64_t Djb2(T v, typename enable_if<is_pod<T>::value>::type* = nullptr)
    {
        return Djb2(reinterpret_cast<uint8_t const*>(&v), sizeof(v));
    }

    inline uint64_t Djb2(string const &v)
    {
        return Djb2(reinterpret_cast<uint8_t const*>(v.c_str()), v.size());
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

    template<typename T>
    inline uint64_t Fnv(T v, typename enable_if<is_pod<T>::value>::type* = nullptr)
    {
        return Fnv(reinterpret_cast<uint8_t const*>(&v), sizeof(v));
    }

    inline uint64_t Fnv(string const &v)
    {
        return Fnv(reinterpret_cast<uint8_t const*>(v.c_str()), v.size());
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

    template<typename TKey>
    inline uint64_t ComputeHashCode1(TKey const &key)
    {
        return HashFunctions::Djb2(key);
    }

    template<typename TKey>
    inline uint64_t ComputeHashCode2(TKey const &key)
    {
        return HashFunctions::Fnv(key);
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


template<typename TKey>
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
        using namespace PerfectHashUtils;

        if (mSlotCount == 0) throw runtime_error("hasn't been initialized");

        auto hashCode1 = ComputeHashCode1(key);
        auto hashCode2 = ComputeHashCode2(key);
        auto multiplierId = mBucketId2MultiplierIdMap[ComputeBucketId(hashCode1, GetBucketCount())];
        auto &multiplier = mMultipliers[multiplierId];
        return ComputeSlotId(hashCode1, hashCode2, multiplier.first, multiplier.second, GetSlotCount());
    }

    size_t GetSlotCount() const { return mSlotCount; }
    size_t GetBucketCount() const { return mBucketId2MultiplierIdMap.size(); }
    size_t GetMultiplierCount() const { return mMultipliers.size(); }

    void Save(char const *filePath) const
    {
        ofstream fo(filePath, ios::binary);

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

    void Load(char const *filePath)
    {
        ifstream fi(filePath, ios::binary);

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

class PerfectHashCodeGenerator final
{
public:
    PerfectHashCodeGenerator(
        size_t slotCount,
        vector<uint8_t> buckets,
        vector<pair<uint32_t, uint32_t>> multipliers) :
        mSlotCount(slotCount), mBucketId2MultiplierIdMap(move(buckets)), mMultipliers(move(multipliers))
    {
    }

    void Generate(char const *sourceFilePath, char const *functionName, char const *keyType) const
    {
        ofstream fo(sourceFilePath);

        GenerateIncludes(fo);
        GenerateHashFunctions(fo);
        GenerateFunction(fo, functionName, keyType);
        GenerateSlotCountVariable(fo, functionName);
    }

private:
    static void GenerateIncludes(ofstream &fo)
    {
        fo <<
            "#include <cstdint>\n"
            "#include <string>\n"
            "using namespace std;\n"
            "\n";
    }

    static void GenerateHashFunctions(ofstream &fo)
    {
        fo <<
            "static inline uint64_t Djb2(uint8_t const *bytes, size_t length)\n"
            "{\n"
            "    uint64_t hashCode = 5381;\n"
            "    for (size_t i = 0; i < length; ++i)\n"
            "        hashCode = ((hashCode << 5) + hashCode) + bytes[i];\n"
            "    return hashCode;\n"
            "}\n"
            "\n"
            "template<typename TKey>\n"
            "static inline uint64_t Djb2(TKey v, typename enable_if<is_pod<TKey>::value>::type* = nullptr)\n"
            "{\n"
            "    return Djb2(reinterpret_cast<uint8_t const*>(&v), sizeof(v));\n"
            "}\n"
            "\n"
            "static inline uint64_t Djb2(string const &v)\n"
            "{\n"
            "    return Djb2(reinterpret_cast<uint8_t const*>(v.c_str()), v.size());\n"
            "}\n"
            "\n"
            "static inline uint64_t Fnv(uint8_t const *bytes, size_t length)\n"
            "{\n"
            "    auto fnvBasis = 14695981039346656037ULL;\n"
            "    auto fnvPrime = 1099511628211ULL;\n"
            "    auto hashCode = fnvBasis;\n"
            "    for (size_t i = 0; i < length; ++i)\n"
            "        hashCode = (hashCode ^ bytes[i]) * fnvPrime;\n"
            "    return hashCode;\n"
            "}\n"
            "\n"
            "template<typename TKey>\n"
            "static inline uint64_t Fnv(TKey v, typename enable_if<is_pod<TKey>::value>::type* = nullptr)\n"
            "{\n"
            "    return Fnv(reinterpret_cast<uint8_t const*>(&v), sizeof(v));\n"
            "}\n"
            "\n"
            "static inline uint64_t Fnv(string const &v)\n"
            "{\n"
            "    return Fnv(reinterpret_cast<uint8_t const*>(v.c_str()), v.size());\n"
            "}\n"
            "\n";
    }

    void GenerateFunction(ofstream &fo, char const *functionName, char const *keyType) const
    {
        fo <<
            "size_t " << functionName << "(" << keyType << " const &key)\n"
            "{\n"
            "     auto h1 = Djb2(key);\n"
            "     auto h2 = Fnv(key);\n"
            "     switch (h1 % " << mBucketId2MultiplierIdMap.size() << ")\n"
            "     {\n";

        for (size_t i = 0; i < mBucketId2MultiplierIdMap.size(); ++i)
        {
            auto &multiplier = mMultipliers[mBucketId2MultiplierIdMap[i]];
            fo
                << "        case " << i << ":"
                << "return (h1 + h2 *" << multiplier.first << " + " << multiplier.second << ") % " << mSlotCount << ";\n";
        }

        fo <<
            "     }\n"
            "   return 0;\n"
            "}\n";
    }

    void GenerateSlotCountVariable(ofstream &fo, char const *functionName) const
    {
        fo << "size_t " << functionName << "_slotCount = " << mSlotCount << ";";
    }

private:
    size_t mSlotCount;
    vector<uint8_t> mBucketId2MultiplierIdMap;
    vector<pair<uint32_t, uint32_t>> mMultipliers;
};

template<typename TKey>
class PerfectHashBuilder final
{
public:
    PerfectHashBuilder(
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

    PerfectHashFunction<TKey> CreateFunction()
    {
        using namespace PerfectHashUtils;

        auto slotCount = NextPrime(static_cast<size_t>(ceil(mKeys.size() / mLoadFactor)));
        auto bucketCount = NextPrime(static_cast<size_t>(ceil(double(slotCount) / mAssociativity)));
        if (slotCount == 0 || bucketCount == 0)
            throw invalid_argument("slot count/bucket count should not be 0");

        GroupKeysIntoOrderedBuckets(bucketCount);

        vector<uint8_t> bucketId2MultiplierIdMap(bucketCount);
        vector<pair<uint32_t, uint32_t>> multipliers;
        GenerateMultipliers(slotCount, bucketId2MultiplierIdMap, multipliers);

        return PerfectHashFunction<TKey>(slotCount, move(bucketId2MultiplierIdMap), move(multipliers));
    }

    PerfectHashCodeGenerator CreateCodeGenerator()
    {
        using namespace PerfectHashUtils;

        auto slotCount = NextPrime(static_cast<size_t>(ceil(mKeys.size() / mLoadFactor)));
        auto bucketCount = NextPrime(static_cast<size_t>(ceil(double(slotCount) / mAssociativity)));
        if (slotCount == 0 || bucketCount == 0)
            throw invalid_argument("slot count/bucket count should not be 0");

        GroupKeysIntoOrderedBuckets(bucketCount);

        vector<uint8_t> bucketId2MultiplierIdMap(bucketCount);
        vector<pair<uint32_t, uint32_t>> multipliers;
        GenerateMultipliers(slotCount, bucketId2MultiplierIdMap, multipliers);

        return PerfectHashCodeGenerator(slotCount, move(bucketId2MultiplierIdMap), move(multipliers));
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
        typename vector<TKey>::const_iterator keyBegin, typename vector<TKey>::const_iterator keyEnd,
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
        typename vector<TKey>::const_iterator keyBegin, typename vector<TKey>::const_iterator keyEnd,
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


template<typename TKey>
static void GenerateUniqueRandoms(vector<TKey> &keys, Random<TKey> &random)
{
    generate(keys.begin(), keys.end(), random);
    sort(keys.begin(), keys.end());
    keys.erase(unique(keys.begin(), keys.end()), keys.end());
}

template<typename TKey>
static void Test()
{
    Random<TKey> random(kSeed_Test);

    PerfectHashBuilder<TKey> builder(0.5, 8, kSeed_PerfectHashBuilder);

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

static void Test_CodeGen()
{
    string cTokens[] = { "auto", "break", "case", "char", "const", "continue", "default", "do", "double", "else", "enum", "extern", "float", "for", "goto", "if", "inline", "int", "long", "register", "restrict", "return", "short", "signed", "sizeof", "static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while", "_Alignas", "_Alignof", "_Atomic", "_Bool", "_Complex", "_Generic", "_Imaginary", "_Noreturn", "_Static_assert", "_Thread_local", "__func__", "...", ">>=", "<<=", "+=", "-=", "*=", "/=", "%=", "&=", "^=", "|=", ">>", "<<", "++", "--", "->", "&&", "||", "<=", ">=", "==", "!=", ";", "{", "}", ",", ":", "=", "(", ")", "[", "]", ".", "&", "!", "~", "-", "+", "*", "/", "%", "<", ">", "^", "|", "?" };

    PerfectHashBuilder<string> builder(1, 4, kSeed_PerfectHashBuilder);
    for (auto &token : cTokens)
        builder.AddKey(token);

    auto generator = builder.CreateCodeGenerator();
    generator.Generate("hashToken.cpp", "HashToken", "string");
}

template<typename TKey>
static void Benchmark(size_t lengthScale)
{
    Random<TKey> random(kSeed_Benchmark);

    PerfectHashBuilder<TKey> builder(0.5, 8, kSeed_PerfectHashBuilder);

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
    Test<int>();
    Test<string>();
    Test_CodeGen();

#ifdef NDEBUG
    Benchmark<int>(1000 * 1000);
    Benchmark<string>(1000 * 1000);
#else
    Benchmark<int>(1000);
    Benchmark<string>(1000);
#endif
}