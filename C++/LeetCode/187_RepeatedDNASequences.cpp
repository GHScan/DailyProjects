#include <chrono>

template <typename TFunc>
inline double Time(TFunc const &f, int loop = 3) {
    using namespace std::chrono;

    if (loop > 1)
        f();

    auto t = 0.0;
    for (auto i = 0; i < loop; ++i) {
        auto start = high_resolution_clock::now();
        f();
        auto end = high_resolution_clock::now();
        t = t + duration<double>(end - start).count();
    }

    return t / loop;
}



#include <cstdio>
#include <cassert>

#include <list>
#include <stack>
#include <string>
#include <vector>
#include <numeric>
#include <iostream>
#include <iterator>
#include <algorithm>
#include <unordered_set>
#include <unordered_map>

using namespace std;

class Solution_compact {
public:
    vector<string> findRepeatedDnaSequences(string s_)
    {
        int len = (int)s_.size();
        char const *s = s_.c_str();

        vector<string> result;
        if (len < keySize) return result;

        unordered_map<int32_t, int32_t> strs;

        int32_t hash = 0;
        for (int i = 0; i < keySize; ++i)
            hash = (hash << 2) + mapping[s[i]];
        strs[hash] = 1;
        int32_t mask = (1 << (2 * keySize - 2)) - 1;

        for (int ei = keySize; ei < len; ++ei)
        {
            hash = ((hash & mask) << 2) + mapping[s[ei]];
            auto it = strs.find(hash);
            if (it == strs.end())
            {
                strs.insert(make_pair(hash, 1));
            }
            else
            {
                if (it->second == 1)
                    result.emplace_back(s + ei + 1 - keySize, s + ei + 1);
                ++it->second;
            }
        }

        return result;
    }

    Solution_compact()
    {
        mapping['A'] = 0;
        mapping['C'] = 1;
        mapping['G'] = 2;
        mapping['T'] = 3;
    }

    int8_t mapping[128];
    static constexpr int keySize = 10;
};

class Solution_fast {
public:
    class Counter
    {
    public:
        Counter(int32_t idxBits)
        {
            wordCount = 1 << int32_t(idxBits - 4);
            counts = new int32_t[wordCount];
            reset();
        }

        ~Counter()
        {
            delete[] counts;
        }

        void reset()
        {
            memset(counts, 0, wordCount * sizeof(int32_t));
        }

        int lookupAndInc(int32_t i)
        {
            int32_t wordIdx = i * 2 / 32;
            int32_t wordOff = i * 2 % 32;
            int32_t word = counts[wordIdx];
            int32_t count = (word >> wordOff) & 0x3;
            word = (word & ~(0x3 << wordOff)) | (min(count + 1, 2) << wordOff);
            counts[wordIdx] = word;
            return count;
        }

    private:
        int32_t *counts;
        int32_t wordCount;
    };

    vector<string> findRepeatedDnaSequences(string s_)
    {
        int len = (int)s_.size();
        char const *s = s_.c_str();

        vector<string> result;
        if (len < 10) return result;

        counter.reset();

        int32_t hash = 0;
        for (int i = 0; i < 10; ++i)
            hash = (hash << 2) + mapping[s[i]];
        counter.lookupAndInc(hash);

        for (int ei = 10; ei < len; ++ei)
        {
            hash = ((hash & 0x3ffff) << 2) + mapping[s[ei]];
            int32_t count = counter.lookupAndInc(hash);
            if (count == 1)
                result.emplace_back(s + ei - 9, s + ei + 1);
        }

        return result;
    }

    Solution_fast()
        : counter(20)
    {
        memset(mapping, 0, sizeof mapping);
        mapping['A'] = 0;
        mapping['C'] = 1;
        mapping['G'] = 2;
        mapping['T'] = 3;
    }

    Counter counter;
    int8_t mapping[128];
};

int main()
{
    for (auto &test : vector<string>
        {
            "AAAA",
            "AAAAAAAAAAA",
            "TTTTTTTTTTT",
            "AAAAACCCCCAAAAACCCCCCAAAAAGGGTTT",
            string(100000, 'A'),
        })
    {
        vector<string> result;
        auto t0 = Time([&]() {
                result = Solution_fast().findRepeatedDnaSequences(test);
            });
        cout << "----------------------\n";
        cout << t0 * 1e3 << " ms\n";
        copy(result.begin(), result.end(), ostream_iterator<string>(cout, "\n"));
    }
}