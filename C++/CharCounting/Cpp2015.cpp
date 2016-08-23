#include "stdafx.h"

#include <cstdint>
#include <chrono>
#include <iostream>
#include <random>
#include <vector>
#include <algorithm>
using namespace std;


template <typename TFunc>
static void TimeIt(char const* name, int times, TFunc func) {
    if (times > 1) func();

    auto start = chrono::high_resolution_clock::now();
    for (auto i = 0; i < times; ++i) {
        func();
    }
    auto end = chrono::high_resolution_clock::now();

    cout << name << " : " << chrono::duration<double>(end - start).count() / times << " s" << endl;
}


int main() {
    default_random_engine randomEngine;
    uniform_int_distribution<int> randomDist('a', 'z');

    size_t const length = 10 * 1000 * 1000;
    string str(length + 16, '\0');
    generate(str.begin(), str.begin() + length, [&randomEngine, &randomDist]()
    {
        return randomDist(randomEngine);
    });


    // problem: counting the occurrence of each character in a length unpredictable string


    vector<uint32_t> result;

    TimeIt("sequential counting", 3, [&str, &result]() {

        uint32_t counts[128] = { 0 };

        for (auto *p = str.c_str(); *p; ++p)
            ++counts[*p];

        result.assign(begin(counts), end(counts));
    });

    TimeIt("unrolled counting", 3, [&str, &result]() {

        uint32_t counts[128] = { 0 };

        for (auto *p = str.c_str(); *p; p += 8) {
            ++counts[p[0]];
            ++counts[p[1]];
            ++counts[p[2]];
            ++counts[p[3]];
            ++counts[p[4]];
            ++counts[p[5]];
            ++counts[p[6]];
            ++counts[p[7]];
        }

        for (auto c = 'a'; c <= 'z'; ++c) {
            if (result[c] != counts[c])
                throw logic_error("");
        }
    });

    TimeIt("parallel counting", 3, [&str, &result]() {

        uint32_t pcounts[8][128] = { 0 };

        for (auto *p = str.c_str(); *p; p += 8) {
            ++pcounts[0][p[0]];
            ++pcounts[1][p[1]];
            ++pcounts[2][p[2]];
            ++pcounts[3][p[3]];
            ++pcounts[4][p[4]];
            ++pcounts[5][p[5]];
            ++pcounts[6][p[6]];
            ++pcounts[7][p[7]];
        }

        for (auto c = 'a'; c <= 'z'; ++c) {
            uint32_t sum = 0;
            for (auto &counts : pcounts)
                sum += counts[c];
            if (result[c] != sum)
                throw logic_error("");
        }
    });

    TimeIt("16bit counting", 3, [&str, &result]() {

        uint32_t b16Counts[65536] = { 0 };

        for (auto *p = reinterpret_cast<uint16_t const*>(str.c_str()); *p; p += 8) {
            ++b16Counts[p[0]];
            ++b16Counts[p[1]];
            ++b16Counts[p[2]];
            ++b16Counts[p[3]];
            ++b16Counts[p[4]];
            ++b16Counts[p[5]];
            ++b16Counts[p[6]];
            ++b16Counts[p[7]];
        }

        uint32_t counts[128] = { 0 };
        for (auto i = 0; i < 65536; ++i) {
            counts[i & 0xff] += b16Counts[i];
            counts[i >> 8] += b16Counts[i];
        }

        for (auto c = 'a'; c <= 'z'; ++c) {
            if (result[c] != counts[c])
                throw logic_error("");
        }
    });

    cout << '\n';
    for (auto c = 'a'; c <= 'z'; ++c)
        cout << c << ' ' << result[c] << '\n';
}