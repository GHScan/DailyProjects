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


    // problem: counting the occurrence of each character for a length unpredictable string


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

        uint32_t counts[8][128] = { 0 };

        for (auto *p = str.c_str(); *p; p += 8) {
            ++counts[0][p[0]];
            ++counts[1][p[1]];
            ++counts[2][p[2]];
            ++counts[3][p[3]];
            ++counts[4][p[4]];
            ++counts[5][p[5]];
            ++counts[6][p[6]];
            ++counts[7][p[7]];
        }

        for (auto c = 'a'; c <= 'z'; ++c) {
            uint32_t sum = 0;
            for (auto &counts2 : counts)
                sum += counts2[c];
            if (result[c] != sum)
                throw logic_error("");
        }
    });

    cout << '\n';
    for (auto c = 'a'; c <= 'z'; ++c)
        cout << c << ' ' << result[c] << '\n';
}