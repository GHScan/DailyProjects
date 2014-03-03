
#include <stdio.h>
#include <stdlib.h>

#include <iostream>
using namespace std;

#include "BitVector.h"
#include "Timer.h"

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage : %s n\n", argv[0]);
        return 1;
    }

    const int PASS = 2;
    const int n = atoi(argv[1]);
    const int step = (n + PASS - 1) / PASS;

    vector<int> nums;
    for (int i; cin >> i; ) nums.push_back(i);
    vector<int> nums2;
    nums2.reserve(nums.size());

    {
        Timer _t(stderr, "bitvector pass");

        BitVector v(step);
        for (int start = 0; start < n; start += step) {
            int end = start + step;
            for (auto i : nums) {
                if (i >= start && i < end) v.set(i - start, 1);
            }
            for (int i = 0; i < v.getSize(); ++i) {
                if (v.get(i)) {
                    nums2.push_back(start + i);
                    v.set(i, 0);
                }
            }
        }
    }

    for (auto i : nums2) cout << i << '\n';

    return 0;
}
