
#include <stdio.h>
#include <algorithm>
#include <vector>
#include <iostream>
using namespace std;

#include "Timer.h"

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage : %s n\n", argv[0]);
        return 1;
    }

    const int PASS = 40;
    const int n = atoi(argv[1]);
    const int step = (n + PASS - 1) / PASS;

    vector<int> nums;
    for (int i; cin >> i;) nums.push_back(i);
    vector<int> nums2;
    nums2.reserve(nums.size());

    {
        Timer _t(stderr, "libpasssort");

        for (int start = 0; start < n; start += step) {
            int end = start + step;
            int oi = (int)nums2.size();
            for (auto i : nums) {
                if (i >= start && i < end) nums2.push_back(i);
            }
            sort(nums2.begin() + oi, nums2.end());
        }
    }

    for (auto i : nums2) cout << i << '\n';
}
