
#include <stdio.h>
#include <algorithm>
#include <vector>
#include <iostream>
using namespace std;

#include "Timer.h"

int main() {
    vector<int> nums;

    for (int n; cin >> n;) nums.push_back(n);

    {
        Timer _t(stderr, "libsort");
        sort(nums.begin(), nums.end());
    }

    for (auto i : nums) cout << i << '\n';
}
