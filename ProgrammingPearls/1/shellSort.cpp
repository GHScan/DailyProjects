#include <assert.h>
#include <stdio.h>

#include <algorithm>
#include <vector>
#include <iostream>
using namespace std;

#include "Timer.h"

template<typename T>
static void shellSort(T *begin, T *end) {
    for (int gap = (end - begin) / 2; gap > 0; gap /= 2) {
        for (T *cur = begin + gap; cur < end; ++cur) {
            for (T *the = cur - gap; the >= begin && the[0] > the[gap]; the -= gap) {
                swap(the[0], the[gap]);
            }
        }
    }
}

int main() {
    vector<int> nums;

    for (int n; cin >> n;) nums.push_back(n);

    {
        Timer _t(stderr, "shellSort");
        shellSort(&nums[0], &nums[0] + nums.size());
    }

    for (auto i : nums) cout << i << '\n';
}
