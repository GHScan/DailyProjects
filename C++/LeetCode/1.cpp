#include "pch.h" 

#include <assert.h>
#include <vector>
#include <unordered_map>

class Solution {
    public:
        vector<int> twoSum(vector<int> &numbers, int target) {
            unordered_multimap<int, int> h;
            for (int i = 0; i < (int)numbers.size(); ++i) {
                h.insert(make_pair(numbers[i], i + 1));
            }

            for (auto numIdx : h) {
                auto range = h.equal_range(target - numIdx.first);
                auto begin = range.first, end = range.second;
                while (begin != end && begin->second == numIdx.second) ++begin;
                if (begin != end) {
                    auto i1 = numIdx.second, i2 = begin->second;
                    return {min(i1, i2), max(i1, i2)};
                }
            }

            assert(0);
            return {0, 0};
        }
};

int main() {
    Solution so;
    vector<int> v{0, 1, 2, 0};
    auto r = so.twoSum(v, 0);
    cout << r[0] << ',' << r[1] << endl;
}
