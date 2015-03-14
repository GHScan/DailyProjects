#include "stdafx.h"

//------------------------------
class Solution {
public:
    vector<int> plusOne(vector<int> &digits) {
        int carry = 1;
        for (int i = (int)digits.size() - 1; i >= 0; --i) {
            carry += digits[i];
            digits[i] = carry % 10;
            carry /= 10;
        }
        if (carry > 0) digits.insert(digits.begin(), carry);
        return digits;
    }
};
//------------------------------
#include <initializer_list>

static void printResult(vector<int> const &result) {
    for (auto i : result) cout << i << ',';
    cout << endl;
}

static vector<int> makeVector(std::initializer_list<int> ints) {
    return vector<int>(ints.begin(), ints.end());
}

int main() {
    Solution s;
    printResult(s.plusOne(makeVector({0})));
    printResult(s.plusOne(makeVector({1})));
    printResult(s.plusOne(makeVector({9})));
    printResult(s.plusOne(makeVector({1, 0})));
    printResult(s.plusOne(makeVector({ 1, 1 })));
    printResult(s.plusOne(makeVector({1, 9})));
    printResult(s.plusOne(makeVector({ 9, 9 })));
}
