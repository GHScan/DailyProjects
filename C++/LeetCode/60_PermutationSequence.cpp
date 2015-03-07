#include "stdafx.h"

//------------------------------
class Solution {
public:
    Solution() {
        mFactorials[0] = 1;
        for (int i = 1; i < 9; ++i) mFactorials[i] = mFactorials[i - 1] * (i + 1);
    }
    string getPermutation(int n, int k) {
        if (n <= 0 || k <= 0 || k > mFactorials[n - 1]) return "";

        string leftDigits(DIGITS, DIGITS + n);
        string result;
        result.reserve(n);

        int idx = k - 1;
        for (int i = n; i >= 1; --i) {
            int factor = mFactorials[i - 2];
            int j = idx / factor;
            result += leftDigits[j];
            leftDigits.erase(leftDigits.begin() + j);
            idx %= factor;
        }
        return result;
    }
private:
    int mFactorials[9];
    const char *DIGITS = "123456789";
};
//------------------------------

int main() {
    Solution s;

    cout << s.getPermutation(-1, -1) << endl;
    cout << s.getPermutation(0, 0) << endl;
    cout << s.getPermutation(2, 0) << endl;
    cout << s.getPermutation(0, 2) << endl;
    for (int i = 0; i < 6; ++i) cout << s.getPermutation(3, i + 1) << endl;
    for (int i = 0; i < 24; ++i) cout << s.getPermutation(4, i + 1) << endl;
    for (int i = 1; i <= 9; ++i) cout << s.getPermutation(i, 1) << "," << s.getPermutation(i, 2) << endl;
}
