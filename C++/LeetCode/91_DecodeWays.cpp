#include "stdafx.h"

#include <assert.h>

//-------------------------------------------------------------------------
class Solution {
public:
    int numDecodings(string s) {
        if (s.empty()) return 0;

        vector<int> nums(s.size(), 0);
        for (int i = (int)s.size() - 1; i >= 0; --i) {
            int n = 0;
            if (s[i] >= '1' && s[i] <= '9') n += i + 1 < (int)nums.size() ? nums[i + 1] : 1;
            if (i + 1 < (int)s.size() && s[i] == '1' && s[i + 1] >= '0' && s[i + 1] <= '9') n += i + 2 < (int)nums.size() ? nums[i + 2] : 1;
            if (i + 1 < (int)s.size() && s[i] == '2' && s[i + 1] >= '0' && s[i + 1] <= '6') n += i + 2 < (int)nums.size() ? nums[i + 2] : 1;
            nums[i] = n;
        }
        return nums.front();
    }
};
//-------------------------------------------------------------------------

int main() {
    Solution so;
    cout << so.numDecodings("") << endl;
    cout << so.numDecodings("1") << endl;
    cout << so.numDecodings("12") << endl;
    cout << so.numDecodings("21") << endl;
    cout << so.numDecodings("23") << endl;
    cout << so.numDecodings("32") << endl;
    cout << so.numDecodings("1234") << endl;
}
