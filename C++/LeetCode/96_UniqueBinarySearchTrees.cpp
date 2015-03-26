#include "stdafx.h" 

//-----------------------------------------------------------------
class Solution {
public:
    int numTrees(int n) {
        if (n == 0) return 0;
        
        vector<int> nums(n + 1, 0);
        nums[0] = 1;
        nums[1] = 1;
        for (int i = 2; i <= n; ++i) {
            int num = 0;
            for (int j = 0; j < i; ++j) {
                num += nums[j] * nums[i - j -1];
            }
            nums[i] = num;
        }
        return nums.back();
    }
};
//-----------------------------------------------------------------

int main() {
    Solution so;
    for (int i = 0; i < 20; ++i) cout << so.numTrees(i) << endl;
}
