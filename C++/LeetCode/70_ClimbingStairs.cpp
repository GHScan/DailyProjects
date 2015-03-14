#include "stdafx.h"

//------------------------------
class Solution {
public:
    Solution(): mFibs({1, 2}) {
    }
    int climbStairs(int n) {
        while ((int)mFibs.size() < n) {
            mFibs.push_back(mFibs.back() + mFibs[mFibs.size() - 2]);
        }
        return mFibs[n - 1];
    }
private:
    vector<int> mFibs;
};
//------------------------------

int main() {
    Solution s;
    for (int i = 1; i < 32; ++i) cout << s.climbStairs(i) << endl;
}
