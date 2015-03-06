#include "stdafx.h"

//------------------------------
class Solution {
public:
    vector<vector<int>> permute(vector<int> &num) {
        vector<vector<int>> result;
        perm(result, num, 0);
        return result;
    }
private:
    void perm(vector<vector<int>> &result, vector<int> &num, int i) {
        if (i == (int)num.size()) {
            result.push_back(num);
            return;
        }
        for (int j = i; j < (int)num.size(); ++j) {
            swap(num[i], num[j]);
            perm(result, num, i + 1);
            swap(num[i], num[j]);
        }
    }
};

//------------------------------
#define countOf(a) int(sizeof(a) / sizeof(a[0]))

static void printResult(vector<vector<int>> &result) {
    cout << "#######" << endl;
    for (auto &v : result) {
        for (auto i : v) cout << i << ',';
        cout << endl;
    }
}

int main() {
    Solution s;
    printResult(s.permute(vector<int>{}));
    printResult(s.permute(vector<int>{1}));
    printResult(s.permute(vector<int>{1,2}));
    printResult(s.permute(vector<int>{1,2,3}));
}
