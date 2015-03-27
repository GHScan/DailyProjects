#include "stdafx.h" 

//-----------------------------------------------------------------
class Solution {
public:
    vector<vector<int> > generate(int numRows) {
        vector<vector<int>> result;
        if (numRows <= 0) return result;

        result.push_back(vector<int>(1, 1));

        for (int i = 2; i <= numRows; ++i) {
            vector<int> curr(1, 1), &last = result.back();
            for (int j = 0; j < i - 2; ++j) {
                curr.push_back(last[j] + last[j + 1]);
            }
            curr.push_back(1);
            result.push_back(curr);
        }

        return result;
    }
};
//-----------------------------------------------------------------
static void printResult(vector<vector<int>> const &result) {
    cout << "############" << endl;
    for (auto &v : result) {
        for (auto i : v) cout << i << ',';
        cout << endl;
    }
}

int main() {
    Solution so;
    printResult(so.generate(0));
    printResult(so.generate(1));
    printResult(so.generate(2));
    printResult(so.generate(5));
}
