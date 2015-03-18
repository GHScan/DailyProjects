#include "stdafx.h"

//------------------------------
class Solution {
public:
    vector<vector<int>> combine(int n, int k) {
        vector<vector<int>> results;
        if (n <= 0 || k < 0 || k > n) return results;

        vector<int> temp;
        temp.reserve(k);
        combine(results, temp, 1, n, k);

        return results;
    }
private:
    void combine(vector<vector<int>> &results, vector<int> &temp, int first, int last, int count) {
        if ((int)temp.size() == count) {
            results.push_back(temp);
            return;
        }
        if (first > last) return;

        for (; first <= last; ++first) {
            temp.push_back(first);
            combine(results, temp, first + 1, last, count);
            temp.pop_back();
        }
    }
};
//------------------------------
static void printResult(vector<vector<int>> const &results) {
    cout << "###############" << endl;
    for (auto &v : results) {
        for (auto i : v) cout << i << ',';
        cout << endl;
    }
}

int main() {
    Solution s;
    printResult(s.combine(0, 0));
    printResult(s.combine(1, 0));
    printResult(s.combine(0, 1));
    printResult(s.combine(2, 1));
    printResult(s.combine(1, 2));
    printResult(s.combine(3, 1));
    printResult(s.combine(4, 1));
    printResult(s.combine(3, 2));
    printResult(s.combine(4, 2));
    printResult(s.combine(3, 3));
    printResult(s.combine(4, 3));
}
