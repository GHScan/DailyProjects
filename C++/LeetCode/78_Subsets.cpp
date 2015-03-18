#include "stdafx.h"

//------------------------------
#include <algorithm>

class Solution {
public:
    vector<vector<int> > subsets(vector<int> &S) {
        vector<vector<int>> results;
        if (S.empty()) return results;

        sort(S.begin(), S.end());
        
        vector<int> temp;
        generate(results, temp, &S[0], &S[0] + S.size());
        return results;
    }
private:
    void generate(vector<vector<int>> &results, vector<int> &temp, int *begin, int *end) {
        if (begin == end) {
            results.push_back(temp);
            return;
        }

        generate(results, temp, begin + 1, end);

        temp.push_back(*begin);
        generate(results, temp, begin + 1, end);
        temp.pop_back();
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
    printResult(s.subsets(vector<int>{}));
    printResult(s.subsets(vector<int>{1}));
    printResult(s.subsets(vector<int>{2, 1}));
    printResult(s.subsets(vector<int>{3, 2, 1}));
    printResult(s.subsets(vector<int>{4, 3, 2, 1}));
}
