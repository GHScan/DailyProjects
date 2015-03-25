#include "stdafx.h"

#include <assert.h>

//-------------------------------------------------------------------------
#include <algorithm>

class Solution {
public:
    vector<vector<int> > subsetsWithDup(vector<int> &S) {
        vector<vector<int>> result;
        if (S.empty()) return result;

        sort(S.begin(), S.end());
        
        vector<int> path;
        generate(result, path, &S[0], &S[0] + S.size());
        return result;
    }
private:
    void generate(vector<vector<int>> &result, vector<int> &path, int *begin, int *end) {
        if (begin == end) {
            result.push_back(path);
            return;
        }

        int v = *begin;
        int count = 0;
        for (; begin != end && *begin == v; ++begin, ++count);

        generate(result, path, begin, end);
        for (int i = 0; i < count; ++i) {
            path.push_back(v);
            generate(result, path, begin, end);
        }
        path.erase(path.begin() + path.size() - count, path.end());
    }
};
//-------------------------------------------------------------------------
static void printResult(vector<vector<int>> const &result) {
    cout << "############" << endl;
    for (auto &v : result) {
        for (auto i : v) cout << i << ',';
        cout << endl;
    }
}

int main() {
    Solution so;
    printResult(so.subsetsWithDup(vector<int>{}));
    printResult(so.subsetsWithDup(vector<int>{1}));
    printResult(so.subsetsWithDup(vector<int>{1, 2, 2}));
    printResult(so.subsetsWithDup(vector<int>{1, 2, 3}));
    printResult(so.subsetsWithDup(vector<int>{1, 2, 2, 3}));
}
