#include "stdafx.h"

//------------------------------
#include <algorithm>
#include <map>

struct VectorLess {
    bool operator () (vector<int> const &a, vector<int> const &b) const {
        return lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
    }
};

class Solution {
public:
    vector<string> anagrams(vector<string> &strs) {
        map<vector<int>, vector<string>, VectorLess> flag2Strs;

        vector<int> flag(26, 0);
        for (auto &s : strs) {
            flag.assign(flag.size(), 0);
            for (auto c : s) ++flag[c - 'a'];

            flag2Strs[flag].push_back(s);
        }

        for (auto &p : flag2Strs) {
            if (p.second.size() > 1) return p.second;
        }
        return {};
    }
};

//------------------------------
#define countOf(a) int(sizeof(a) / sizeof(a[0]))

static void printResult(vector<string> &result) {
    cout << "#######" << endl;
    for (auto &v : result) {
        cout << v << endl;
    }
}

int main() {
    Solution s;
    printResult(s.anagrams(vector<string>{}));
    printResult(s.anagrams(vector<string>{"tea"}));
    printResult(s.anagrams(vector<string>{ "tea", "and"}));
    printResult(s.anagrams(vector<string>{ "tea", "ate" }));
    printResult(s.anagrams(vector<string>{"tea", "and", "ate", "eat", "den"}));
}
