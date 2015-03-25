#include "stdafx.h"

#include <assert.h>

//-------------------------------------------------------------------------
class Solution {
public:
    vector<int> grayCode(int n) {
        vector<int> result(1, 0);
        vector<bool> generated(1 << n, false);
        generated[0] = true;
        
        for (int i = 1; i < 1 << n; ++i) {
            int last = result.back();
            for (int j = 0; j < 32; ++j) {
                int v = last ^ (1 << j);
                if (!generated[v]) {
                    generated[v] = true;
                    result.push_back(v);
                    break;
                }
            }
        }

        return result;
    }
};
//-------------------------------------------------------------------------
static void printResult(vector<int> const &result) {
    for (auto i : result) cout << i << ',';
    cout << endl;
}

int main() {
    Solution so;
    printResult(so.grayCode(0));
    printResult(so.grayCode(1));
    printResult(so.grayCode(2));
    printResult(so.grayCode(3));
    printResult(so.grayCode(4));
    so.grayCode(16);
}
