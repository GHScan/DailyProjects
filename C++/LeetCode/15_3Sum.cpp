//
#include "stdafx.h"

#include <assert.h>
#include <string>
//--------------------------------------------------
#include <algorithm>

class Solution {
public:
	vector<vector<int> > threeSum(vector<int> &num) {
		vector<vector<int>> result;

		sort(num.begin(), num.end());

		for (int i = 0; i < (int)num.size(); ) {
			int iv = num[i];

			for (int j = i + 1; j < (int)num.size(); ) {
				int jv = num[j];

				int kv = -(iv + jv);
				if (kv >= jv) {
					auto it = lower_bound(num.begin() + j + 1, num.end(), kv);
					if (it != num.end() && *it == kv) {
						int a[] = {iv, jv, kv};
						result.push_back(vector<int>(a, a + 3));
					}
				}

				for (++j; j < (int)num.size() && num[j] == jv; ++j);
			}

			for (++i; i < (int)num.size() && num[i] == iv; ++i);
		}

		return result;
	}
};
//--------------------------------------------------

static void printResult(vector<vector<int>> const &result) {
	cout << "#######" << endl;
	for (auto &v : result) {
		for (auto i : v) cout << i << ',';
		cout << endl;
	}
}

int main() {
	Solution s;
	{
		int a[] = {-1, 0, 1, 2, -1, -4,};
		printResult(s.threeSum(vector<int>(a, a + sizeof(a) / sizeof(a[0]))));
	}
	{
		int a[] = {0, 0, 1, 2, 0, 0, -3, 4, -4};
		printResult(s.threeSum(vector<int>(a, a + sizeof(a) / sizeof(a[0]))));
	}
}
