//
#include "stdafx.h"

#include <assert.h>
#include <string>
//--------------------------------------------------
#include <algorithm>
#include <limits>

class Solution {
public:
	int threeSumClosest(vector<int> &num, int target) {
		int result = -1;
		int minDiff = numeric_limits<int>::max();

		sort(num.begin(), num.end());

		for (int i = 0; i < (int)num.size(); ) {
			int iv = num[i];

			for (int j = i + 1; j < (int)num.size(); ) {
				int jv = num[j];

				int expect = target - (iv + jv);
				auto begin = num.begin() + j + 1;
				auto it = lower_bound(begin, num.end(), expect);
				vector<int>::iterator k;
				if (it == num.end()) k = it - 1;
				else if (it == begin) k = it;
				else {
					int kv1 = *it, kv2 = *(it - 1);
					k = abs(kv1 - expect) < abs(kv2 - expect) ? it : it - 1;
				}
				if (k - num.begin() > j) {
					int diff = abs(*k - expect);
					if (diff < minDiff) {
						minDiff = diff;
						result = iv + jv + *k;
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
int main() {
	Solution s;
	{
		int a[] = {-1, 2, 1, -4,};
		cout << s.threeSumClosest(vector<int>(a, a + sizeof(a) / sizeof(a[0])), 1) << endl;
	}
}
