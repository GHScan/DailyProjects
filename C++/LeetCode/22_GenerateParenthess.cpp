#include "stdafx.h"

#include <assert.h>
#include <string>

//--------------------------------------------------
#include <unordered_map>

class Solution {
public:
	Solution() {
		mMem[0] = vector<string>(1, "");
	}
	vector<string> const & generateParenthesis(int n) {
		auto it = mMem.find(n);
		if (it != mMem.end()) return it->second;

		vector<string> result;
		for (int i = 0; i <= n - 1; ++i) {
			for (auto &inner : generateParenthesis(i)) {
				for (auto &suffix : generateParenthesis(n - 1 - i)) {
					string s(1, '(');
					s += inner;
					s += ')';
					s += suffix;
					result.push_back(s);
				}
			}
		}

		return mMem[n] = result;
	}
private:
	unordered_map<int, vector<string>> mMem;
};
//--------------------------------------------------
static void printResult(vector<string> const &v) {
	for (auto &s : v) cout << s << ',';
	cout << endl;
}

int main() {
	Solution s;
	for (int i = 0; i < 5; ++i) printResult(s.generateParenthesis(i));
}
