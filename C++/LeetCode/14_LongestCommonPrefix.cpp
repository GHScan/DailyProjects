//
#include "stdafx.h"

#include <assert.h>
#include <string>
//--------------------------------------------------
class Solution {
public:
	string longestCommonPrefix(vector<string> &strs) {
		if (strs.empty()) return "";
		else if (strs.size() == 1) return strs.front();

		int len = commonPrefix(strs[0], strs[1]);
		for (int i = 2; i < (int)strs.size(); ++i) {
			len = shorterCommonPrefix(strs.front(), strs[i], len);
		}

		return strs.front().substr(0, len);
	}
private:
	int commonPrefix(string const &a, string const &b) {
		int len = 0;
		while (len < (int)a.size() && len < (int)b.size() && a[len] == b[len]) ++len;
		return len;
	}
	int shorterCommonPrefix(string const &a, string const &b, int len) {
		if (strncmp(a.c_str(), b.c_str(), len) == 0) return len;
		return commonPrefix(a, b);
	}
};
//--------------------------------------------------

int main() {
	Solution s;
	{
		string a[] = {"abcdef", "abcde", "abc", "abcbb"};
		vector<string> v(a, a + 4);
		cout << s.longestCommonPrefix(v) << endl;
	}
}
