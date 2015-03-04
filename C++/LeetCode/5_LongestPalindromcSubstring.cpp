#include "stdafx.h"

#include <assert.h>
#include <string>

//--------------------------------------------------

class Solution {
public:
	string longestPalindrome(string const &s) {
		int maxLen = 0;
		int pos = 0;
		for (int i = 0; i < (int)s.size(); ++i) {
			int len = max(palindromeLength(s, i - 1, i + 1), palindromeLength(s, i - 1, i));
			if (len > maxLen) {
				maxLen = len;
				pos = i - len / 2;
			}
		}
		return s.substr(pos, maxLen);
	}
private:
	int palindromeLength(string const &s, int c1, int c2) {
		for (int i = min(c1 + 1, (int)s.size() - c2); i > 0 && s[c1] == s[c2]; --i, --c1, ++c2);
		return c2 - c1 - 1;
	}
};

//--------------------------------------------------

int main() {
	Solution s;
	cout << s.longestPalindrome("a") << endl;
	cout << s.longestPalindrome("abcabcbad") << endl;
	cout << s.longestPalindrome("baaa") << endl;
	cout << s.longestPalindrome("aaaab") << endl;
	cout << s.longestPalindrome("aabcabc") << endl;
	cout << s.longestPalindrome("aaaaba") << endl;
	cout << s.longestPalindrome("bbaaaabcaab") << endl;
}
