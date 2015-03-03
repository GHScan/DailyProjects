//
#include "stdafx.h"

#include <assert.h>
#include <string>

//--------------------------------------------------
class Solution {
public:
	bool isMatch(const char *s, const char *p) {
		if (*p == 0 && *s == 0) return true;
		else if (*p == 0) return false;
		else if (p[1] == '*') {
			for (;;) {
				if (isMatch(s, p + 2)) return true;
				if (!(*s != 0 && *p == '.' || *p == *s)) return false;
				++s;
			}
			return false;
		} else {
			return (*s != 0 && *p == '.' || *p == *s) && isMatch(s + 1, p + 1);
		}
	}
};
//--------------------------------------------------

int main() {
	Solution s;
	cout << s.isMatch("aa","a") << endl;
	cout << s.isMatch("aa","aa") << endl;
	cout << s.isMatch("aaa","aa") << endl;
	cout << s.isMatch("aa", "a*") << endl;
	cout << s.isMatch("aa", ".*") << endl;
	cout << s.isMatch("ab", ".*") << endl;
	cout << s.isMatch("aab", "c*a*b") << endl;
	cout << s.isMatch("aab", "a*a") << endl;
	cout << s.isMatch("a", "a*a") << endl;
	cout << s.isMatch("a", ".*..a*") << endl;
	cout << s.isMatch("a", "ab*") << endl;
	cout << s.isMatch("", "b*") << endl;
	cout << s.isMatch("b", "b*") << endl;
	cout << s.isMatch("", "b") << endl;
}
