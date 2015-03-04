#include "stdafx.h"

#include <assert.h>
#include <string>

//--------------------------------------------------
class Solution {
public:
	int strStr(const char *haystack, const char *needle) {
		if (haystack == nullptr || needle == nullptr) return -1;

		int len2 = length(needle);
		int charJump[256];
		for (auto &jump : charJump) jump = len2 + 1;
		for (int i = 0; i < len2; ++i) charJump[(unsigned char)needle[i]] = len2 - i;

		const char *s = haystack;
		for (;;) {
			for (int i = 0; i < len2; ++i) {
				if (s[i] == 0) return -1;
			}

			if (compareN(s, needle, len2) == 0) return s - haystack;

			if (s[len2] == 0) return -1;

			s += charJump[(unsigned char)s[len2]];
		}
	}
private:
	int length(const char *s) {
		int len = 0;
		while (*s++) ++len;
		return len;
	}
	int compareN(const char *s1, const char *s2, int len) {
		for (int i = 0; i < len; ++i, ++s1, ++s2) {
			if (*s1 != *s2) return *s1 - *s2;
		}
		return 0;
	}
};
//--------------------------------------------------

int main() {
	Solution s;

	cout << s.strStr(nullptr, nullptr) << endl;
	cout << s.strStr(nullptr, "abc") << endl;
	cout << s.strStr("abc", nullptr) << endl;
	cout << s.strStr("", "") << endl;
	cout << s.strStr("", "abc") << endl;
	cout << s.strStr("abc", "") << endl;

	cout << s.strStr("abcdefghijk", "abc") << endl;
	cout << s.strStr("abcdefghijk", "0abc") << endl;
	cout << s.strStr("abcdefghijk", "aaa") << endl;
	cout << s.strStr("abcdefghijk", "ijk") << endl;
	cout << s.strStr("abcdefghijk", "ijkl") << endl;

	cout << s.strStr("aaa", "") << endl;
	cout << s.strStr("aaa", "a") << endl;
	cout << s.strStr("aaa", "aa") << endl;
	cout << s.strStr("aaa", "aaa") << endl;
	cout << s.strStr("aaa", "aaaa") << endl;

	cout << s.strStr("中国", "中国") << endl;
	cout << s.strStr("中国人", "中国") << endl;
	cout << s.strStr("中国人", "美国") << endl;
	cout << s.strStr("中国美国人", "美国") << endl;
	cout << s.strStr("中国美国", "美国") << endl;

	cout << s.strStr("mississippi", "issi") << endl;
}
