//
#include "stdafx.h"

#include <assert.h>
#include <string>

//--------------------------------------------------
class Solution {
public:
	bool isValid(string s) {
		if (const char *p = brackets(s.c_str())) {
			return *p == 0;
		} else {
			return false;
		}
	}
private:
	const char* bracket(const char *s) {
		switch (*s) {
		case '[':
			if ((s = brackets(s + 1)) && *s == ']') return s + 1;
			break;
		case '{':
			if ((s = brackets(s + 1)) && *s == '}') return s + 1;
			break;
		case '(':
			if ((s = brackets(s + 1)) && *s == ')') return s + 1;
			break;
		default: break;
		}
		return nullptr;
	}
	const char* brackets(const char *s) {
		while (const char *r = bracket(s)) s = r;
		return s;
	}
};
//--------------------------------------------------

int main() {
	Solution s;
	const char *strs[] = {
		"", "(", "{]", "()", "()[]{}", "(]", "([)]", "([]({{}})[[]()]){}", "([]({{{})[[]()]){}", 
	};
	for (auto str : strs) cout << str << " => " << s.isValid(str) << endl;
}
