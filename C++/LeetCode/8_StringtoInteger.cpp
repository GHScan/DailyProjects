
#include <limits>

class Overflow {};

class Solution {
public:
	int atoi(string str) {
		char const *s = str.c_str();

		int sign = 1;

		for (; isspace(*s); ++s);
		if (*s == '+') ++s;
		else if (*s == '-') {
			++s;
			sign = -1;
		}
		// for (; isspace(*s); ++s);

		try {
			int i = 0;
			for (; isdigit(*s); ++s) {
				i = add(mul(i, 10), *s - '0');
			}

			return i * sign;
		} catch (Overflow const &) {
			if (sign == 1) return numeric_limits<int>::max();
			else return numeric_limits<int>::min();
		}
	}

private:
	int mul(int a, int b) {
		if (a == 0 || b == 0) return 0;
		if (numeric_limits<int>::max() / a < b) throw Overflow();
		return a * b;
	}
	int add(int a, int b) {
		if (numeric_limits<int>::max() - a < b) throw Overflow();
		return a + b;
	}
};
