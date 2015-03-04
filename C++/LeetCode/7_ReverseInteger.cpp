//
#include "stdafx.h"

#include <assert.h>
#include <string>

//--------------------------------------------------
#include <limits>

class Overflow {};

class Solution {
public:
	int reverse(int x) {
		try {
			return x < 0 ? -reversePositive(-x, 0) : reversePositive(x, 0);
		} catch (Overflow const&) {
			return 0;
		}
	}
private:
	int reversePositive(int x, int result) {
		if (x == 0) return result;
		else return reversePositive(x / 10, add(x % 10, mul(result, 10)));
	}
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

//--------------------------------------------------

int main() {
	Solution s;
	cout << s.reverse(123) << endl;
	cout << s.reverse(-123) << endl;
	cout << s.reverse(1534236469) << endl;
}
