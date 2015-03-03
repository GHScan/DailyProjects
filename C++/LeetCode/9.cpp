#include <math.h>

class Solution {
public:
	bool isPalindrome(int x) {
		if (x < 0) return false;
		else if (x < 10) return true;

		int digits = (int)ceil(log(x + 1) / log(10));
		return isPalindrome(x, (int)pow(10, digits - 1));
	}
private:
	bool isPalindrome(int x, int factor) {
		if (factor == 1) return true;
		else if (factor == 10) return x / 10 == x % 10;
		else {
			return x / factor == x % 10 && isPalindrome(x % factor / 10, factor / 100);
		}
	}
};
