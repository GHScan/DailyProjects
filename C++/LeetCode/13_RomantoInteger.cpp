//
#include "stdafx.h"

#include <assert.h>
#include <string>
//--------------------------------------------------
class Solution {
public:
	Solution() {
		romanChar2Int['I'] = 1;
		romanChar2Int['V'] = 5;
		romanChar2Int['X'] = 10;
		romanChar2Int['L'] = 50;
		romanChar2Int['C'] = 100;
		romanChar2Int['D'] = 500;
		romanChar2Int['M'] = 1000;
	}
	int romanToInt(string const &s) {
		int result = 0;

		int lastCharInt = 0;
		auto it = s.rbegin();
		while (it != s.rend()) {
			char c = *it;
			int count = 1;
			for (++it; it != s.rend() && *it == c; ++it, ++count);

			int charInt = romanChar2Int[c];
			int num = count * charInt;
			result += charInt > lastCharInt ? num : -num;
			lastCharInt = charInt;
		}

		return result;
	}
private:
	int romanChar2Int[128];
};
//--------------------------------------------------

int main() {
	Solution s;
	char const *romans[] = {
		"I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII", "XIII", "XIV", "XV", "XVI", "XX", "XXX", "XL", "XC", "XCIX", "C", "CI", "CII", "CXCIX", "CC", "DCCC", "CM", "MMM", "MMMCCCXXXIII", "MMMCMXCIX",
	};
	for (auto roman : romans) {
		cout << roman << "=>" << s.romanToInt(roman) << endl;
	}
}
