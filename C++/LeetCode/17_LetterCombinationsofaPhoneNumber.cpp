//
#include "stdafx.h"

#include <assert.h>
#include <string>
//--------------------------------------------------
class Solution {
private:
	string mDigit2Letters[10];
public:
	Solution() {
		mDigit2Letters[2] = "abc";
		mDigit2Letters[3] = "def";
		mDigit2Letters[4] = "ghi";
		mDigit2Letters[5] = "jkl";
		mDigit2Letters[6] = "mno";
		mDigit2Letters[7] = "pqrs";
		mDigit2Letters[8] = "tuv";
		mDigit2Letters[9] = "wxyz";
	}

	vector<string> letterCombinations(string digits) {
		vector<string> result;
		for (auto digit : digits) {
			vector<string> newResult;
			combine(digit, newResult, result);
			swap(result, newResult);
		}
		return result;
	}

private:
	void combine(char digit, vector<string> &out, vector<string> const &in) {
		auto &letters = mDigit2Letters[digit - '0'];
		out.reserve(letters.size() * in.size());

		if (in.empty()) {
			for (auto c : letters) out.push_back(string(1, c));
		} else {
			for (auto s : in) {
				for (auto c : letters) {
					out.push_back(s + c);
				}
			}
		}
	}
};
//--------------------------------------------------
int main() {
	Solution s;

	for (auto str : s.letterCombinations("")) cout << str << ',';
	cout << endl;

	for (auto str : s.letterCombinations("1")) cout << str << ',';
	cout << endl;

	for (auto str : s.letterCombinations("23")) cout << str << ',';
	cout << endl;
}
