//
#include "stdafx.h"

#include <assert.h>
#include <string>

//--------------------------------------------------
#include <vector>

struct Symbol {
	char c;
	int x, y;
	Symbol(char _c, int _x, int _y): c(_c), x(_x), y(_y) {}
};

enum class State {
	Down,
	UpRight,
};

class Solution {
public:
	string convert(string const &s, int nRows) {
		vector<Symbol> symbols;
		zigtag(s, symbols, nRows);
		sort(symbols.begin(), symbols.end(), [](Symbol const &a, Symbol const &b){
			if (a.y == b.y) return a.x < b.x;
			return a.y < b.y; 
		});

		string result;
		for (auto &symbol : symbols) result.push_back(symbol.c);
		return result;
	}
private:
	void zigtag(string const &s, vector<Symbol> &symbols, int rows) {
		int x = 0, y = -1;
		State state = State::Down;
		for (auto c : s) {
			if (state == State::Down && y + 1 == rows) state = State::UpRight;
			if (state == State::Down) symbols.push_back(Symbol(c, x, ++y));
			if (state == State::UpRight) symbols.push_back(Symbol(c, ++x, y = max(0, y - 1)));
			if (state == State::UpRight && y - 1 < 0) state = State::Down;
		}
	}
};

//--------------------------------------------------

int main() {
	Solution s;
	cout << s.convert("PAYPALISHIRING", 3) << endl;
	cout << s.convert("ABCD", 2) << endl;
	cout << s.convert("AB", 1) << endl;
	cout << s.convert("ABCD", 1) << endl;
	cout << s.convert("Apalindromeisaword,phrase,number,orothersequenceofunitsthatcanbereadthesamewayineitherdirection,withgeneralallowancesforadjustmentstopunctuationandworddividers.", 1) << endl;
}
