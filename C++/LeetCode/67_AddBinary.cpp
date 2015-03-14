#include "stdafx.h"

//------------------------------
class Solution {
public:
    string addBinary(string a, string b) {
        if (a.size() < b.size()) return addBinary(b, a);

        string result;

        int carry = 0;
        auto ita = a.rbegin();
        for (auto itb = b.rbegin(); itb != b.rend(); ++itb, ++ita) {
            carry += (*ita - '0') + (*itb- '0');
            result += carry % 2 + '0';
            carry /= 2;
        }
        for (; ita != a.rend(); ++ita) {
            carry += *ita - '0';
            result += carry % 2 + '0';
            carry /= 2;
        }
        if (carry > 0) result.push_back('1');
        return string(result.rbegin(), result.rend());
    }
};
//------------------------------
int main() {
    Solution s;
    cout << s.addBinary("0", "0") << endl;
    cout << s.addBinary("0", "1") << endl;
    cout << s.addBinary("1", "0") << endl;
    cout << s.addBinary("1", "1") << endl;
    cout << s.addBinary("11", "1") << endl;
    cout << s.addBinary("1", "11011") << endl;
    cout << s.addBinary("1", "11111") << endl;
    cout << s.addBinary("11", "11111") << endl;
    cout << s.addBinary("1010", "1011") << endl;
}
