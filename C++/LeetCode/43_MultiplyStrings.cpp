#include "stdafx.h"

//------------------------------
#include <algorithm>

class Solution {
public:
    string multiply(string const &s1, string const &s2) {
        if (s1.empty() || s2.empty()) return "";
        if (s1.size() < s2.size()) return multiply(s2, s1);

        vector<int> num1(s1.size(), 0), num2(s2.size(), 0);
        transform(s1.begin(), s1.end(), num1.rbegin(), [](char c){ return c - '0'; });
        transform(s2.begin(), s2.end(), num2.rbegin(), [](char c){ return c - '0'; });

        vector<int> numr(num1.size() + num2.size(), 0);
        vector<int> temp(num1.size() + 1, 0);
        for (int i = 0; i < (int)num2.size(); ++i) {
            mulSingle(&temp[0], &num1[0], &num1[0] + num1.size(), num2[i]);
            addInplace(&numr[i], &temp[0], &temp[0] + temp.size());
        }
        while (numr.size() > 1 && numr.back() == 0) numr.pop_back();

        string result(numr.size(), 0);
        transform(numr.begin(), numr.end(), result.rbegin(), [](int i){ return i + '0'; });
        return result;
    }
private:
    void addInplace(int *out, const int *begin, const int *end) {
        int carry = 0;
        for (; begin != end; ++begin, ++out) {
            carry += *begin + *out;
            *out = carry % 10;
            carry /= 10;
        }
        for (; carry > 0; ++out) {
            carry += *out;
            *out = carry % 10;
            carry /= 10;
        }
    }
    void mulSingle(int *out, const int *begin, const int *end, int n) {
        int carry = 0;
        for (; begin != end; ++begin, ++out) {
            carry += n * *begin;
            *out = carry % 10;
            carry /= 10;
        }
        *out = carry;
    }
};

//------------------------------
#define countOf(a) int(sizeof(a) / sizeof(a[0]))

static string pow(Solution so, string const &a, int n) {
    if (n == 0) return "1";
    else if (n % 2 == 0) {
        string r = pow(so, a, n / 2);
        return so.multiply(r, r);
    } else {
        return so.multiply(a, pow(so, a, n - 1));
    }
}

int main() {
    Solution s;
    cout << s.multiply("", "") << endl;
    cout << s.multiply("", "0") << endl;
    cout << s.multiply("0", "") << endl;
    cout << s.multiply("0", "0") << endl;
    cout << s.multiply("0", "123") << endl;
    cout << s.multiply("123", "0") << endl;
    cout << s.multiply("12345", "54321") << endl;
    cout << s.multiply("1234567", "7654321") << endl;
    cout << s.multiply("123456789123456789123456789", "987654321987654321987654321") << endl;

    cout << pow(s, "2", 32) << endl;
    cout << pow(s, "2", 100) << endl;
    cout << pow(s, "3", 100) << endl;
    cout << pow(s, "5", 100) << endl;
    cout << pow(s, "7", 100) << endl;
}
