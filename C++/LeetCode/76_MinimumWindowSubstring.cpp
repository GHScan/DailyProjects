#include "stdafx.h"

//------------------------------
#include <stdint.h>
#include <limits>

class Solution {
public:
    string minWindow(string const &S, string const &T) {
        int tCharCounts[256] = {0};
        int tTotalCharCount = 0;
        for (auto c : T) ++tCharCounts[(uint8_t)c];
        for (auto count : tCharCounts) tTotalCharCount += count;
        if (tTotalCharCount == 0) return "";

        int minStart = -1;
        int minLen = numeric_limits<int>::max();

        int suffixStart = 0;
        int suffixCharCounts[256] = {0};
        int suffixValidTotalCharCount = 0;
        for (int i = 0; i < (int)S.size(); ++i) {
            int charIdx = (uint8_t)S[i];
            ++suffixCharCounts[charIdx];
            suffixValidTotalCharCount += tCharCounts[charIdx] > 0 && suffixCharCounts[charIdx] <= tCharCounts[charIdx] ? 1 : 0;
            while (suffixValidTotalCharCount == tTotalCharCount && suffixStart <= i) {
                if (i - suffixStart + 1 < minLen) {
                    minLen = i - suffixStart + 1;
                    minStart = suffixStart;
                }

                int charIdx = (uint8_t)S[suffixStart];
                ++suffixStart;
                suffixValidTotalCharCount -= tCharCounts[charIdx] > 0 && suffixCharCounts[charIdx] <= tCharCounts[charIdx] ? 1 : 0;
                --suffixCharCounts[charIdx];
            }
        }

        if (minStart == -1) return "";
        else return string(S.begin() + minStart, S.begin() + minStart + minLen);
    }
};
//------------------------------
int main() {
    Solution s;
    cout << s.minWindow("ABC", "") << endl;
    cout << s.minWindow("A", "AA") << endl;
    cout << s.minWindow("AA", "AA") << endl;
    cout << s.minWindow("AAB", "AA") << endl;
    cout << s.minWindow("ABA", "AA") << endl;
    cout << s.minWindow("ABBA", "AA") << endl;
    cout << s.minWindow("BBBAA", "ABC") << endl;
    cout << s.minWindow("BBBAAC", "ABC") << endl;
    cout << s.minWindow("BBBAACBB", "ABC") << endl;
    cout << s.minWindow("ADOBECODEBANC", "ABC") << endl;
    cout << s.minWindow("ask_not_what_your_country_can_do_for_you_ask_what_you_can_do_for_your_country", "ask_country") << endl;
    cout << s.minWindow("BBBAACCCCBA", "ABC") << endl;
    cout << s.minWindow("BBBAACCCCBA", "ABCA") << endl;
}
