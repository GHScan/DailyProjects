#include "pch.h" 


//-----------------------------------------------------------------

class Solution {
    public:
        int lengthOfLongestSubstring(string s) {
            int lastCurrs[128];
            for (auto &i : lastCurrs) i = -1;

            int maxLen = 0;
            int lastLen = 0;
            for (int i = 0; i < (int)s.size(); ++i) {
                int c = s[i];
                auto distance = i - lastCurrs[c];
                lastCurrs[c] = i;
                lastLen = min(lastLen + 1, distance);
                maxLen = max(maxLen, lastLen);
            }

            return maxLen;
        }
};

//-----------------------------------------------------------------

int main() {
    Solution s;
    cout << s.lengthOfLongestSubstring("abcabcbb") << endl;
    cout << s.lengthOfLongestSubstring("bbbb") << endl;
    cout << s.lengthOfLongestSubstring("aab") << endl;
}
