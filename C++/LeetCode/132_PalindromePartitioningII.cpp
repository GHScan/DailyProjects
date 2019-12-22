#include <string>
#include <vector>
#include <queue>
#include <iterator>
#include <iostream>
#include <algorithm>

using namespace std;

class Solution {
public:
    int minCut(string s) {
        if (s.empty()) return 0;

        vector<bool> bitmap;
        buildBitmap(s, bitmap);

        vector<int> minCuts(s.size() + 1, -1);
        for (int len = 1; len <= (int)s.size(); ++len)
        {
            int minCut = (1 << 30);
            for (int lastLen = 1; lastLen <= len; ++lastLen)
            {
                if (isPalindrome(s, bitmap, len - lastLen, lastLen))
                {
                    minCut = min(minCut, minCuts[len - lastLen] + 1);
                }
            }
            minCuts[len] = minCut;
        }

        return minCuts[s.size()];
    }

    void buildBitmap(string const &s, vector<bool> &bitmap)
    {
        bitmap.assign(s.size() * (s.size() + 1), false);
        for (int center = 0; center < (int)s.size(); ++center)
        {
            {
                int maxHalf = min(center, (int)s.size() - center - 1);
                for (int half = 0; half <= maxHalf; ++half)
                {
                    if (s[center - half] != s[center + half])
                        break;
                    bitmap[(center - half) * s.size() + 2 * half + 1] = true;
                }
            }
            {
                int maxHalf = min(center, (int)s.size() - center - 2);
                for (int half = 0; half <= maxHalf; ++half)
                {
                    if (s[center - half] != s[center + 1 + half])
                        break;
                    bitmap[(center - half) * s.size() + 2 * half + 2] = true;
                }
            }
        }
    }

    bool isPalindrome(string const &s, vector<bool> &bitmap, int off, int len)
    {
        return bitmap[off * s.size() + len];
    }
};

int main()
{
    for (auto test : vector<string>
        {
            "", "a", "aab", "aabbaba", 
            "ababababababababababababcbabababababababababababa",
            "apjesgpsxoeiokmqmfgvjslcjukbqxpsobyhjpbgdfruqdkeiszrlmtwgfxyfostpqczidfljwfbbrflkgdvtytbgqalguewnhvvmcgxboycffopmtmhtfizxkmeftcucxpobxmelmjtuzigsxnncxpaibgpuijwhankxbplpyejxmrrjgeoevqozwdtgospohznkoyzocjlracchjqnggbfeebmuvbicbvmpuleywrpzwsihivnrwtxcukwplgtobhgxukwrdlszfaiqxwjvrgxnsveedxseeyeykarqnjrtlaliyudpacctzizcftjlunlgnfwcqqxcqikocqffsjyurzwysfjmswvhbrmshjuzsgpwyubtfbnwajuvrfhlccvfwhxfqthkcwhatktymgxostjlztwdxritygbrbibdgkezvzajizxasjnrcjwzdfvdnwwqeyumkamhzoqhnqjfzwzbixclcxqrtniznemxeahfozp",
        })
    {
        cout << Solution().minCut(test) << "\n";
    }
}