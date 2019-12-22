#include <string>
#include <vector>
#include <queue>
#include <iterator>
#include <iostream>
#include <algorithm>

using namespace std;

class Solution {
public:
    vector<vector<string>> partition(string s) {
        vector<char const *> par;
        vector<vector<string>> result;
        partition(s.c_str(), (int)s.size(), par, result);
        return result;
    }

    void partition(char const *s, int len, vector<char const *> &par, vector<vector<string>> &result)
    {
        par.push_back(s);

        if (len == 0)
        {
            vector<string> tmp;
            for (int i = 0; i < (int)par.size() - 1; ++i)
                tmp.emplace_back(par[i], par[i + 1]);
            result.push_back(move(tmp));
        }
        else
        {
            for (int i = 1; i <= len; ++i)
            {
                if (isPalindrome(s, i))
                {
                    partition(s + i, len - i, par, result);
                }
            }
        }

        par.pop_back();
    }

    bool isPalindrome(char const *s, int len)
    {
        int half = len / 2;
        for (int i = 0; i < half; ++i)
        {
            if (s[i] != s[len - 1 - i])
                return false;
        }
        return true;
    }
};

int main()
{
    for (auto test : vector<string>
        {
            "", "a", "aab", "aabbaba"
        })
    {
        auto result = Solution().partition(test);
        cout << "------------\n";
        for (auto &par : result)
        {
            for (auto &s : par)
                cout << s << ",";
            cout << "\n";
        }
    }
}