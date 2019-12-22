
#include <cstdio>

#include <string>
#include <vector>
#include <stack>
#include <list>
#include <unordered_map>
#include <iostream>
#include <algorithm>

using namespace std;

class Solution {
public:
    string reverseWords(string s) {
        string result;

        int n = (int)s.size();
        int ei = n;
        for (;;)
        {
            for (; ei - 1 >= 0 && isspace(s[ei - 1]); --ei);
            if (ei == 0) break;

            int i = ei - 1;
            for (; i - 1 >= 0 && !isspace(s[i - 1]); --i);

            if (!result.empty())
                result.push_back(' ');
            result.insert(result.end(), s.begin() + i, s.begin() + ei);

            ei = i;
        }

        return result;
    }
};

int main()
{
    for (auto &test : vector<string>
        {
            "",
            "the sky is blue",
            "  hello world!  ",
            "a good   example",
        })
    {
        cout << Solution().reverseWords(test) << "\n";
    }
}