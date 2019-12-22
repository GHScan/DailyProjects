
#include <cstdio>

#include <string>
#include <vector>
#include <stack>
#include <list>
#include <unordered_map>
#include <iostream>
#include <iterator>
#include <algorithm>

using namespace std;

class Solution {
public:
    string convertToTitle(int n) {
        string result;
        do
        {
            int a = n / 26;
            int b = n % 26;
            if (b == 0 && a > 0)
            {
                --a;
                b += 26;
            }
            result.push_back(b - 1 + 'A');
            n = a;
        } while (n > 0);
        return string(result.rbegin(), result.rend());
    }
};

int main()
{
    for (auto &test : vector<int>
        {
            1,
            2,
            26, 
            27,
            28,
            701,
        })
    {
        cout << Solution().convertToTitle(test) << "\n";
    }
}