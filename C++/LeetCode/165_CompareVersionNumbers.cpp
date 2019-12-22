
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
    int compareVersion(string version1, string version2) {
        return compare(version1.c_str(), version2.c_str());
    }

    int compare(char const *v1, char const *v2)
    {
        if (!*v1 && !*v2) return 0;

        int n1 = 0;
        if (*v1)
        {
            char *e1;
            n1 = strtol(v1, &e1, 10);
            v1 = e1;
        }

        int n2 = 0;
        if (*v2)
        {
            char *e2;
            n2 = strtol(v2, &e2, 10);
            v2 = e2;
        }

        if (n1 > n2)
            return 1;
        if (n2 > n1)
            return -1;

        for (; *v1 && !isdigit(*v1); ++v1);
        for (; *v2 && !isdigit(*v2); ++v2);

        return compare(v1, v2);
    }
};

int main()
{
    for (auto &test : vector<pair<string, string>>
        {
            {"", ""},
            {"1", ""},
            {"", "1"},
            {"0.1", "1.1"},
            {"1.0.1", "1"},
            {"7.5.2.4", "7.5.3"},
            {"1.01", "1.001"},
            {"1.0", "1.0.0"},
        })
    {
        cout << Solution().compareVersion(test.first, test.second) << "\n";
    }
}