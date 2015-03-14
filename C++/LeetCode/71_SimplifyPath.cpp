#include "stdafx.h"

//------------------------------
class Solution {
public:
    string simplifyPath(string const &path) {
        mNames.clear();

        for (int i = 0; i < (int)path.size();) {
            int j = i;
            for (; j < (int)path.size() && path[j] != '/'; ++j);

            if (j - i == 1 && path[i] == '.');
            else if (j - i == 2 && path[i] == '.' && path[i + 1] == '.') {
                if (!mNames.empty()) mNames.pop_back();
            } else if (j - i > 0) {
                mNames.push_back(make_pair(i, j));
            }

            i = j + 1;
        }
        if (mNames.empty()) return "/";
        
        string result;
        for (auto &name : mNames) {
            result += '/';
            result.insert(result.end(), path.begin() + name.first, path.begin() + name.second);
        }
        return result;
    }
private:
    vector<pair<int, int>> mNames;
};
//------------------------------

int main() {
    Solution s;
    cout << s.simplifyPath("/home/..") << endl;
    cout << s.simplifyPath("/home/") << endl;
    cout << s.simplifyPath("/a/./b/../../c/") << endl;
    cout << s.simplifyPath("/..") << endl;
    cout << s.simplifyPath("/") << endl;
}
