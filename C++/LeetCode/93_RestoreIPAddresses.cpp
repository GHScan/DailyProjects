#include "stdafx.h" 

//-----------------------------------------------------------------
class Solution {
public:
    vector<string> restoreIpAddresses(string const &s) {
        vector<string> result;
        if (s.empty()) return result;

        string path;
        generate(result, path, 0, s.c_str(), s.c_str() + s.size());
        return result;
    }
private:
    void generate(vector<string> &result, string &path, int depth, const char *begin, const char *end) {
        if (begin >= end || depth >= 4) {
            if (begin == end && depth == 4) result.push_back(string(path.begin() + 1, path.end()));
            return;
        }

        path.push_back('.');
        for (int len = 1; len <= 3 && begin + len <= end; ++len) {
            if (isValid(begin, begin + len)) {
                path.insert(path.end(), begin, begin + len);
                generate(result, path, depth + 1, begin + len, end);
                path.erase(path.end() - len, path.end());
            }
        }
        path.pop_back();
    }
    bool isValid(const char *begin, const char *end) {
        switch (end - begin) {
        case 1: return begin[0] >= '0' && begin[0] <= '9';
        case 2: return (begin[0] >= '1' && begin[0] <= '9') && (begin[1] >= '0' && begin[1] <= '9');
        case 3: 
            if (begin[0] == '1') {
                return (begin[1] >= '0' && begin[1] <= '9') && (begin[2] >= '0' && begin[2] <= '9');
            } else if (begin[0] == '2') {
                if (begin[1] == '5') {
                    return begin[2] >= '0' && begin[2] <= '5';
                } else if (begin[1] >= '0' && begin[1] <= '4') {
                    return begin[2] >= '0' && begin[2] <= '9';
                } else return false;
            } else return false; 
        default:
            return false;
        }
    }
};
//-----------------------------------------------------------------
static void printResult(vector<string> const &result) {
    cout << "#####################" << endl;
    for (auto &s : result) {
        cout << s << endl;
    }
}

int main() {
    Solution so;
    printResult(so.restoreIpAddresses(""));
    printResult(so.restoreIpAddresses("25525511135"));
    printResult(so.restoreIpAddresses("111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"));
}
