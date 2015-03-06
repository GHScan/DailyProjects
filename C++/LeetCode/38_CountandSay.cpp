#include "stdafx.h"

//------------------------------

class Solution {
public:
    string countAndSay(int n) {
        string strs[2] = {"1"};
        int input = 0;
        while (--n > 0) {
            int output = 1 - input;
            derive(strs[output], strs[input]);
            input = output;
        }
        return strs[input];
    }
private:
    void derive(string &output, string const &input) {
        output.clear();

        char numBuffer[20];
        for (int i = 0; i < (int)input.size(); ) {
            int c = input[i];

            int count = 1;
            for (; i + count < (int)input.size() && input[i + count] == c; ++count);
            i += count;

            sprintf(numBuffer, "%d%c", count, c);
            output += numBuffer;
        }
    }
};

//------------------------------

int main() {
    Solution s;
    for (int i = 1; i < 10; ++i) {
        cout << s.countAndSay(i) << endl;
    }
}
