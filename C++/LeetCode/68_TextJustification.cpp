#include "stdafx.h"

//------------------------------
class Solution {
public:
    vector<string> fullJustify(vector<string> &words, int L) {
        vector<string> result;
        if (words.empty()) return result;

        auto begin = &words[0];
        auto end = begin + words.size();
        do {
            result.push_back(peekLine(begin, end, L));
            assert(begin == end || result.back().size() == L);
        } while(begin != end);
        return result;
    }
private:
    string peekLine(string *&begin, string *end, int L) {
        assert(begin != end);

        string *it = begin;
        int minLen = (int)(*it++).size();
        int strsLen = minLen;
        for (; it != end && (int)it->size() + 1 + minLen <= L; ++it) {
            minLen += 1 + (int)it->size();
            strsLen += (int)it->size();
        }
        if (it == end) {
            string result(*begin++);
            for (; begin != it; ++begin) {
                result += ' ';
                result += *begin;
            }
            return result + string(L - result.size(), ' ');
        }
        if (begin + 1 == it) {
            string result(*begin++);
            return result + string(L - result.size(), ' ');
        }

        string padding((L - minLen) / (it - begin - 1) + 1, ' ');
        int moreChar = (L - minLen) % (it - begin - 1);
        string result(*begin++);
        for (; begin != it; ++begin) {
            if (moreChar-- > 0) result += ' ';
            result += padding;
            result += *begin;
        }
        return result;
    }
};
//------------------------------
static void printResult(vector<string> const &result) {
    cout << "#############" << endl;
    for (auto &s : result) cout << s << '|' << endl;
}

int main() {
    Solution s;
    printResult(s.fullJustify(vector<string>{}, 1));
    printResult(s.fullJustify(vector<string>{"a"}, 1));
    printResult(s.fullJustify(vector<string>{"a"}, 2));
    printResult(s.fullJustify(vector<string>{"a"}, 3));
    printResult(s.fullJustify(vector<string>{"a"}, 4));
    printResult(s.fullJustify(vector<string>{"ab"}, 4));
    printResult(s.fullJustify(vector<string>{"ab"}, 5));
    printResult(s.fullJustify(vector<string>{"ab"}, 6));
    printResult(s.fullJustify(vector<string>{"a", "abcdefg"}, 7));
    printResult(s.fullJustify(vector<string>{"a", "b", "abcdefg"}, 7));
    printResult(s.fullJustify(vector<string>{"a", "b", "c", "abcdefg"}, 7));
    printResult(s.fullJustify(vector<string>{"a", "b", "c", "abcdefg"}, 8));
    printResult(s.fullJustify(vector<string>{"a", "b", "c", "abcdefg"}, 9));
    printResult(s.fullJustify(vector<string>{"a", "b", "c", "abcdefg"}, 10));
    printResult(s.fullJustify(vector<string>{"a", "b", "c", "abcde", "abcdefg"}, 10));
    printResult(s.fullJustify(vector<string>{ "This", "is", "an", "example", "of", "text", "justification."}, 16));
    printResult(s.fullJustify(vector<string>{"Listen", "to", "many,", "speak", "to", "a", "few."}, 6));
}
