#include "stdafx.h"

//------------------------------
#include <unordered_map>

struct WordInfo {
    int id;
    int count;
};

class Solution {
public:
    vector<int> findSubstring(string const &s, vector<string> const &L) {
        vector<int> result;
        if (L.empty() || L.size() * L.front().size() > s.size()) return result;

        int wordCount = (int)L.size();
        int wordSize = (int)L.front().size();
        int seqSize = wordCount * wordSize;

        unordered_map<string, WordInfo> h;
        for (int i = 0; i < wordCount; ++i) {
            auto it = h.find(L[i]);
            if (it == h.end()) it = h.insert(make_pair(L[i], WordInfo{(int)h.size(), 0})).first;
            it->second.count += 1;
        }

        vector<int> occurs(s.size() - wordSize + 1, -1);
        {
            string word;
            for (auto it = s.begin(), end = s.end() - wordSize + 1; it != end; ++it) {
                word.assign(it, it + wordSize);
                auto wordIt = h.find(word);
                if (wordIt != h.end()) occurs[it - s.begin()] = wordIt->second.id;
            }
        }

        vector<int> wordsCount(h.size(), 0);
        for (auto &p : h) wordsCount[p.second.id] = p.second.count;

        vector<char> tempWordsCount(wordsCount.size(), 0);
        for (int i = 0, lasti = (int)s.size() - seqSize; i <= lasti;) {
            tempWordsCount.assign(tempWordsCount.size(), 0);

            int count = 0;
            for (int j = i; count < wordCount && occurs[j] != -1 && tempWordsCount[occurs[j]] < wordsCount[occurs[j]]; j += wordSize, ++count) {
                ++tempWordsCount[occurs[j]];
            }

            if (count == wordCount) {
                result.push_back(i);
            }

            for (++i; i <= lasti && occurs[i] == -1; ++i);
        }

        return result;
    }
};

//------------------------------
static void printResult(vector<int> const &result) {
    for (auto i : result) cout << i << ',';
    cout << endl;
}

int main() {
    Solution s;
    printResult(s.findSubstring("", {}));
    printResult(s.findSubstring("", { "foo", "bar" }));
    printResult(s.findSubstring("barfoothefoobarman", {}));
    printResult(s.findSubstring("barfoothefoobarman", { "foo", "bar" }));
    printResult(s.findSubstring("barfoo", { "foo", "bar" }));
    printResult(s.findSubstring("barfoofoobar", { "foo", "bar" }));
    printResult(s.findSubstring("barafoofoobbar", { "foo", "bar" }));
    printResult(s.findSubstring("barafoobarbfoo", { "foo", "bar" }));
    printResult(s.findSubstring("ab", { "a", "b", "c" }));
    printResult(s.findSubstring("cab", { "a", "b", "c" }));
    printResult(s.findSubstring("abbca", { "a", "b", "c" }));
    printResult(s.findSubstring("cabbca", { "a", "b", "c" }));
    printResult(s.findSubstring("lingmindraboofooowingdingbarrwingmonkeypoundcake", { "fooo", "barr", "wing", "ding", "wing" }));
    printResult(s.findSubstring("aaa", { "a", "a", }));
}
