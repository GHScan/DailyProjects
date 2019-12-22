#include <cassert>

#include <string>
#include <vector>
#include <queue>
#include <numeric>
#include <unordered_map>
#include <iterator>
#include <iostream>
#include <algorithm>

using namespace std;

#define CharSet     26

class ACAutomaton
{
public:
    ACAutomaton(vector<string> const &words_)
        : words(words_)
    {
        root = new Node(nullptr);

        buildSuccLink(words);
        buildFailLink();
        buildWordLink();
    }

    ~ACAutomaton()
    {
        root->destroy();
    }

    void buildSuccLink(vector<string> const &words)
    {
        for (int i = 0; i < (int)words.size(); ++i)
        {
            Node *p = root;
            for (char c : words[i])
            {
                int code = c - 'a';
                assert(code >= 0 && code < CharSet);
                if (p->succLink[code] == nullptr)
                    p->succLink[code] = new Node(p);
                p = p->succLink[code];
            }
            p->word = i;
        }
    }

    void buildFailLink()
    {
        queue<pair<Node *, int>> pendings;

        for (Node *n : root->succLink)
        {
            if (n == nullptr) continue;

            n->failLink = root;
            for (int i = 0; i < CharSet; ++i)
            {
                if (Node *succ = n->succLink[i])
                {
                    pendings.push(make_pair(succ, i));
                }
            }
        }

        while (!pendings.empty())
        {
            Node *n = pendings.front().first;
            int code = pendings.front().second;

            pendings.pop();

            Node *fail = n->parent->failLink;
            while (fail->succLink[code] == nullptr && fail->failLink != nullptr)
            {
                fail = fail->failLink;
            }
            if (fail->succLink[code] != nullptr)
            {
                n->failLink = fail->succLink[code];
                if (n->failLink->failLink == nullptr)
                {
                    pendings.push(make_pair(n->failLink, code));
                }
            }
            else
            {
                n->failLink = root;
            }

            for (int i = 0; i < CharSet; ++i)
            {
                Node *succ = n->succLink[i];
                if (succ != nullptr && succ->failLink == nullptr)
                    pendings.push(make_pair(succ, i));
            }
        }
    }

    void buildWordLink()
    {
        queue<Node*> pendings;

        for (Node *n : root->succLink)
        {
            if (n == nullptr) continue;
            pendings.push(n);
        }

        while (!pendings.empty())
        {
            Node *n = pendings.front();
            pendings.pop();

            Node *p = n->failLink;
            while (p->word < 0 && p->failLink != nullptr)
                p = p->failLink;

            n->wordLink = p;

            for (Node *succ : n->succLink)
            {
                if (succ == nullptr) continue;
                pendings.push(succ);
            }
        }
    }

    void search(string const &input, vector<pair<int, int>> &token)
    {
        Node *n = root;

        int i = 0;
        while (i < (int)input.size())
        {
            int code = input[i] - 'a';
            assert(code >= 0 && code < CharSet);

            if (Node *succ = n->succLink[code])
            {
                ++i;
                n = succ;
                
                for (Node *p = n; p != root; p = p->wordLink)
                {
                    if (p->word >= 0)
                        token.push_back(make_pair(i - (int)words[p->word].size(), p->word));
                }
            }
            else if (n != root)
                n = n->failLink;
            else
                ++i;
        }
    }

private:
    struct Node
    {
        int word;
        Node *parent;
        Node *wordLink;
        Node *failLink;
        Node *succLink[CharSet];

        Node(Node *parent_)
            : word(-1)
            , parent(parent_)
            , wordLink(nullptr)
            , failLink(nullptr)
        {
            memset(succLink, 0, sizeof(succLink));
        }

        void destroy()
        {
            for (Node *succ : succLink)
            {
                if (succ != nullptr)
                    succ->destroy();
            }

            delete this;
        }
    };

    Node *root;
    vector<string> words;
};


class Solution {
public:
    bool wordBreak(string s, vector<string> &wordDict) {
        vector<string> result;

        ACAutomaton ac(wordDict);

        unordered_multimap<int, int> map;
        {
            vector<pair<int, int>> tokens;
            ac.search(s, tokens);

            map.reserve(tokens.size());
            for (auto token : tokens)
                map.insert(token);
        }

        if (!isCoverable((int)s.size(), map, wordDict))
            return false;

        return enumerateWordBreak((int)s.size(), 0, map, wordDict);
    }

    bool isCoverable(int len, unordered_multimap<int, int> const &map, vector<string> const &wordDict)
    {
        int maxCover = 0;
        int i = 0;
        while (i < len)
        {
            int maxSize = 0;
            auto range = map.equal_range(i);
            auto begin = range.first;
            auto end = range.second;
            int count = 0;
            for (auto it = begin; it != end; ++it, ++count)
                maxSize = max(maxSize, (int)wordDict[it->second].size());

            int step = 1;
            if (i == maxCover && count == 1)
                step = (int)wordDict[begin->second].size();

            if (maxSize > 0)
                maxCover = max(maxCover, i + maxSize);

            if (i >= maxCover)
                return false;

            i += step;
        }
        return true;
    }

    bool enumerateWordBreak(
        int len, int i,
        unordered_multimap<int, int> const &tokens,
        vector<string> const &wordDict)
    {
        if (i == len)
        {
            return true;
        }

        auto range = tokens.equal_range(i);
        auto begin = range.first;
        auto end = range.second;
        for (auto it = begin; it != end; ++it)
        {
            if (enumerateWordBreak(
                len, i + (int)wordDict[it->second].size(), tokens, wordDict))
            {
                return true;
            }
        }

        return false;
    }
};

int main()
{
    for (auto test :
        vector<pair<string, vector<string>>>
        {
            make_pair("abcd", vector<string>{"a","abc","b","cd"}),
            make_pair("catsandog", vector<string>{"cats", "dog", "sand", "and", "cat"}),
            make_pair("catsanddog", vector<string>{"cat", "cats", "and", "sand", "dog"}),
            make_pair("catsanddog", vector<string>{"cat", "cats", "and", "sand", "dog"}),
            make_pair("pineapplepenapple", vector<string>{"apple", "pen", "applepen", "pine", "pineapple"}),
            make_pair(
                "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                vector<string>{"a","aa","aaa","aaaa","aaaaa","aaaaaa","aaaaaaa","aaaaaaaa","aaaaaaaaa","aaaaaaaaaa"}),
            make_pair(
                "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabaabaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                vector<string>{"aa","aaa","aaaa","aaaaa","aaaaaa","aaaaaaa","aaaaaaaa","aaaaaaaaa","aaaaaaaaaa","ba"}),
        })
    {
        cout << Solution().wordBreak(test.first, test.second) << "\n";
    }
}