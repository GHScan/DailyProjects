#include <string>
#include <vector>
#include <queue>
#include <iterator>
#include <iostream>
#include <algorithm>

using namespace std;

class Solution {
public:
    int ladderLength(string beginWord, string endWord, vector<string> &wordList) {

        vector<vector<int>> adjacencies;
        vector<int> distances;
        int src = (int)(find(wordList.begin(), wordList.end(), beginWord) - wordList.begin());
        if (src == wordList.size())
            wordList.push_back(beginWord);
        int dst = (int)(find(wordList.begin(), wordList.end(), endWord) - wordList.begin());
        if (dst == wordList.size())
            return 0;

        buildAdjacencies(adjacencies, wordList);
        calculateDistances(distances, src, dst, adjacencies);

        if (distances[dst] > wordList.size())
            return 0;

        return distances[dst] + 1;
    }

    bool isTransformable(string const &a, string const &b)
    {
        if (a.size() != b.size()) return false;
        int len = 0;
        for (int i = 0; i < (int)a.size(); ++i)
            len += a[i] == b[i];
        return len == a.size() - 1;
    }

    void buildAdjacencies(
        vector<vector<int>> &adjacencies,
        vector<string> const &wordList)
    {
        int count = (int)wordList.size();
        adjacencies.resize(count);
        for (int i = 0; i < count; ++i)
        {
            for (int j = 0; j < i; ++j)
            {
                if (isTransformable(wordList[i], wordList[j]))
                {
                    adjacencies[i].push_back(j);
                    adjacencies[j].push_back(i);
                }
            }
        }
    }

    void calculateDistances(
        vector<int> &distances,
        int src, int dst,
        vector<vector<int>> const &adjacencies)
    {
        std::queue<int> pendings;

        distances.assign(adjacencies.size(), 1 << 30);
        distances[src] = 0;
        pendings.push(src);

        while (!pendings.empty())
        {
            int i = pendings.front();
            pendings.pop();

            if (i == dst)
                continue;

            int nextDis = distances[i] + 1;
            for (int j : adjacencies[i])
            {
                if (distances[j] > nextDis)
                {
                    distances[j] = nextDis;
                    pendings.push(j);
                }
            }
        }
    }
};

int main()
{
    {
        vector<string> words {"hot", "dot", "dog", "lot", "log", "cog"};
        cout << Solution().ladderLength("hit", "cog", words) << "\n";
    }
    {
        vector<string> words{ "hot","dot","dog","lot","log" };
        cout << Solution().ladderLength("hit", "cog", words) << "\n";
    }
    {
        vector<string> words{ "hot","dog", };
        cout << Solution().ladderLength("hot", "dog", words) << "\n";
    }
}