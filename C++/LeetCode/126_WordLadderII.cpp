#include <string>
#include <vector>
#include <queue>
#include <iterator>
#include <iostream>
#include <algorithm>

using namespace std;

class Solution {
public:
    vector<vector<string>> findLadders(string beginWord, string endWord, vector<string> &wordList) {

        wordList.push_back(beginWord);

        vector<vector<int>> adjacencies;
        vector<int> distances;
        vector<vector<string>> pathes;
        int src = (int)(find(wordList.begin(), wordList.end(), beginWord) - wordList.begin());
        int dst = (int)(find(wordList.begin(), wordList.end(), endWord) - wordList.begin());
        if (dst == wordList.size())
            return pathes;

        buildAdjacencies(adjacencies, wordList);
        calculateDistances(distances, src, dst, adjacencies);
        {
            vector<string> path;
            buildPath(dst, path, pathes, distances, adjacencies, wordList);
        }

        return pathes;
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

    void buildPath(
        int i,
        vector<string> &path,
        vector<vector<string>> &pathes,
        vector<int> const &distances,
        vector<vector<int>> const &adjacencies,
        vector<string> const &wordList)
    {
        path.push_back(wordList[i]);

        if (distances[i] == 0)
        {
            vector<string> revPath(path.rbegin(), path.rend());
            pathes.push_back(revPath);
        }
        else
        {
            for (int j : adjacencies[i])
            {
                if (distances[j] + 1 == distances[i])
                {
                    buildPath(j, path, pathes, distances, adjacencies, wordList);
                }
            }
        }

        path.pop_back();
    }
};

int main()
{
    {
        vector<string> words {"hot", "dot", "dog", "lot", "log", "cog"};
        vector<vector<string>> ladders = Solution().findLadders("hit", "cog", words);
        for (vector<string> const &ladder : ladders)
        {
            copy(ladder.cbegin(), ladder.cend(), ostream_iterator<string>(cout, ","));
            cout << "\n";
        }
    }
    {
        vector<string> words{ "hot","dot","dog","lot","log" };
        vector<vector<string>> ladders = Solution().findLadders("hit", "cog", words);
        for (vector<string> const &ladder : ladders)
        {
            copy(ladder.cbegin(), ladder.cend(), ostream_iterator<string>(cout, ","));
            cout << "\n";
        }
    }
}