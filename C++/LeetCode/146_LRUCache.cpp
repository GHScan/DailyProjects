
#include <cstdio>

#include <string>
#include <vector>
#include <list>
#include <unordered_map>
#include <iostream>

using namespace std;

class LRUCache {
public:
    LRUCache(int capacity_)
        : capacity(capacity_)
        , size(0)
    {
    }

    int get(int key) {
        auto it = map.find(key);
        if (it == map.end())
            return -1;
        recentList.splice(recentList.begin(), recentList, it->second);
        return it->second->second;
    }

    void put(int key, int value) {
        auto it = map.find(key);
        if (it != map.end())
        {
            recentList.splice(recentList.begin(), recentList, it->second);
            it->second->second = value;
        }
        else
        {
            if (size == capacity)
            {
                map.erase(recentList.back().first);
                recentList.pop_back();
            }
            else
            {
                ++size;
            }
            map[key] = recentList.insert(recentList.begin(), make_pair(key, value));
        }
    }

private:
    list<pair<int, int>> recentList;
    std::unordered_map<int, list<pair<int, int>>::iterator> map;
    int size;
    int capacity;
};

int main()
{
    LRUCache cache(2 /* capacity */);

    cache.put(1, 1);
    cache.put(2, 2);
    cout << cache.get(1) << "\n";       // returns 1
    cache.put(3, 3);                    // evicts key 2
    cout << cache.get(2) << "\n";       // returns -1 (not found)
    cache.put(4, 4);                    // evicts key 1
    cout << cache.get(1) << "\n";       // returns -1 (not found)
    cout << cache.get(3) << "\n";       // returns 3
    cout << cache.get(4) << "\n";       // returns 4
}