#include <string>
#include <vector>
#include <queue>
#include <iterator>
#include <iostream>
#include <algorithm>

using namespace std;

class Node {
public:
    int val;
    vector<Node*> neighbors;

    Node() {}

    Node(int _val, vector<Node *> _neighbors) {
        val = _val;
        neighbors = _neighbors;
    }
};

//---------------------------------

#include <string>
#include <vector>
#include <queue>
#include <unordered_map>
#include <iterator>
#include <iostream>
#include <algorithm>

using namespace std;

class Solution {
public:
    Node *cloneGraph(Node *node) {
        unordered_map<Node *, Node *> map;
        return clone(node, map);
    }

    Node *clone(Node *node, unordered_map<Node*, Node*> &map)
    {
        {
            auto it = map.find(node);
            if (it != map.end())
                return it->second;
        }

        Node *newNode = new Node(node->val, vector<Node *>());
        map[node] = newNode;

        for (Node *neighbor : node->neighbors)
            newNode->neighbors.push_back(clone(neighbor, map));

        return newNode;
    }
};

int main()
{
    
}