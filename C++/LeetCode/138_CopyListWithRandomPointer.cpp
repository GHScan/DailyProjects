class Node {
public:
    int val;
    Node *next;
    Node *random;

    Node() {}

    Node(int _val, Node *_next, Node *_random) {
        val = _val;
        next = _next;
        random = _random;
    }
};

//----------------------------

#include <string>
#include <vector>
#include <queue>
#include <numeric>
#include <unordered_map>
#include <iterator>
#include <iostream>
#include <algorithm>

using namespace std;

class Solution {
public:
    Node *copyRandomList(Node *head) {
        unordered_map<Node *, Node *> map;
        queue<Node *> pendings;

        if (head == nullptr)
            return head;

        map[head] = new Node(head->val, nullptr, nullptr);
        pendings.push(head);

        while (!pendings.empty())
        {
            Node *n = pendings.front();
            pendings.pop();

            Node *newN = map[n];

            if (n->next != nullptr)
            {
                auto it = map.find(n->next);
                if (it == map.end())
                {
                    it = map.insert(make_pair(n->next, new Node(n->next->val, nullptr, nullptr))).first;
                    pendings.push(n->next);
                }
                newN->next = it->second;
            }
            if (n->random != nullptr)
            {
                auto it = map.find(n->random);
                if (it == map.end())
                {
                    it = map.insert(make_pair(n->random, new Node(n->random->val, nullptr, nullptr))).first;
                    pendings.push(n->random);
                }
                newN->random = it->second;
            }
        }

        return map[head];
    }
};

int main()
{

}