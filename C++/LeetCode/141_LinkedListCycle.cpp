struct ListNode 
{
    int val;
    ListNode *next;
    ListNode(int x) : val(x), next(nullptr) {}
};

//--------------------------

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

class Solution {
public:
    bool hasCycle(ListNode *head) {
        ListNode *p = head;
        ListNode *p2 = head;

        if (head == nullptr)
            return false;

        for (;;)
        {
            if (p2->next == nullptr) return false;
            p2 = p2->next;
            if (p2->next == nullptr) return false;
            p2 = p2->next;

            p = p->next;

            if (p2 == p)
                return true;
        }

        return false;
    }
};

int main()
{
}