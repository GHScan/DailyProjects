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
    ListNode *detectCycle(ListNode *head) {

        int cycleSize = 0;
        {
            ListNode *p = head;
            ListNode *p2 = head;

            if (head == nullptr)
                return nullptr;

            for (;;)
            {
                if (p2->next == nullptr) return nullptr;
                p2 = p2->next;
                if (p2->next == nullptr) return nullptr;
                p2 = p2->next;

                p = p->next;

                if (p2 == p)
                    break;
            }

            for (;;)
            {
                p2 = p2->next->next;
                p = p->next;

                ++cycleSize;

                if (p2 == p)
                    break;
            }
        }

        {
            ListNode *p = head;
            ListNode *p2 = head;

            for (int i = 0; i < cycleSize; ++i)
                p2 = p2->next;

            for (; p != p2;)
            {
                p2 = p2->next;
                p = p->next;
            }

            return p;
        }
    }
};

int main()
{
}