 struct ListNode {
     int val;
     ListNode *next;
     ListNode(int x, ListNode *next_ = nullptr) : val(x), next(next_) {}
 };
 

//------------------

#include <cstdio>

#include <string>
#include <vector>
#include <list>
#include <unordered_map>
#include <iostream>

using namespace std;

class Solution {
public:
    ListNode *insertionSortList(ListNode *head) {
        ListNode *output = nullptr;
        while (head != nullptr)
        {
            ListNode *t = head;
            head = head->next;

            ListNode **pp = &output;
            while (*pp != nullptr && t->val > (*pp)->val)
                pp = &(*pp)->next;
            ListNode *tt = *pp;
            *pp = t;
            t->next = tt;
        }
        return output;
    }
};

static void printList(ListNode *head)
{
    for (; head != nullptr; head = head->next)
        cout << head->val << ",";
    cout << "\n";
}

int main()
{
    printList(Solution().insertionSortList(nullptr));
    printList(Solution().insertionSortList(new ListNode(1)));
    printList(Solution().insertionSortList(new ListNode(2, new ListNode(1))));
    printList(Solution().insertionSortList(new ListNode(1, new ListNode(3, new ListNode(2)))));
    printList(Solution().insertionSortList(new ListNode(2, new ListNode(4, new ListNode(1, new ListNode(3))))));
}