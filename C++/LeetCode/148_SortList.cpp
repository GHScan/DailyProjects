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
    ListNode *sortList(ListNode *head) {
        sort(head, nullptr);
        return head;
    }

    ListNode *partition(ListNode *begin, ListNode *end)
    {
        int val = begin->val;
        auto *p = begin;
        for (auto *i = begin->next; i != end; i = i->next)
        {
            if (i->val <= val)
            {
                p = p->next;
                swap(i->val, p->val);
            }
        }
        swap(begin->val, p->val);
        return p;
    }

    void sort(ListNode *begin, ListNode *end) {
        if (begin == end || begin->next == end)
            return;
        auto p = partition(begin, end);
        sort(begin, p);
        sort(p->next, end);
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
    printList(Solution().sortList(nullptr));
    printList(Solution().sortList(new ListNode(1)));
    printList(Solution().sortList(new ListNode(2, new ListNode(1))));
    printList(Solution().sortList(new ListNode(1, new ListNode(3, new ListNode(2)))));
    printList(Solution().sortList(new ListNode(2, new ListNode(4, new ListNode(1, new ListNode(3))))));
    printList(Solution().sortList(new ListNode(2, new ListNode(4, new ListNode(5, new ListNode(3, new ListNode(1)))))));
}