struct ListNode 
{
    int val;
    ListNode *next;
    ListNode(int x, ListNode *next_) : val(x), next(next_) {}
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
    void reorderList(ListNode *head) {
        if (head == nullptr)
            return;

        ListNode *head2;
        breakList(head, head2);
        mergeList(head, head2);
    }

    void breakList(ListNode *head, ListNode *&head2)
    {
        int len = 0;
        for (auto p = head; p != nullptr; p = p->next)
            ++len;

        int half = (len + 1) / 2 - 1;
        for (int i = 0; i < half; ++i)
            head = head->next;

        {
            ListNode *t = head;
            head = head->next;
            t->next = nullptr;
        }

        head2 = nullptr;
        while (head != nullptr)
        {
            ListNode *t = head;
            head = head->next;
            t->next = head2;
            head2 = t;
        }
    }

    void mergeList(ListNode *n1, ListNode *n2)
    {
        while (n2 != nullptr)
        {
            ListNode *t1 = n1;
            ListNode *t2 = n2;
            n1 = n1->next;
            n2 = n2->next;
            t1->next = t2;
            t2->next = n1;
        }
        if (n1 != nullptr)
            n1->next = nullptr;
    }
};

void print(ListNode *head)
{
    for (; head != nullptr; head = head->next)
        cout << head->val << "->";
    cout << "\n";
}

static ListNode* creatList(int len)
{
    ListNode *head = nullptr;
    ListNode **p = &head;
    for (int i = 1; i <= len; ++i)
    {
        *p = new ListNode(i, nullptr);
        p = &(*p)->next;
    }
    return head;
}

int main()
{
    for (int len = 0; len <= 5; ++len)
    {
        ListNode *head = creatList(len);
        Solution().reorderList(head);
        print(head);
    }
}