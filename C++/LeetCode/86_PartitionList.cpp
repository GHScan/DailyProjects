#include "stdafx.h"

#include <assert.h>

struct ListNode {
    int val;
    ListNode *next;
    ListNode(int x) : val(x), next(NULL) {}
};
//-------------------------------------------------------------------------
class Solution {
public:
    ListNode *partition(ListNode *head, int x) {
        ListNode *nodes[2] = {nullptr};
        while (head != nullptr) {
            ListNode *temp = head;
            head = head->next;
            auto p = nodes + (temp->val < x ? 0 : 1);
            temp->next = *p;
            *p = temp;
        }
        for (int i = 1; i >= 0; --i) {
            while (nodes[i] != nullptr) {
                ListNode *temp = nodes[i];
                nodes[i] = nodes[i]->next;
                temp->next = head;
                head = temp;
            }
        }
        return head;
    }
};
//-------------------------------------------------------------------------
static ListNode* createList(initializer_list<int> init) {
    ListNode *head, **p = &head;

    for (auto i : init) {
        auto n = new ListNode(i);
        *p = n;
        p = &n->next;
    }

    return head;
}

static void printList(ListNode *l) {
    while (l != nullptr) {
        cout << l->val << ',';
        l = l->next;
    }
    cout << endl;
}

int main() {
    Solution so;
    printList(so.partition(nullptr, 1));
    printList(so.partition(createList({0}), 1));
    printList(so.partition(createList({1, 0}), 1));
    printList(so.partition(createList({0, 4, 1, 0, 1, 2, 3}), 1));
    printList(so.partition(createList({ 1, 4, 3, 2, 5, 2 }), 3));
}
