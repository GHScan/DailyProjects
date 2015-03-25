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
    ListNode *deleteDuplicates(ListNode *head) {
        ListNode **p = &head;
        for (ListNode *n = head; n != nullptr; ) {
            *p = n;
            p = &n->next;
            int v = n->val;
            for (; n != nullptr && n->val == v; n = n->next);
        }
        *p = nullptr;
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

    printList(so.deleteDuplicates(nullptr));
    printList(so.deleteDuplicates(createList({1})));
    printList(so.deleteDuplicates(createList({ 1, 1, 2 })));
    printList(so.deleteDuplicates(createList({1, 2, 3})));
    printList(so.deleteDuplicates(createList({ 1, 1, 2, 3 })));
    printList(so.deleteDuplicates(createList({ 1, 1, 2, 3, 3 })));
    printList(so.deleteDuplicates(createList({ 1, 2, 2, 3 })));
    printList(so.deleteDuplicates(createList({ 1, 2, 3, 3 })));
    printList(so.deleteDuplicates(createList({ 1, 1, 2, 2, 3, 3 })));
    printList(so.deleteDuplicates(createList({ 1, 2, 3, 3, 4, 4, 5 })));
    printList(so.deleteDuplicates(createList({ 1, 1, 1, 2, 3, })));
}
