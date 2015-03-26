#include "stdafx.h" 

struct ListNode {
    int val;
    ListNode *next;
    ListNode(int x) : val(x), next(NULL) {}
};

//-----------------------------------------------------------------
class Solution {
public:
    ListNode *reverseBetween(ListNode *head, int m, int n) {
        if (head == nullptr) return head;

        ListNode **p = &head;
        for (int i = 0; i < m - 1; ++i) {
            p = &(*p)->next;
        }

        ListNode *tail = *p;
        for (int i = m; i < n + 1; ++i) {
            tail = tail->next;
        }

        ListNode *node = *p;
        for (int i = m; i < n + 1; ++i) {
            ListNode *temp = node->next;
            node->next = tail;
            tail = node;
            node = temp;
        }

        *p = tail;
        return head;
    }
};
//-----------------------------------------------------------------
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
    printList(so.reverseBetween(nullptr, 0, 0));
    printList(so.reverseBetween(createList({1}), 1, 1));
    printList(so.reverseBetween(createList({ 1, 2 }), 1, 1));
    printList(so.reverseBetween(createList({ 1, 2 }), 1, 2));
    printList(so.reverseBetween(createList({ 1, 2, 3 }), 2, 3));
    printList(so.reverseBetween(createList({ 1, 2, 3, 4 }), 2, 3));
    printList(so.reverseBetween(createList({ 1, 2, 3, 4 }), 2, 4));
    printList(so.reverseBetween(createList({ 1, 2, 3, 4 }), 1, 3));
    printList(so.reverseBetween(createList({ 1, 2, 3, 4 }), 1, 4));
}
