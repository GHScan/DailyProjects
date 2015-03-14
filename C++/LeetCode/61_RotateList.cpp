#include "stdafx.h"

struct ListNode {
    int val;
    ListNode *next;
    ListNode(int x) : val(x), next(NULL) {}
};
//------------------------------
class Solution {
public:
    ListNode *rotateRight(ListNode *head, int k) {
        int len = length(head);
        if (len == 0) return nullptr;
        k %= len;

        ListNode **tail = &head;
        for (int skip = len - k; skip > 0; --skip, tail = &(*tail)->next);

        ListNode *newHead = *tail;
        *tail = nullptr;
        return append(newHead, head);
    }
private:
    int length(ListNode *head) {
        int len = 0;
        for (; head != nullptr; ++len, head = head->next);
        return len;
    }
    ListNode* append(ListNode *first, ListNode *second) {
        ListNode **p = &first;
        for (; *p != nullptr; p = &(*p)->next);
        *p = second;
        return first;
    }
};
//------------------------------
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
    Solution s;
    puts("##################");
    for (int i = 0; i < 2; ++i) printList(s.rotateRight(nullptr, i));
    puts("##################");
    for (int i = 0; i < 3; ++i) printList(s.rotateRight(createList({1}), i));
    puts("##################");
    for (int i = 0; i < 4; ++i) printList(s.rotateRight(createList({ 1, 2, }), i));
    puts("##################");
    for (int i = 0; i < 5; ++i) printList(s.rotateRight(createList({ 1, 2, 3}), i));
    puts("##################");
    for (int i = 0; i < 6; ++i) printList(s.rotateRight(createList({ 1, 2, 3, 4 }), i));
}
