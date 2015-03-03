#include "pch.h" 

struct ListNode {
    int val;
    ListNode *next;
    ListNode(int x) : val(x), next(NULL) {}
};

//-----------------------------------------------------------------

class Solution {
    public:
        ListNode *addTwoNumbers(ListNode *l1, ListNode *l2) {
            if (l1 == nullptr) return l2;
            if (l2 == nullptr) return l1;

            ListNode *head, **p = &head;
            int carry = 0;

            while (l1 != nullptr || l2 != nullptr || carry != 0) {

                if (l1 != nullptr) {
                    carry += l1->val;
                    l1 = l1->next;
                }
                if (l2 != nullptr) { 
                    carry += l2->val;
                    l2 = l2->next;
                }

                auto n = new ListNode(carry % 10);
                *p = n;
                p = &n->next;
                carry = carry / 10;
            }

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
    Solution s;
    printList(s.addTwoNumbers(createList({2, 4, 3}), createList({5, 6, 4})));
    printList(s.addTwoNumbers(createList({8, }), createList({5, 9, 4})));
    printList(s.addTwoNumbers(createList({8, }), createList({2})));
}
