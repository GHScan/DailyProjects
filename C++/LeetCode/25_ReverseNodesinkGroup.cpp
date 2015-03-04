#include "stdafx.h"

#include <assert.h>
#include <string>

struct ListNode {
	int val;
	ListNode *next;
	ListNode(int x) : val(x), next(NULL) {}
};

//--------------------------------------------------
class Solution {
public:
	ListNode *reverseKGroup(ListNode *head, int k) {
		ListNode **p = &head;
		for (;;) {
			ListNode *n = *p;
			for (int i = 0; i < k; ++i) {
				if (n == nullptr) return head;
				n = n->next;
			}
			ListNode *m = *p;
			*p = reverseN(*p, n, k);
			p = &m->next;
		}
	}
private:
	ListNode* reverseN(ListNode *head, ListNode *prev, int n) {
		if (n == 0) return prev;
		else {
			ListNode *next = head->next;
			head->next = prev;
			return reverseN(next, head, n - 1);
		}
	}
};
//--------------------------------------------------
template<int N>
static ListNode* createList(int (&a)[N]) {
	ListNode *head, **p = &head;
	for (auto i : a) {
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
	printList(s.reverseKGroup(nullptr, 2));
	{
		int a[] = {1,};
		printList(s.reverseKGroup(createList(a), 2));
	}
	{
		int a[] = {1, 2, };
		printList(s.reverseKGroup(createList(a), 2));
	}
	{
		int a[] = {1, 2, 3};
		printList(s.reverseKGroup(createList(a), 2));
	}
	{
		int a[] = {1, 2, 3, 4};
		printList(s.reverseKGroup(createList(a), 2));
	}
	{
		int a[] = {1, 2, 3, 4, 5};
		printList(s.reverseKGroup(createList(a), 2));
	}
	{
		int a[] = {1, 2, 3, 4, 5};
		printList(s.reverseKGroup(createList(a), 3));
	}
}
