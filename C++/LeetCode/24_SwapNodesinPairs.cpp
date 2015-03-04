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
	ListNode *swapPairs(ListNode *head) {
		ListNode **p = &head;

		while (*p != nullptr && (*p)->next != nullptr) {
			ListNode *curr = *p;
			ListNode *next = curr->next;
			*p = next;
			curr->next = next->next;
			next->next = curr;
			p = &curr->next;
		}

		return head;
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
	printList(s.swapPairs(nullptr));
	{
		int a[] = {1,};
		printList(s.swapPairs(createList(a)));
	}
	{
		int a[] = {1, 2, };
		printList(s.swapPairs(createList(a)));
	}
	{
		int a[] = {1, 2, 3};
		printList(s.swapPairs(createList(a)));
	}
	{
		int a[] = {1, 2, 3, 4};
		printList(s.swapPairs(createList(a)));
	}
	{
		int a[] = {1, 2, 3, 4, 5};
		printList(s.swapPairs(createList(a)));
	}
}
