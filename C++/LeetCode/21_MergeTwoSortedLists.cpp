//
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
	ListNode *mergeTwoLists(ListNode *l1, ListNode *l2) {
		ListNode *head, **p = &head;

		while (l1 != nullptr && l2 != nullptr) {
			ListNode *n;
			if (l1->val < l2->val) {
				n = l1;
				l1 = l1->next;
			} else {
				n = l2;
				l2 = l2->next;
			}
			*p = n;
			p = &n->next;
		}

		if (l1 != nullptr) *p = l1;
		else if (l2 != nullptr) *p = l2;
		else *p = nullptr;

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
	printList(s.mergeTwoLists(nullptr, nullptr));
	{
		int a[] = {1, 2, 3};
		printList(s.mergeTwoLists(createList(a), nullptr));
	}
	{
		int a[] = {1, 2, 3};
		printList(s.mergeTwoLists(nullptr, createList(a)));
	}
	{
		int a[] = {1, 2, 3};
		printList(s.mergeTwoLists(nullptr, createList(a)));
	}
	{
		int a[] = {1, 2, 3};
		int b[] = {4, 5, 6};
		printList(s.mergeTwoLists(createList(a), createList(b)));
	}
	{
		int a[] = {4, 5, 6};
		int b[] = {1, 2, 3};
		printList(s.mergeTwoLists(createList(a), createList(b)));
	}
	{
		int a[] = {1, 3, 4};
		int b[] = {2, 5};
		printList(s.mergeTwoLists(createList(a), createList(b)));
	}
	{
		int a[] = {1, 3, 4};
		int b[] = {3};
		printList(s.mergeTwoLists(createList(a), createList(b)));
	}
}
