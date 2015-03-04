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
    ListNode *removeNthFromEnd(ListNode *head, int n) {
		return remove(head, n);
    }
private:
	ListNode *remove(ListNode *head, int &n) {
		if (head != nullptr) {
			head->next = remove(head->next, n);
			if (--n == 0) head = head->next;
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
	{
		int a[] = {1, 2, 3, 4, 5, };
		for (int i = 0; i < 7; ++i) {
			printList(s.removeNthFromEnd(createList(a), i));
		}
	}
	{
		int a[] = {1,  };
		for (int i = 0; i < 3; ++i) {
			printList(s.removeNthFromEnd(createList(a), i));
		}
	}
	printList(s.removeNthFromEnd(nullptr, 0));
	printList(s.removeNthFromEnd(nullptr, 1));
}
