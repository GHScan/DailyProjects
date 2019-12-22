 struct ListNode {
     int val;
     ListNode *next;
     ListNode(int x) : val(x), next(nullptr) {}
 }; 

//---------------------

#include <cstdio>

#include <string>
#include <vector>
#include <stack>
#include <list>
#include <unordered_map>
#include <iostream>
#include <algorithm>

using namespace std;

class Solution {
public:
    ListNode *getIntersectionNode(ListNode *headA, ListNode *headB) {

        int lenA = 0;
        for (ListNode *p = headA; p != nullptr; p = p->next)
            ++lenA;
        int lenB = 0;
        for (ListNode *p = headB; p != nullptr; p = p->next)
            ++lenB;

        if (lenA < lenB)
        {
            swap(lenA, lenB);
            swap(headA, headB);
        }

        for (int i = lenA - lenB; i > 0; --i)
            headA = headA->next;

        while (headA != nullptr && headA != headB)
        {
            headA = headA->next;
            headB = headB->next;
        }

        return headA;
    }
};

int main()
{
    
}