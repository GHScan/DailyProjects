#include "stdafx.h" 

struct ListNode {
    int val;
    ListNode *next;
    ListNode(int x) : val(x), next(NULL) {}
};

struct TreeNode {
    int val;
    TreeNode *left;
    TreeNode *right;
    TreeNode(int x) : val(x), left(NULL), right(NULL) {}
    TreeNode(int x, TreeNode *_left, TreeNode *_right) : val(x), left(_left), right(_right) {}
};
//-----------------------------------------------------------------
class Solution {
public:
    TreeNode *sortedListToBST(ListNode *head) {
        int count = 0;
        for (ListNode *n = head; n != nullptr; n = n->next, ++count);
        return build(head, count).first;
    }
private:
    pair<TreeNode*, ListNode*> build(ListNode *node, int count) {
        if (count == 0) return make_pair(nullptr, node);
        auto leftPair = build(node, count / 2);
        auto rightPair = build(leftPair.second->next, count - (count / 2) - 1);
        auto tree = new TreeNode(leftPair.second->val);
        tree->left = leftPair.first;
        tree->right = rightPair.first;
        return make_pair(tree, rightPair.second);
    }
};
//-----------------------------------------------------------------
static ListNode* createList(vector<int> const &init) {
    ListNode *head = nullptr, **p = &head;

    for (auto i : init) {
        auto n = new ListNode(i);
        *p = n;
        p = &n->next;
    }

    return head;
}

static void printTree(TreeNode *node) {
    if (node == nullptr) return;
    printTree(node->left);
    cout << node->val << ',';
    printTree(node->right);
}

int main() {
    Solution so;
    vector<int> nums[] = {
        vector<int>{},
        vector<int>{1},
        vector<int>{1, 1, 1},
        vector<int>{1, 2, 3,},
        vector<int>{1, 3, 5, 6, },
        vector<int>{1, 3, 6, 7, 7, 8},
    };
    for (auto &num : nums) {
        printTree(so.sortedListToBST(createList(num)));
        cout << endl;
    }
}
