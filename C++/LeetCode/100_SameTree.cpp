#include "stdafx.h" 

struct TreeNode {
    int val;
    TreeNode *left;
    TreeNode *right;
    TreeNode(int x) : val(x), left(NULL), right(NULL) {}
};
//-----------------------------------------------------------------
class Solution {
public:
    bool isSameTree(TreeNode *p, TreeNode *q) {
        if (p == nullptr || q == nullptr) return p == q;
        else return p->val == q->val && isSameTree(p->left, q->left) && isSameTree(p->right, q->right);
    }
};
//-----------------------------------------------------------------
static TreeNode* buildTree(initializer_list<int> ints) {
    TreeNode *root = nullptr;
    for (auto i : ints) {
        TreeNode **p = &root;
        while (*p != nullptr) {
            if (i <= (*p)->val) p = &(*p)->left;
            else p = &(*p)->right;
        }
        *p = new TreeNode(i);
    }
    return root;
}

int main() {
    Solution so;
    cout << so.isSameTree(nullptr, nullptr) << endl;
    cout << so.isSameTree(nullptr, buildTree({ 1, 3, 2 })) << endl;
    cout << so.isSameTree(buildTree({ 1, 2, 3 }), nullptr) << endl;
    cout << so.isSameTree(buildTree({ 2, 1, 3 }), buildTree({ 2, 3, 1 })) << endl;
    cout << so.isSameTree(buildTree({1, 2, 3}), buildTree({1, 3, 2})) << endl;
    cout << so.isSameTree(buildTree({ 1, 2, 3 }), buildTree({ 2, 1, 3})) << endl;
    cout << so.isSameTree(buildTree({ 1, 2, 3 }), buildTree({ 3, 2, 1})) << endl;
}
