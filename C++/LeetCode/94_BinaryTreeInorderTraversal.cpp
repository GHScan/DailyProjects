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
    vector<int> inorderTraversal(TreeNode *root) {
        vector<int> result;
        if (root == nullptr) return result;

        vector<TreeNode*> stack(1, root);
        while (stack.back()->left != nullptr) stack.push_back(stack.back()->left);
        while (!stack.empty()) {
            TreeNode *n = stack.back();
            stack.pop_back();

            result.push_back(n->val);
            if (n->right != nullptr) {
                stack.push_back(n->right);
                while (stack.back()->left != nullptr) stack.push_back(stack.back()->left);
            }
        }

        return result;
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

static void printResult(vector<int> const &result) {
    for (auto i : result) cout << i << ',';
    cout << endl;
}

int main() {
    Solution so;
    printResult(so.inorderTraversal(nullptr));
    printResult(so.inorderTraversal(buildTree({1})));
    printResult(so.inorderTraversal(buildTree({ 1, 2 })));
    printResult(so.inorderTraversal(buildTree({ 3, 1, 2 })));
    printResult(so.inorderTraversal(buildTree({ 3, 1, 4, 2 })));
    printResult(so.inorderTraversal(buildTree({ 5, 3, 1, 4, 2 })));
    printResult(so.inorderTraversal(buildTree({ 5, 3, 1, 4, 6, 6, 2, 6 })));

}
