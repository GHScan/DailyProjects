#include "stdafx.h" 

struct TreeNode {
    int val;
    TreeNode *left;
    TreeNode *right;
    TreeNode(int x) : val(x), left(NULL), right(NULL) {}
    TreeNode(int x, TreeNode *_left, TreeNode *_right) : val(x), left(_left), right(_right) {}
};
//-----------------------------------------------------------------
#include <algorithm>

class Solution {
public:
    bool isSymmetric(TreeNode *root) {
        return root == nullptr || isSame(root->left, mirror(root->right));
    }
private:
    bool isSame(TreeNode *n1, TreeNode *n2) {
        if (n1 == nullptr || n2 == nullptr) return n1 == n2;
        return n1->val == n2->val && isSame(n1->left, n2->left) && isSame(n1->right, n2->right);
    }
    TreeNode *mirror(TreeNode *n) {
        if (n == nullptr) return n;
        n->left = mirror(n->left);
        n->right = mirror(n->right);
        swap(n->left, n->right);
        return n;
    }
};
//-----------------------------------------------------------------
static void printTree(TreeNode *node) {
    if (node == nullptr) return;
    printTree(node->left);
    cout << node->val << ',';
    printTree(node->right);
}

int main() {
    Solution so;
    TreeNode *trees[] = {
        nullptr,
        new TreeNode(3, new TreeNode(1), new TreeNode(4)),
        new TreeNode(3, new TreeNode(3), new TreeNode(3)),
        new TreeNode(3, new TreeNode(4), new TreeNode(1)),
        new TreeNode(0, new TreeNode(1), nullptr), 
        new TreeNode(1, new TreeNode(2, new TreeNode(3), new TreeNode(4)), new TreeNode(2, new TreeNode(3), new TreeNode(4))),
        new TreeNode(1, new TreeNode(2, new TreeNode(3), new TreeNode(4)), new TreeNode(2, new TreeNode(4), new TreeNode(3))),
        new TreeNode(1, new TreeNode(2, nullptr, new TreeNode(3)), new TreeNode(2, nullptr, new TreeNode(3))),
        new TreeNode(1, new TreeNode(2, nullptr, new TreeNode(3)), new TreeNode(2, new TreeNode(3), nullptr)),
    };
    for (auto tree : trees) {
        cout << so.isSymmetric(tree) << endl;
    }
}
