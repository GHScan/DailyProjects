#include "stdafx.h" 

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
    void flatten(TreeNode *root) {
        flatten(root, nullptr);
    }
private:
    TreeNode* flatten(TreeNode *root, TreeNode *tail) {
        if (root == nullptr) return tail;
        TreeNode *left = root->left, *right = root->right;
        root->right = flatten(left, flatten(right, tail));
        root->left = nullptr;
        return root;
    }
};
//-----------------------------------------------------------------
static void printResult(TreeNode *root) {
    for (; root != nullptr; root = root->right) {
        cout << root->val << ',';
    }
    cout << endl;
}

int main() {
    Solution so;

    TreeNode *trees[] = {
        nullptr,
        new TreeNode(1),
        new TreeNode(1, new TreeNode(2), nullptr),
        new TreeNode(1, nullptr, new TreeNode(2)),
        new TreeNode(5,
        new TreeNode(4,
        new TreeNode(11, new TreeNode(7), new TreeNode(2)),
        nullptr),
        new TreeNode(8,
        new TreeNode(13),
        new TreeNode(4, new TreeNode(5), new TreeNode(1)))),
        new TreeNode(1,
            new TreeNode(2, new TreeNode(3), new TreeNode(4)),
            new TreeNode(5, nullptr, new TreeNode(6))),
    };
    for (auto tree : trees) {
        so.flatten(tree);
        printResult(tree);
    }
}
