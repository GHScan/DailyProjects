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
    bool hasPathSum(TreeNode *root, int sum) {
        if (root == nullptr) return false;
        sum -= root->val;
        if (root->left != nullptr && root->right != nullptr) return hasPathSum(root->left, sum) || hasPathSum(root->right, sum);
        else if (root->left != nullptr) return hasPathSum(root->left, sum);
        else if (root->right != nullptr) return hasPathSum(root->right, sum);
        else return sum == 0;
    }
};
//-----------------------------------------------------------------

int main() {
    Solution so;
    cout << so.hasPathSum(nullptr, 0) << endl;
    cout << so.hasPathSum(new TreeNode(1), 0) << endl;
    cout << so.hasPathSum(new TreeNode(1), 1) << endl;
    cout << so.hasPathSum(new TreeNode(1), 2) << endl;
    cout << so.hasPathSum(new TreeNode(1, new TreeNode(2), nullptr), 1) << endl;
    cout << so.hasPathSum(new TreeNode(1, new TreeNode(2), nullptr), 2) << endl;
    cout << so.hasPathSum(new TreeNode(1, new TreeNode(2), nullptr), 3) << endl;

    auto tree = new TreeNode(5, 
        new TreeNode(4,
            new TreeNode(11, new TreeNode(7), new TreeNode(2)),
            nullptr),
        new TreeNode(8,
            new TreeNode(13),
            new TreeNode(4, nullptr, new TreeNode(1))));
    cout << so.hasPathSum(tree, 22) << endl;
    cout << so.hasPathSum(tree, 26) << endl;
    cout << so.hasPathSum(tree, 18) << endl;
    cout << so.hasPathSum(tree, 17) << endl;
}
