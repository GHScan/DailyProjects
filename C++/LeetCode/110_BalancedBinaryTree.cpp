#include "stdafx.h" 

struct TreeNode {
    int val;
    TreeNode *left;
    TreeNode *right;
    TreeNode(int x) : val(x), left(NULL), right(NULL) {}
    TreeNode(int x, TreeNode *_left, TreeNode *_right) : val(x), left(_left), right(_right) {}
};
//-----------------------------------------------------------------
#include <stdlib.h>
#include <algorithm>

class Solution {
public:
    bool isBalanced(TreeNode *root) {
        bool balanced = true;
        depth(root, balanced);
        return balanced;
    }
private:
    int depth(TreeNode *root, bool &balanced) {
        if (root == nullptr) return 0;
        int leftDepth = depth(root->left, balanced);
        if (!balanced) return 0;
        int rightDepth = depth(root->right, balanced);
        if (!balanced) return 0;
        if (abs(leftDepth - rightDepth) > 1) {
            balanced = false;
            return 0;
        }
        return max(leftDepth, rightDepth) + 1;
    }
};
//-----------------------------------------------------------------

int main() {
    Solution so;
    cout << so.isBalanced(nullptr) << endl;
    cout << so.isBalanced(new TreeNode(1)) << endl;
    cout << so.isBalanced(new TreeNode(1, new TreeNode(2, new TreeNode(3), nullptr), nullptr)) << endl;
    cout << so.isBalanced(new TreeNode(1, new TreeNode(2, new TreeNode(3), nullptr), new TreeNode(2))) << endl;
    cout << so.isBalanced(new TreeNode(3, new TreeNode(9), new TreeNode(20, new TreeNode(15), new TreeNode(7)))) << endl;
    cout << so.isBalanced(new TreeNode(3, new TreeNode(9), new TreeNode(20, new TreeNode(15, new TreeNode(1, new TreeNode(2), nullptr), nullptr), new TreeNode(7)))) << endl;
}
