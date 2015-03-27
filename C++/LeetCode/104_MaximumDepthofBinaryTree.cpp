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
    int maxDepth(TreeNode *root) {
        if (root == nullptr) return 0;
        return max(maxDepth(root->left), maxDepth(root->right)) + 1;
    }
};
//-----------------------------------------------------------------

int main() {
    Solution so;
    cout << so.maxDepth(nullptr) << endl;
    cout << so.maxDepth(new TreeNode(1)) << endl;
    cout << so.maxDepth(new TreeNode(1, new TreeNode(2, new TreeNode(3), nullptr), nullptr)) << endl;
    cout << so.maxDepth(new TreeNode(1, new TreeNode(2, new TreeNode(3), nullptr), new TreeNode(2))) << endl;
    cout << so.maxDepth(new TreeNode(3, new TreeNode(9), new TreeNode(20, new TreeNode(15), new TreeNode(7)))) << endl;
    cout << so.maxDepth(new TreeNode(3, new TreeNode(9), new TreeNode(20, new TreeNode(15, new TreeNode(1, new TreeNode(2), nullptr), nullptr), new TreeNode(7)))) << endl;
}
