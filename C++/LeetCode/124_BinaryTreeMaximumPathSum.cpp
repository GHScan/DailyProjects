#include "stdafx.h"

struct TreeNode {
    int val;
    TreeNode *left;
    TreeNode *right;
    TreeNode(int x, TreeNode *_left = nullptr, TreeNode *_right = nullptr) : val(x), left(_left), right(_right) {}
};
//-------------------------------------------------------------------------
#include <limits>

class Solution {
public:
    int maxPathSum(TreeNode* root) {
        int partialSum, sum;
        getSum(root, partialSum, sum);
        return sum;
    }
private:
    void getSum(TreeNode* root, int &partialSum, int &sum) {
        if (root == nullptr) {
            partialSum = 0;
            sum = std::numeric_limits<int>::min();
            return;
        }

        int leftPartialSum, leftSum, rightPartialSum, rightSum;
        getSum(root->left, leftPartialSum, leftSum);
        getSum(root->right, rightPartialSum, rightSum);

        partialSum = max(max(leftPartialSum, rightPartialSum) + root->val, 0);
        sum = max(max(leftSum, rightSum), root->val + leftPartialSum + rightPartialSum);
    }
};
//-------------------------------------------------------------------------

int main() {
    Solution so;

    cout << so.maxPathSum(nullptr) << endl;
    cout << so.maxPathSum(new TreeNode(-1)) << endl;
    cout << so.maxPathSum(new TreeNode(1, new TreeNode(-1))) << endl;
    cout << so.maxPathSum(new TreeNode(2, new TreeNode(-1, new TreeNode(4)))) << endl;
    cout << so.maxPathSum(new TreeNode(1, new TreeNode(2), new TreeNode(3))) << endl;
}
