#include "stdafx.h" 

struct TreeNode {
    int val;
    TreeNode *left;
    TreeNode *right;
    TreeNode(int x) : val(x), left(NULL), right(NULL) {}
    TreeNode(int x, TreeNode *_left, TreeNode *_right) : val(x), left(_left), right(_right) {}
};
//-----------------------------------------------------------------
#include <limits>

class Solution {
public:
    bool isValidBST(TreeNode *root) {
        return isValid(root, -numeric_limits<double>::max(), numeric_limits<double>::max());
    }
private:
    bool isValid(TreeNode *root, double minVal, double maxVal) {
        return root == nullptr || 
            ((root->val > minVal && root->val < maxVal)
            && isValid(root->left, minVal, root->val)
            && isValid(root->right, root->val, maxVal));
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
    cout << so.isValidBST(nullptr) << endl;
    cout << so.isValidBST(buildTree({ 3 })) << endl;
    cout << so.isValidBST(buildTree({ numeric_limits<int>::min() })) << endl;
    cout << so.isValidBST(buildTree({ numeric_limits<int>::max() })) << endl;
    cout << so.isValidBST(buildTree({ numeric_limits<int>::min(), numeric_limits<int>::max() })) << endl;
    cout << so.isValidBST(buildTree({1, 2, 3})) << endl;
    cout << so.isValidBST(buildTree({ 1, 2, 3 , 4})) << endl;
    cout << so.isValidBST(buildTree({ 5, 1, 2, 3, 4 })) << endl;
    cout << so.isValidBST(buildTree({ 5, 1, 2, 6, 6, 3, 4 })) << endl;
    cout << so.isValidBST(buildTree({ 5, 1, 2, 6, 3, 4 })) << endl;
    cout << so.isValidBST(new TreeNode(3, new TreeNode(1, new TreeNode(-1), nullptr), new TreeNode(13))) << endl;
    cout << so.isValidBST(new TreeNode(3, new TreeNode(5, new TreeNode(-1), nullptr), new TreeNode(13))) << endl;
    cout << so.isValidBST(new TreeNode(3, new TreeNode(1, new TreeNode(-1), nullptr), new TreeNode(0))) << endl;
}
