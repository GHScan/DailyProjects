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
    TreeNode *buildTree(vector<int> const &preorder, vector<int> const &inorder) {
        if (preorder.empty()) return nullptr;
        return build(&preorder[0], &preorder[0] + preorder.size(), &inorder[0], &inorder[0] + inorder.size());
    }
private:
    TreeNode* build(const int *preBegin, const int *preEnd, const int *inBegin, const int *inEnd) {
        if (preBegin == preEnd) return nullptr;

        const int *inMid = find(inBegin, inEnd, *preBegin);
        int leftCount = inMid - inBegin;

        auto node = new TreeNode(*preBegin);
        node->left = build(preBegin + 1, preBegin + 1 + leftCount, inBegin, inMid);
        node->right = build(preBegin + 1 + leftCount, preEnd, inMid + 1, inEnd);
        return node;
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
    
    vector<int> orders[][2] = {
        {vector<int>{}, vector<int>{}},
        { vector<int>{1}, vector<int>{1} },
        { vector<int>{2, 1}, vector<int>{1, 2} },
        { vector<int>{1, 2}, vector<int>{2, 1} },
        { vector<int>{2, 1, 3,}, vector<int>{1, 2, 3} },
        { vector<int>{3, 2, 1, }, vector<int>{1, 2, 3} },
    };
    for (auto &v2 : orders) {
        printTree(so.buildTree(v2[0], v2[1]));
        cout << endl;
    }
}
