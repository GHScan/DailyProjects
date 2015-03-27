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
    TreeNode *buildTree(vector<int> &inorder, vector<int> &postorder) {
        if (inorder.empty()) return nullptr;
        return build(&postorder[0], &postorder[0] + postorder.size(), &inorder[0], &inorder[0] + inorder.size());
    }
private:
    TreeNode* build(const int *postBegin, const int *postEnd, const int *inBegin, const int *inEnd) {
        if (postBegin == postEnd) return nullptr;

        const int *inMid = find(inBegin, inEnd, postEnd[-1]);
        int leftCount = inMid - inBegin;

        auto node = new TreeNode(postEnd[-1]);
        node->left = build(postBegin, postBegin + leftCount, inBegin, inMid);
        node->right = build(postBegin + leftCount, postEnd - 1, inMid + 1, inEnd);
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
