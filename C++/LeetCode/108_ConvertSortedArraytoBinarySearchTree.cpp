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
    TreeNode *sortedArrayToBST(vector<int> const &num) {
        if (num.empty()) return nullptr;
        return build(&num[0], &num[0] + num.size());
    }
private:
    TreeNode* build(const int *begin, const int *end) {
        if (begin == end) return nullptr;
        const int *mid = begin + (end - begin) / 2;
        auto node = new TreeNode(*mid);
        node->left = build(begin, mid);
        node->right = build(mid + 1, end);
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
    vector<int> nums[] = {
        vector<int>{},
        vector<int>{1},
        vector<int>{1, 1, 1},
        vector<int>{1, 2, 3,},
        vector<int>{1, 3, 5, 6, },
        vector<int>{1, 3, 6, 7, 7, 8},
    };
    for (auto &num : nums) {
        printTree(so.sortedArrayToBST(num));
        cout << endl;
    }
}
