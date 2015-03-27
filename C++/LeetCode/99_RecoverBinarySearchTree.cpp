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
    void recoverTree(TreeNode *root) {
        int* wrongs[2] = {0};
        getMin(root, nullptr, wrongs);
        getMax(root, nullptr, wrongs + 1);
        if (wrongs[0] != nullptr && wrongs[1] != nullptr) {
            swap(*wrongs[0], *wrongs[1]);
        }
    }
private:
    int* getMax(TreeNode *node, int *leftMaxVal, int **wrong) {
        if (*wrong != nullptr) return nullptr;
        if (node == nullptr) return leftMaxVal;
        leftMaxVal = getMax(node->left, leftMaxVal, wrong);
        if (leftMaxVal != nullptr && node->val < *leftMaxVal) {
            *wrong = leftMaxVal;
            return nullptr;
        }
        return getMax(node->right, &node->val, wrong);
    }
    int* getMin(TreeNode *node, int *rightMinVal, int **wrong) {
        if (*wrong != nullptr) return nullptr;
        if (node == nullptr) return rightMinVal;
        rightMinVal = getMin(node->right, rightMinVal, wrong);
        if (rightMinVal != nullptr && node->val > *rightMinVal) {
            *wrong = rightMinVal;
            return nullptr;
        }
        return getMin(node->left, &node->val, wrong);
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
    };
    for (auto tree : trees) {
        so.recoverTree(tree);
        printTree(tree);
        cout << endl;
    }
}
