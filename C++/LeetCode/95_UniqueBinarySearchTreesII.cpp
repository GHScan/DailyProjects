#include "stdafx.h" 

struct TreeNode {
    int val;
    TreeNode *left;
    TreeNode *right;
    TreeNode(int x) : val(x), left(NULL), right(NULL) {}
};
//-----------------------------------------------------------------
#include <functional>

class Solution {
public:
    vector<TreeNode *> generateTrees(int n) {
        vector<TreeNode*> result;
        if (n == 0) return vector<TreeNode*>{nullptr};

        vector<int> nums(n, 0);
        for (int i = 0; i < n; ++i) nums[i] = i + 1;
        buildTree(&nums[0], nums.size(), [&](TreeNode *node){
            result.push_back(node);
        });
        return result;
    }
private:
    void buildTree(int A[], int n, function<void(TreeNode*)> const &callback) {
        if (n == 0) {
            callback(nullptr);
            return;
        }

        for (int i = 0; i < n; ++i) {
            buildTree(A, i, [&](TreeNode *left){
                buildTree(A + i + 1, n - i - 1, [&](TreeNode *right){
                    TreeNode *node = new TreeNode(A[i]);
                    node->left = left;
                    node->right = right;
                    callback(node);
                });
            });
        }
    }
};
//-----------------------------------------------------------------
static void printTree(TreeNode *node) {
    if (node == nullptr) return;
    printTree(node->left);
    cout << node->val << ',';
    printTree(node->right);
}
static void printResult(vector<TreeNode*> const &result) {
    cout << "############ " << result.size() << endl;
    for (auto tree : result) {
        printTree(tree);
        cout << endl;
    }
}

int main() {
    Solution so;
    
    printResult(so.generateTrees(0));
    printResult(so.generateTrees(1));
    printResult(so.generateTrees(2));
    printResult(so.generateTrees(3));
    printResult(so.generateTrees(4));
}
