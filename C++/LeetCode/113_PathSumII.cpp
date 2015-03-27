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
    vector<vector<int> > pathSum(TreeNode *root, int sum) {
        vector<vector<int>> result;
        if (root == nullptr) return result;

        vector<int> path;
        find(result, path, root, sum);

        return result;
    }
private:
    void find(vector<vector<int>> &result, vector<int> &path, TreeNode *root, int sum) {
        sum -= root->val;
        path.push_back(root->val);

        if (root->left != nullptr) find(result, path, root->left, sum);
        if (root->right != nullptr) find(result, path, root->right, sum);
        if (root->left == nullptr && root->right == nullptr && sum == 0) {
            result.push_back(path);
        }

        path.pop_back();
    }
};
//-----------------------------------------------------------------
static void printResult(vector<vector<int>> const &result) {
    cout << "##############" << endl;
    for (auto &v : result) {
        for (auto i : v) cout << i << ',';
        cout << endl;
    }
}

int main() {
    Solution so;
    printResult(so.pathSum(nullptr, 0));
    printResult(so.pathSum(new TreeNode(1), 0));
    printResult(so.pathSum(new TreeNode(1), 1));
    printResult(so.pathSum(new TreeNode(1), 2));
    printResult(so.pathSum(new TreeNode(1, new TreeNode(2), nullptr), 1));
    printResult(so.pathSum(new TreeNode(1, new TreeNode(2), nullptr), 2));
    printResult(so.pathSum(new TreeNode(1, new TreeNode(2), nullptr), 3));

    auto tree = new TreeNode(5, 
        new TreeNode(4,
            new TreeNode(11, new TreeNode(7), new TreeNode(2)),
            nullptr),
        new TreeNode(8,
            new TreeNode(13),
            new TreeNode(4, new TreeNode(5), new TreeNode(1))));
    printResult(so.pathSum(tree, 22));
    printResult(so.pathSum(tree, 26));
    printResult(so.pathSum(tree, 18));
    printResult(so.pathSum(tree, 17));
}
