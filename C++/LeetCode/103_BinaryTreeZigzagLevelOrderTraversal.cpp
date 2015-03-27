#include "stdafx.h" 

struct TreeNode {
    int val;
    TreeNode *left;
    TreeNode *right;
    TreeNode(int x) : val(x), left(NULL), right(NULL) {}
    TreeNode(int x, TreeNode *_left, TreeNode *_right) : val(x), left(_left), right(_right) {}
};
//-----------------------------------------------------------------
#include <queue>
#include <algorithm>

class Solution {
public:
    vector<vector<int> > zigzagLevelOrder(TreeNode *root) {
        vector<vector<int>> result;
        if (root == nullptr) return result;

        queue<pair<TreeNode*, int>> queue;
        queue.push(make_pair(root, 0));
        while (!queue.empty()) {
            auto p = queue.front();
            queue.pop();

            if ((int)result.size() <= p.second) result.push_back(vector<int>());
            result.back().push_back(p.first->val);

            if (p.first->left != nullptr) queue.push(make_pair(p.first->left, p.second + 1));
            if (p.first->right != nullptr) queue.push(make_pair(p.first->right, p.second + 1));
        }

        for (int i = 1; i < (int)result.size(); i += 2) {
            reverse(result[i].begin(), result[i].end());
        }

        return result;
    }
};
//-----------------------------------------------------------------
static void printResult(vector<vector<int>> const &result) {
    cout << "################" << endl;
    for (auto &v : result) {
        for (auto i : v) cout << i << ',';
        cout << endl;
    }
}

int main() {
    Solution so;
    printResult(so.zigzagLevelOrder(nullptr));
    printResult(so.zigzagLevelOrder(new TreeNode(1)));
    printResult(so.zigzagLevelOrder(new TreeNode(1, new TreeNode(2, new TreeNode(3), nullptr), nullptr)));
    printResult(so.zigzagLevelOrder(new TreeNode(1, new TreeNode(2, new TreeNode(3), nullptr), new TreeNode(2))));
    printResult(so.zigzagLevelOrder(new TreeNode(3, new TreeNode(9), new TreeNode(20, new TreeNode(15), new TreeNode(7)))));
}
