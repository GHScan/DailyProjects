 struct TreeNode 
 {
     int val;
     TreeNode *left;
     TreeNode *right;
     TreeNode(int x, TreeNode *left_ = nullptr, TreeNode *right_ = nullptr) 
         : val(x), left(left_), right(right_) {}
 }; 

//----------------

#include <cstdio>
#include <cassert>

#include <list>
#include <stack>
#include <queue>
#include <string>
#include <vector>
#include <numeric>
#include <iostream>
#include <iterator>
#include <algorithm>
#include <unordered_set>
#include <unordered_map>

using namespace std;

class Solution {
public:
    vector<int> rightSideView(TreeNode *root) 
    {
        vector<int> result;
        if (root == nullptr) return result;

        queue<pair<int, TreeNode *>> q;
        q.push(make_pair(0, root));

        while (!q.empty())
        {
            int level = q.front().first;
            TreeNode *node = q.front().second;
            q.pop();

            result.resize(level + 1);
            result[level] = node->val;

            if (node->left != nullptr)
                q.push(make_pair(level + 1, node->left));
            if (node->right != nullptr)
                q.push(make_pair(level + 1, node->right));
        }
        
        return result;
    }
};

int main()
{
    for (auto &test : vector<TreeNode*>
        {
            nullptr,
            new TreeNode(1, new TreeNode(2, nullptr, new TreeNode(5)), new TreeNode(3, nullptr, new TreeNode(4))),
            new TreeNode(1, new TreeNode(2, nullptr, new TreeNode(5)), new TreeNode(3, nullptr)),
        })
    {
        auto result = Solution().rightSideView(test);
        copy(result.begin(), result.end(), ostream_iterator<int>(cout, ","));
        cout << "\n";
    }
}