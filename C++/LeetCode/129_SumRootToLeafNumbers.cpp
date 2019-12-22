#include "stdafx.h"

struct TreeNode {
    int val;
    TreeNode *left;
    TreeNode *right;
    TreeNode(int x, TreeNode *left_ = nullptr, TreeNode *right_ = nullptr)
        : val(x)
        , left(left_)
        , right(right_) 
    {
    }
};

//----------------------------------------------

#include <string>
#include <vector>
#include <queue>
#include <iterator>
#include <iostream>
#include <algorithm>

using namespace std;

class Solution {
public:
    int sumNumbers(TreeNode* root) 
    {
        string prefix;
        int result = 0;
        sum(root, prefix, result);
        return result;
    }

    void sum(TreeNode *node, string &prefix, int &result)
    {
        if (node == nullptr)
            return;

        prefix.push_back('0' + node->val);

        if (node->left == nullptr && node->right == nullptr)
        {
            result += atoi(prefix.c_str());
        }
        else
        {
            sum(node->left, prefix, result);
            sum(node->right, prefix, result);
        }

        prefix.pop_back();
    }
};

int main(int argc, char *argv[])
{
    cout << Solution().sumNumbers(nullptr) << "\n";
    cout << Solution().sumNumbers(new TreeNode(1, new TreeNode(2), new TreeNode(3))) << "\n";
    cout << Solution().sumNumbers(new TreeNode(4, new TreeNode(9, new TreeNode(5), new TreeNode(1)), new TreeNode(0))) << "\n";
}
