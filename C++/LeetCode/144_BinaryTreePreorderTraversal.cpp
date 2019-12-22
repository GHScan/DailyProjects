 struct TreeNode {
     int val;
     TreeNode *left;
     TreeNode *right;
     TreeNode(int x) : val(x), left(nullptr), right(nullptr) {}
 };
 

//---------------------------

#include <cstdio>

#include <string>
#include <vector>
#include <iostream>

using namespace std;

class Solution {
public:
    vector<int> preorderTraversal(TreeNode *root) {
        vector<int> result;
        traverse(result, root);
        return result;
    }

    void traverse(vector<int> &result, TreeNode *node)
    {
        if (node == nullptr) return;

        result.push_back(node->val);
        traverse(result, node->left);
        traverse(result, node->right);
    }
};

int main()
{
    
}