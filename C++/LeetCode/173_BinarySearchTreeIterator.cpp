
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
 

//-----------------------

#include <cstdio>

#include <string>
#include <vector>
#include <stack>
#include <list>
#include <unordered_map>
#include <iostream>
#include <iterator>
#include <algorithm>

using namespace std;

class BSTIterator {
public:
    BSTIterator(TreeNode *root) {
        for (; root != nullptr; root = root->left)
            nodes.push_back(root);
    }

    /** @return the next smallest number */
    int next() {
        auto node = nodes.back();
        int val = node->val;
        nodes.pop_back();

        for (auto p = node->right; p != nullptr; p = p->left)
            nodes.push_back(p);

        return val;
    }

    /** @return whether we have a next smallest number */
    bool hasNext() {
        return !nodes.empty();
    }

private:
    vector<TreeNode *> nodes;
};


int main()
{
    auto root = new TreeNode(7, new TreeNode(3), new TreeNode(15, new TreeNode(9), new TreeNode(20)));
    BSTIterator iterator(root);
    cout << iterator.next() << "\n";    // return 3
    cout << iterator.next() << "\n";    // return 7
    cout << iterator.hasNext() << "\n"; // return true
    cout << iterator.next() << "\n";    // return 9
    cout << iterator.hasNext() << "\n"; // return true
    cout << iterator.next() << "\n";    // return 15
    cout << iterator.hasNext() << "\n"; // return true
    cout << iterator.next() << "\n";    // return 20
    cout << iterator.hasNext() << "\n"; // return false
}