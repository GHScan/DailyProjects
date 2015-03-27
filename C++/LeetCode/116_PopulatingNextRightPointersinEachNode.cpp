#include "stdafx.h" 

struct TreeLinkNode {
    int val;
    TreeLinkNode *left, *right, *next;
    TreeLinkNode(int x) : val(x), left(NULL), right(NULL), next(NULL) {}
    TreeLinkNode(int x, TreeLinkNode *_left, TreeLinkNode *_right) : val(x), left(_left), right(_right), next(NULL) {}
};
//-----------------------------------------------------------------
class Solution {
public:
    void connect(TreeLinkNode *root) {
        setNext(root, nullptr);
    }
private:
    void setNext(TreeLinkNode *node, TreeLinkNode *next) {
        if (node == nullptr) return;
        node->next = next;
        setNext(node->left, node->right);
        setNext(node->right, next == nullptr ? nullptr : next->left);
    }
};
//-----------------------------------------------------------------
static void printResult(TreeLinkNode *node) {
    if (node == nullptr) return;
    printResult(node->left);
    printf("%d(%d),", node->val, node->next == nullptr ? -1 : node->next->val);
    printResult(node->right);
}

int main() {
    Solution so;
    TreeLinkNode *nodes[] = {
        new TreeLinkNode(1),
        new TreeLinkNode(1, new TreeLinkNode(2), new TreeLinkNode(3)),
        new TreeLinkNode(1,
            new TreeLinkNode(2, new TreeLinkNode(4), new TreeLinkNode(5)),
            new TreeLinkNode(3, new TreeLinkNode(6), new TreeLinkNode(7))),
    };
    for (auto node : nodes) {
        so.connect(node);
        printResult(node);
        puts("");
    }
}
