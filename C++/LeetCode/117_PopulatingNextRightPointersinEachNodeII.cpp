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
        vector<TreeLinkNode*> nexts;
        setNext(root, 0, nexts);
    }
private:
    void setNext(TreeLinkNode *node, int depth, vector<TreeLinkNode*> &nexts) {
        if (node == nullptr) return;
        while ((int)nexts.size() <= depth) nexts.push_back(nullptr);
        node->next = nexts[depth];
        nexts[depth] = node;
        setNext(node->right, depth + 1, nexts);
        setNext(node->left, depth + 1, nexts);
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
        new TreeLinkNode(1,
            new TreeLinkNode(2, new TreeLinkNode(4), new TreeLinkNode(5)),
            new TreeLinkNode(3, nullptr, new TreeLinkNode(7))),
        new TreeLinkNode(1,
            new TreeLinkNode(2, new TreeLinkNode(4), nullptr),
            new TreeLinkNode(3, nullptr, new TreeLinkNode(7))),
        new TreeLinkNode(1,
            new TreeLinkNode(2, new TreeLinkNode(4, new TreeLinkNode(7), nullptr), new TreeLinkNode(5)),
            new TreeLinkNode(3, nullptr, new TreeLinkNode(6, nullptr, new TreeLinkNode(8)))),
    };
    for (auto node : nodes) {
        so.connect(node);
        printResult(node);
        puts("");
    }
}
