
#include "pch.h"

#include <assert.h>

template<typename T>
class BinarySearchTree {
    BinarySearchTree(const BinarySearchTree&) = delete;
    BinarySearchTree& operator = (const BinarySearchTree&) = delete;
private:
    struct Node {
        Node *left, *right;
        T value;
        Node (const T &val, Node *_left = nullptr, Node *_right = nullptr): left(_left), right(_right), value(val) {}
        void destroy() {
            if (left != nullptr) left->destroy();
            if (right != nullptr) right->destroy();
            delete this;
        }
        void infixIterate(function<void(const T&)> f) {
            if (left != nullptr) left->infixIterate(f);
            f(value);
            if (right != nullptr) right->infixIterate(f);
        }
        int getDepth() const {
            int ld = left == nullptr ? 0 : left->getDepth();
            int rd = right == nullptr ? 0 : right->getDepth();
            return max(ld, rd) + 1;
        }
    };
public:
    BinarySearchTree(): mRoot(nullptr) {
    }
    ~BinarySearchTree() {
        clear();
    }

    void clear() {
        if (mRoot != nullptr) mRoot->destroy();
        mRoot = nullptr;
    }
    void insertRange(T *begin, T *end) {
        if (begin < end) {
            T *mid = begin + (end - begin) / 2;
            insert(*mid);
            insertRange(begin, mid);
            insertRange(mid + 1, end);
        }
    }
    void insert(const T &val) {
        Node **p = &mRoot;
        while (*p != nullptr) {
            if (val == (*p)->value) return;
            else if (val < (*p)->value) p = &(*p)->left;
            else p = &(*p)->right;
        }
        *p = new Node(val);
    }
    void remove(const T &val) {
        Node **p = &mRoot;
        for (; *p != nullptr;) {
            if (val == (*p)->value) break;
            else if (val < (*p)->value) p = &(*p)->left;
            else p = &(*p)->right;
        }
        if (*p == nullptr) return;

        Node *t = *p;
        if (t->left == nullptr || t->right == nullptr) {
            *p = t->left == nullptr ? t->right : t->left;
            delete t;
        } else {
            Node **min = minChild(&t->right);
            t->value = (*min)->value;
            t = *min;
            *min = t->right;
            delete t;
        }
    }
    bool contain(const T &val) const {
        for (Node *n = mRoot; n != nullptr; ) {
            if (val == n->value) return true;
            else if (val < n->value) n = n->left;
            else n = n->right;
        }
        return false;
    }
    void foreach(function<void(const T &)> f) {
        if (mRoot != nullptr) mRoot->infixIterate(f);
    }
    int getDepth() const {
        return mRoot == nullptr ? 0 : mRoot->getDepth();
    }
private:
    static Node** minChild(Node **n) {
        for (; (*n)->left != nullptr; n = &(*n)->left);
        return n;
    }
private:
    Node *mRoot;
};

static void testBinarySearchTree() {
    BinarySearchTree<int> tree;
    for (int i : {2, 5, 1, 4, 2, 8, 9, 4}) tree.insert(i);
    for (int i = 3; i <= 6; ++i) printf("contain %d:%d\n", i, tree.contain(i));
    tree.remove(2);
    tree.remove(5);
    tree.remove(9);
    for (int i = 3; i <= 6; ++i) printf("contain %d:%d\n", i, tree.contain(i));
    tree.clear();

    printf("@tree: {"); tree.foreach([](int i){ cout << i << ',';}); puts("}");
    for (int i : {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}) tree.insert(i);
    printf("@tree: {"); tree.foreach([](int i){ cout << i << ',';}); puts("}");
    printf("depth %d\n", tree.getDepth());
    tree.clear();

    vector<int> a = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
    tree.insertRange(&a[0], &a[0] + a.size());
    printf("@tree: {"); tree.foreach([](int i){ cout << i << ',';}); puts("}");
    printf("depth %d\n", tree.getDepth());
}

int main() {
    testBinarySearchTree();
    puts("finish");
}
