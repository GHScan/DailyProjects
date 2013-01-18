// vim:fileencoding=gbk

#include "pch.h"

#include <cassert>

template<typename T>
class BinarySearchTree
{
private:
    struct Node
    {
        Node(const T&val, Node* left = NULL, Node *right = NULL) {
            this->val = val;
            this->left = left;
            this->right = right;
        }
        Node *left, *right;
        T val;
    };

public:
    BinarySearchTree() : m_root(NULL), m_size(0) {}
    ~BinarySearchTree() { rdeleteNode(m_root); }

    bool empty() const { return size() == 0; }
    int size() const { return m_size; }

    bool insert(const T& val)
    {
        Node** node = searchNode(val);
        if (*node != NULL) return false;
        *node = new Node(val);
        ++m_size;
        return true;
    }

    bool erase(const T& val)
    {
        Node** pnode = searchNode(val);
        Node *node = *pnode;
        if (node == NULL) return false;
        if (node->left != NULL && node->right != NULL) {
            pnode = minNode(&node->right);
            node->val = (*pnode)->val;
            *pnode = (*pnode)->right;
            delete *pnode;
        }
        else {
            *pnode = node->left != NULL ? node->left : node->right;
            delete node;
        }
        --m_size;
        return true;
    }

    bool search(const T& val) const
    {
        return *const_cast<BinarySearchTree*>(this)->searchNode(val) != NULL;
    }

private:
    static void rdeleteNode(Node *n)
    {
        if (n == NULL) return;
        rdeleteNode(n->left);
        rdeleteNode(n->right);
        delete n;
    }
    static Node** minNode(Node **pn)
    {
        assert(pn != NULL);
        while ((*pn)->left != NULL) pn = &(*pn)->left;
        return pn;
    }

    Node** searchNode(const T& val)
    {
        Node **pcur = &m_root;
        while (*pcur != NULL) {
            if ((*pcur)->val == val) return pcur;
            else if (val < (*pcur)->val) pcur = &(*pcur)->left;
            else pcur = &(*pcur)->right;
        }
        return pcur;
    }

private:
    Node *m_root;
    int m_size;
};

void basic_test()
{
    BinarySearchTree<int> tree;
    for (int i = 0; i < 15; i += 3) tree.insert(i);

    assert(tree.size() == 5);
    assert(tree.search(3));
    assert(tree.search(12));
    assert(!tree.search(10));
    assert(!tree.erase(8));
    assert(tree.search(9));
    assert(tree.erase(9));
    assert(!tree.erase(9));
    assert(tree.size() == 4);
    assert(!tree.insert(3));
    assert(tree.insert(13));
    assert(tree.size() == 5);
    assert(tree.erase(13));
    assert(tree.erase(12));
    assert(tree.erase(6));
    assert(tree.erase(3));
    assert(tree.erase(0));
    assert(tree.empty());
}

int main()
{
    basic_test();
}
