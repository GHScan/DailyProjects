// vim:fileencoding=gbk

#include "pch.h"

#include <cassert>
#include <ctime>

#include <hash_set>
#include <vector>
#include <set>
#include <utility>
#include <algorithm>


template<typename T>
class RBTree
{
private:
    enum NodeColor
    {
        NC_red,
        NC_black,
    };

    struct Node
    {
    private:
        char m_color;
    public:
        Node *left, *right;
        Node *parent;
        T val;

        Node(const T& val, int color, Node *parent, Node *left = NIL, Node *right = NIL)
        {
            this->val = val;
            this->m_color = (char)color;
            this->parent = parent;
            this->left = left;
            this->right = right;
        }

        int getColor() const { return m_color; }
        void setColor(int color) 
        { 
            assert(this != NIL);
            this->m_color = color; 
        }

        static Node* min(Node *n)
        {
            assert(n != NIL);
            while (n != NIL) n = n->left;
            return n;
        }
        static Node* max(Node *n)
        {
            assert(n != NIL);
            while (n != NIL) n = n->right;
            return n;
        }
        static void bindLeft(Node *p, Node *c)
        {
            assert(p != NIL || c != NIL);
            if (p != NIL) p->left = c;
            if (c != NIL) c->parent = p;
        }
        static void bindRight(Node *p, Node *c)
        {
            assert(p != NIL || c != NIL);
            if (p != NIL) p->right= c;
            if (c != NIL) c->parent = p;
        }
        static void bindChild(Node *p, Node *oc, Node *nc)
        {
            if (p != NIL) {
                assert(oc != NIL);
                if (p->left == oc) p->left = nc;
                else p->right = nc;
                oc->parent = NIL;
            }
            if (nc != NIL) nc->parent = p;
        }

        static void delNodes(Node *n)
        {
            if (n == NIL) return;
            delNodes(n->left);
            delNodes(n->right);
            delete n;
        }
    };

public:
    RBTree(): m_root(NIL), m_size(0){}
    ~RBTree() { clear(); }

    bool insert(const T& val)
    {
        Node **p = &m_root, *parent = NIL;
        while (*p != NIL) {
            parent = *p;
            if ((*p)->val == val) return false;
            else if (val < (*p)->val) p = &(*p)->left;
            else p = &(*p)->right;
        }
        insertFixup(*p = new Node(val, NC_red, parent));
        ++m_size;
        return true;
    }

    bool erase(const T& val)
    {
        Node *n = m_root;
        while (n != NIL && n->val != val) {
            if (val < n->val) n = n->left;
            else n = n->right;
        }
        if (n == NIL) return false;

        if (n->left != NIL && n->right != NIL) {
            Node *next = Node::min(n->right);
            n->val = next->val;
            n = next;
        }
        Node *on = n;
        n = n->left != NIL ? n->left : n->right;
        Node::bindChild(on->parent, on, n);
        if (m_root == on) m_root = n;
        if (on->getColor() == NC_black) {
            eraseFixup(n);
        }
        delete on;
        --m_size;
        return true;
    }

    bool find(const T& val)
    {
        Node *n = m_root;
        while (n != NIL && n->val != val) {
            if (val < n->val) n = n->left;
            else n = n->right;
        }
        return n != NIL;
    }

    int size() const { return m_size; }
    bool empty() const { return size() == 0; }
    void clear()
    {
        Node::delNodes(m_root);
        m_root = NIL;
        m_size = 0;
    }

private:
    void insertFixup(Node *n)
    {
        while (n->parent->getColor() == NC_red) {
            if (n->parent == n->parent->parent->left) {
                Node *ppr = n->parent->parent->right;
                if (ppr->getColor() == NC_red) {
                    n->parent->setColor(NC_black);
                    ppr->setColor(NC_black);
                    n = n->parent->parent;
                    n->setColor(NC_red);
                }
                else {
                    if (n == n->parent->right) {
                        n = n->parent;
                        rotateLeft(n);
                    }
                    n->parent->setColor(NC_black);
                    n->parent->parent->setColor(NC_red);
                    rotateRight(n->parent->parent);
                }
            }
            else {
                Node *ppl = n->parent->parent->left;
                if (ppl->getColor() == NC_red) {
                    n->parent->setColor(NC_black);
                    ppl->setColor(NC_black);
                    n = n->parent->parent;
                    n->setColor(NC_red);
                }
                else {
                    if (n == n->parent->left) {
                        n = n->parent;
                        rotateRight(n);
                    }
                    n->parent->setColor(NC_black);
                    n->parent->parent->setColor(NC_red);
                    rotateLeft(n->parent->parent);
                }
            }
        }
        m_root->setColor(NC_black);
    }
    void eraseFixup(Node *n)
    {
        while (n != m_root && n->getColor() == NC_black) {
            if (n == n->parent->left) {
                Node *pr = n->parent->right;
                if (pr->getColor() == NC_red) {
                    pr->setColor(NC_black);
                    n->parent->setColor(NC_red);
                    rotateLeft(n->parent);
                    pr = n->parent->right;
                }
                if (pr->left->getColor() == NC_black && pr->right->getColor() == NC_black) {
                    pr->setColor(NC_red);
                    n = n->parent;
                }
                else {
                    if (pr->right->getColor() == NC_black) {
                        pr->left->setColor(NC_black);
                        pr->setColor(NC_red);
                        rotateRight(pr);
                        pr = n->parent->right;
                    }
                    pr->setColor(n->parent->getColor());
                    n->parent->setColor(NC_black);
                    pr->right->setColor(NC_black);
                    rotateLeft(n->parent);
                    n = m_root;
                }
            }
            else {
                Node *pl = n->parent->left;
                if (pl->getColor() == NC_red) {
                    pl->setColor(NC_black);
                    n->parent->setColor(NC_red);
                    rotateRight(n->parent);
                    pl = n->parent->left;
                }
                if (pl->right->getColor() == NC_black && pl->left->getColor() == NC_black) {
                    pl->setColor(NC_red);
                    n = n->parent;
                }
                else {
                    if (pl->left->getColor() == NC_black) {
                        pl->right->setColor(NC_black);
                        pl->setColor(NC_red);
                        rotateLeft(pl);
                        pl = n->parent->left;
                    }
                    pl->setColor(n->parent->getColor());
                    n->parent->setColor(NC_black);
                    pl->left->setColor(NC_black);
                    rotateRight(n->parent);
                    n = m_root;
                }
            }
        }
        n->setColor(NC_black);
    }
    void rotateLeft(Node *n)
    {
        assert(n->right != NIL);
        Node *p = n->parent, *r = n->right, *rl = n->right->left;
        if (m_root == n) m_root = r;
        Node::bindChild(p, n, r);
        Node::bindRight(n, rl);
        Node::bindLeft(r, n);
    }
    void rotateRight(Node *n)
    {
        assert(n->left != NIL);
        Node *p = n->parent, *l = n->left, *lr = n->left->right;
        if (m_root == n) m_root = l;
        Node::bindChild(p, n, l);
        Node::bindLeft(n, lr);
        Node::bindRight(l, n);
    }

private:
    Node *m_root;
    int m_size;

    static Node *NIL;
    static Node NIL_INS;
};

template<typename T>
typename RBTree<T>::Node* RBTree<T>::NIL = &RBTree<T>::NIL_INS;
template<typename T>
typename RBTree<T>::Node RBTree<T>::NIL_INS = RBTree<T>::Node(T(), RBTree<T>::NC_black, NULL, NULL, NULL);

void basic_test()
{
    const int LOOP = 100;
    const int LEN = 1 << 10;
    std::vector<int> v(LEN << 2);
    for (int i = 0; i < (int)v.size(); ++i) v[i] = i;

    for (int i = 0; i < LOOP; ++i) {
        std::random_shuffle(v.begin(), v.end());

        RBTree<int> tree;
        std::set<int> st;
        for (int i = 0; i < LEN; ++i) {
            if (v[i] % 3 != 0) {
                tree.insert(v[i]);
                st.insert(v[i]);
            }
            else {
                tree.erase(v[i]);
                st.erase(v[i]);
            }
        }
        assert(st.size() == tree.size());
        for (int i = 0; i < LEN / 2; ++i) {
            if (st.count(v[i]) > 0) {
                assert(tree.find(v[i]));
            }
            else assert(!tree.find(v[i]));
        }
    }
}

void performance_test()
{
#ifdef _DEBUG
    const int LOOP = 1;
#else
    const int LOOP = 1000;
#endif

    std::vector<int> v;
    for (int i = 0; i < (1 << 15); ++i) v.push_back(i);

    {
        RBTree<int> st;
        clock_t t = 0;
        for (int i = 0; i < LOOP; ++i) {
            std::random_shuffle(v.begin(), v.end());
            clock_t c = clock();
            for (int i = 0; i < (int)v.size(); ++i) {
                if (v[i] % 3 != 0) st.insert(v[i]);
                else st.erase(v[i]);
            }
            t += clock() - c;
        }

        cout << "RBTree : " << (t / float(CLOCKS_PER_SEC)) << endl;
    }

    {
        std::set<int> st;
        clock_t t = 0;
        for (int i = 0; i < LOOP; ++i) {
            std::random_shuffle(v.begin(), v.end());
            clock_t c = clock();
            for (int i = 0; i < (int)v.size(); ++i) {
                if (v[i] % 3 != 0) st.insert(v[i]);
                else st.erase(v[i]);
            }
            t += clock() - c;
        }

        cout << "std::set : " << (t / float(CLOCKS_PER_SEC)) << endl;
    }

    {
        stdext::hash_set<int> st;
        clock_t t = 0;
        for (int i = 0; i < LOOP; ++i) {
            std::random_shuffle(v.begin(), v.end());
            clock_t c = clock();
            for (int i = 0; i < (int)v.size(); ++i) {
                if (v[i] % 3 != 0) st.insert(v[i]);
                else st.erase(v[i]);
            }
            t += clock() - c;
        }

        cout << "stdext::hash_set : " << (t / float(CLOCKS_PER_SEC)) << endl;
    }
}

int main()
{
    srand((unsigned int)time(NULL));

    basic_test();
    performance_test();
}
