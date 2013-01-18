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
    enum NodeSide
    {
        NS_left,
        NS_right,
    };
    template<int s>
    struct Sibline
    {
        enum { value = s ^ 1 };
    };

    struct Node
    {
        int color;
        Node *children[2];
        Node *parent;
        T val;

        Node(const T& val, int color, Node *parent)
        {
            this->val = val;
            this->color = color;
            this->parent = parent;
            this->children[NS_left] = this->children[NS_right] = NIL;
        }

        static Node* min(Node *n)
        {
            assert(n != NIL);
            while (n != NIL) n = n->children[NS_left];
            return n;
        }
        static Node* max(Node *n)
        {
            assert(n != NIL);
            while (n != NIL) n = n->children[NS_right];
            return n;
        }
        template<int s>
        static void bindChild(Node *p, Node *c)
        {
            assert(p != NIL || c != NIL);
            if (p != NIL) p->children[s] = c;
            if (c != NIL) c->parent = p;
        }
        static void replaceChild(Node *p, Node *oc, Node *nc)
        {
            if (p != NIL) {
                assert(oc != NIL);
                if (p->children[NS_left] == oc) p->children[NS_left] = nc;
                else p->children[NS_right] = nc;
                oc->parent = NIL;
            }
            if (nc != NIL) nc->parent = p;
        }

        static void delNodes(Node *n)
        {
            if (n == NIL) return;
            delNodes(n->children[NS_left]);
            delNodes(n->children[NS_right]);
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
            else if (val < (*p)->val) p = &(*p)->children[NS_left];
            else p = &(*p)->children[NS_right];
        }
        insertFixup(*p = new Node(val, NC_red, parent));
        ++m_size;
        return true;
    }

    bool erase(const T& val)
    {
        Node *n = m_root;
        while (n != NIL && n->val != val) {
            if (val < n->val) n = n->children[NS_left];
            else n = n->children[NS_right];
        }
        if (n == NIL) return false;

        if (n->children[NS_left] != NIL && n->children[NS_right] != NIL) {
            Node *next = Node::min(n->children[NS_left]);
            n->val = next->val;
            n = next;
        }
        Node *on = n;
        n = n->children[NS_left] != NIL ? n->children[NS_left] : n->children[NS_right];
        Node::replaceChild(on->parent, on, n);
        if (m_root == on) m_root = n;
        if (on->color == NC_black) {
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
            if (val < n->val) n = n->children[NS_left];
            else n = n->children[NS_right];
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
    template<int s>
    Node* insertFixupHalf(Node *n)
    {
        Node *ppr = n->parent->parent->children[Sibline<s>::value];
        if (ppr->color == NC_red) {
            n->parent->color = NC_black;
            ppr->color = NC_black;
            n = n->parent->parent;
            n->color = NC_red;
        }
        else {
            if (n == n->parent->children[Sibline<s>::value]) {
                n = n->parent;
                rotate<s>(n);
            }
            n->parent->color = NC_black;
            n->parent->parent->color = NC_red;
            rotate<Sibline<s>::value>(n->parent->parent);
        }
        return n;
    }
    void insertFixup(Node *n)
    {
        while (n->parent->color == NC_red) {
            if (n->parent == n->parent->parent->children[NS_left]) {
                n = insertFixupHalf<NS_left>(n);
            }
            else {
                n = insertFixupHalf<NS_right>(n);
            }
        }
        m_root->color = NC_black;
    }
    template<int s>
    Node* eraseFixupHalf(Node *n)
    {
        Node *pr = n->parent->children[Sibline<s>::value];
        if (pr->color == NC_red) {
            pr->color = NC_black;
            n->parent->color = NC_red;
            rotate<s>(n->parent);
            pr = n->parent->children[Sibline<s>::value];
        }
        if (pr->children[NS_left]->color == NC_black && pr->children[NS_right]->color == NC_black) {
            pr->color = NC_red;
            n = n->parent;
        }
        else {
            if (pr->children[Sibline<s>::value]->color == NC_black) {
                pr->children[s]->color = NC_black;
                pr->color = NC_red;
                rotate<Sibline<s>::value>(pr);
                pr = n->parent->children[Sibline<s>::value];
            }
            pr->color = n->parent->color;
            n->parent->color = NC_black;
            pr->children[Sibline<s>::value]->color = NC_black;
            rotate<s>(n->parent);
            n = m_root;
        }
        return n;
    }
    void eraseFixup(Node *n)
    {
        while (n != m_root && n->color == NC_black) {
            if (n == n->parent->children[NS_left]) {
                n = eraseFixupHalf<NS_left>(n);
            }
            else {
                n = eraseFixupHalf<NS_right>(n);
            }
        }
        n->color = NC_black;
    }
    template<int s>
    void rotate(Node *n)
    {
        assert(n->children[Sibline<s>::value] != NIL);
        Node *p = n->parent, *r = n->children[Sibline<s>::value], *rl = n->children[Sibline<s>::value]->children[s];
        if (m_root == n) m_root = r;
        Node::replaceChild(p, n, r);
        Node::bindChild<Sibline<s>::value>(n, rl);
        Node::bindChild<s>(r, n);
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
typename RBTree<T>::Node RBTree<T>::NIL_INS = RBTree<T>::Node(T(), RBTree<T>::NC_black, NULL);

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
