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
class BinarySearchTree
{
private:
    struct Node
    {
        Node *parent;
        Node *left, *right;
        T val;
        Node(const T& val, Node *parent, Node *left = NULL, Node *right = NULL)
        {
            this->val = val;
            this->parent = parent;
            this->left = left;
            this->right = right;
        }

        static Node* min(Node *n)
        {
            if (n == NULL) return n;
            while (n->left != NULL) n = n->left;
            return n;
        }
        static Node* max(Node *n)
        {
            if (n == NULL) return n;
            while (n->right != NULL) n = n->right;
            return n;
        }
        static Node* next(Node *n)
        {
            assert(n != NULL);
            if (n->right != NULL) {
                return min(n->right);
            }
            while (n->parent != NULL && n->parent->right == n) n = n->parent;
            return n->parent;
        }
        static Node* pre(Node *n)
        {
            assert(n != NULL);
            if (n->left != NULL) {
                return max(n->left);
            }
            while (n->parent != NULL && n->parent->left == n) n = n->parent;
            return n->parent;
        }
        static void delNodes(Node *n)
        {
            if (n == NULL) return;
            delNodes(n->left);
            delNodes(n->right);
            delete n;
        }
    };

public:
    class iterator
    {
    public:
        typedef std::bidirectional_iterator_tag iterator_category;
        typedef T value_type;
        typedef size_t difference_type;
        typedef size_t distance_type;	
        typedef T* pointer;
        typedef T& reference;
    public:
        iterator operator ++ (int)
        {
            iterator r(*this);
            ++*this;
            return r;
        }
        iterator& operator ++ ()
        {
            m_n = Node::next(m_n);
            return *this;
        }
        iterator operator -- (int)
        {
            iterator r(*this);
            --*this;
            return r;
        }
        iterator& operator -- () 
        {
            m_n = Node::pre(m_n);
            return *this;
        }
        bool operator == (const iterator& o) const
        {
            return m_n == o.m_n;
        }
        bool operator != (const iterator& o) const
        {
            return !(*this == o);
        }
        T* operator -> () const
        {
            return m_n != NULL ? &m_n->val : NULL;
        }
        T& operator * () const
        {
            return m_n != NULL ? m_n->val : *(T*)0;
        }
    private:
        friend class BinarySearchTree;
        explicit iterator(Node *n = NULL): m_n(n) {}
    private:
        Node *m_n;
    };

public:
    BinarySearchTree(): m_root(NULL), m_size(0) {}
    ~BinarySearchTree() { clear(); }

    bool empty() const { return size() == 0; }
    size_t size() const { return m_size; }

    void clear() 
    { 
        Node::delNodes(m_root); 
        m_root = NULL;
        m_size = 0;
    }

    iterator begin() const { return iterator(Node::min(m_root)); }
    iterator begin() { return iterator(Node::min(m_root)); }
    iterator end() const { return iterator(NULL); }
    iterator end() { return iterator(NULL); }

    std::pair<iterator, bool> insert(const T& val) { return insert(lower_bound(val), val); }
    std::pair<iterator, bool> insert(iterator iter, const T& val)
    {
        if (m_root == NULL) {
            ++m_size;
            return std::make_pair(iterator(m_root = new Node(val, NULL)), true);
        }
        if (iter == end()) {
            ++m_size;
            Node *n = Node::min(m_root);
            return std::make_pair(iterator(n->left = new Node(val, n)), true);
        }
        else {
            if (iter.m_n->val == val) return std::make_pair(iter, false);
            ++m_size;

            Node *p = NULL;
            Node *n = iter.m_n;
            while (n != NULL) {
                p = n;
                if (val < n->val) n = n->left;
                else n = n->right;
            }
            n = new Node(val, p);
            if (val < p->val) p->left = n;
            else p->right = n;
            return std::make_pair(iterator(n), true);
        }
    }

    void erase(const T& val) 
    { 
        iterator iter = lower_bound(val);
        if (iter != end() && *iter == val) erase(iter);
    }
    void erase(iterator iter)
    {
        if (iter == end()) return;
        --m_size;
        Node *n = iter.m_n;
        if (n->left != NULL && n->right != NULL) {
            Node *cn = Node::min(n->right);
            n->val = cn->val;
            n = cn;
        }
        Node *p = n->parent;
        Node **pp = p == NULL ? &m_root : (p->left == n ? &p->left : &p->right);
        Node *cn = n->left != NULL ? n->left : n->right;
        if (cn != NULL) cn->parent = p;
        *pp = cn;
        delete n;
    }

    iterator find(const T& val) const
    {
        iterator iter = lower_bound(val);
        return iter != end() && *iter == val ? iter : end();
    }

    size_t count(const T& val) const { return find(val) != end() ? 1 : 0; }

    iterator lower_bound(const T& val) const
    {
        Node *p = NULL;
        Node *n = m_root;
        while (n != NULL && n->val != val) {
            p = n;
            if (val < n->val) n = n->left;
            else n = n->right;
        }
        if (n != NULL) return iterator(n);
        if (p == NULL) return end();
        else if (val < p->val) return iterator(Node::pre(p));
        else {
            assert(val > p->val);
            return iterator(p);
        }
    }

private:
    Node *m_root;
    size_t m_size;
};

void printBST(const BinarySearchTree<int>& tree)
{
    BinarySearchTree<int>::iterator iter = tree.begin();
    while(iter != tree.end()) {
        cout << *iter << ',';
        ++iter;
    }
    cout << endl;
}

void basic_test()
{
    BinarySearchTree<int> tree;
    std::set<int> st;

    const int LEN = 1 << 17;
    std::vector<int> v(LEN);
    for (int i = 0; i < LEN; ++i) v[i] = i;

    std::random_shuffle(v.begin(), v.end());
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
     assert(std::equal(st.begin(), st.end(), tree.begin()));
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
        BinarySearchTree<int> st;
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

        cout << "BinarySearchTree : " << (t / float(CLOCKS_PER_SEC)) << endl;
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
