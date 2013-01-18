// vim: fileencoding=gbk

#ifndef HASH_H
#define HASH_H

#include <cassert>

#include <string>
#include <vector>
#include <algorithm>

#include "MemoryPool.h"

template<typename T>
inline size_t hashValue(const T& val)
{
    return (size_t)val ^ 0xdeadbeef;
}

template<typename IterT>
inline size_t hashValue(IterT begin, IterT end)
{
    size_t r = 2166136261u;
    while(begin != end)
        r = 16777619u * r ^ (size_t)*begin++;
    return r;
}

inline size_t hashValue(char *str)
{
    return hashValue(str, str + strlen(str));
}

inline size_t hashValue(const std::string& s)
{
    return hashValue(s.c_str(), s.c_str() + s.size());
}

template<typename T>
struct HashSetTraits
{
    static size_t hash(const T& val) { return hashValue(val); }
    static bool equal(const T& a, const T& b) { return a == b; }
};

template<typename T, typename Traits = HashSetTraits<T> >
class HashSet
{
private:
    struct Node
    {
        Node *next;
        T val;
    };
public:
    class Iterator
    {
    public:
        typedef std::forward_iterator_tag iterator_category;
        typedef T value_type;
        typedef size_t difference_type;
        typedef size_t distance_type;	// retained
        typedef T* pointer;
        typedef T& reference;
    public:
        const T& operator * () const
        {
            assert(m_n != NULL);
            return m_n->val;
        }
        T& operator * () { return (T&)**((const Iterator*)this); }
        const T* operator -> () const
        {
            assert(m_n != NULL);
            return &m_n->val;
        }
        T* operator -> () { return (T*)((const Iterator*)this)->operator->(); }
        Iterator& operator ++ ()
        {
            assert(m_i >= 0 && m_n != NULL);
            m_n = m_n->next;
            return forwardUntilValidOrEnd();
        }
        Iterator operator ++ (int)
        {
            Iterator i(*this);
            ++*this;
            return i;
        }
        bool operator == (const Iterator& o) const 
        {
            return &m_table == &o.m_table && m_i == o.m_i && m_n == o.m_n;
        } 
        bool operator != (const Iterator& o) const { return !(*this == o);}
    private:
        friend HashSet;
        Iterator(std::vector<Node*>&table, int i = -1, Node *n = 0):m_table(table), m_i(i), m_n(n){}
        Iterator& forwardUntilValidOrEnd()
        {
            assert(m_i >= 0 && m_i < (int)m_table.size());
            if (m_n != NULL) return *this;
            while (++m_i < (int)m_table.size()) {
                if ((m_n = m_table[m_i]) != NULL) return *this;
            }
            m_i = -1;
            return *this;
        }
    private:
        int m_i;
        Node *m_n;
        std::vector<Node*>& m_table;
    };
    class ConstIterator
    {
    public:
        typedef std::forward_iterator_tag iterator_category;
        typedef const T value_type;
        typedef size_t difference_type;
        typedef size_t distance_type;	// retained
        typedef const T* pointer;
        typedef const T& reference;
    public:
        const T& operator * () const
        {
            assert(m_n != NULL);
            return m_n->val;
        }
        const T* operator -> () const
        {
            assert(m_n != NULL);
            return &m_n->val;
        }
        ConstIterator& operator ++ ()
        {
            assert(m_i >= 0 && m_n != NULL);
            m_n = m_n->next;
            return forwardUntilValidOrEnd();
        }
        ConstIterator operator ++ (int)
        {
            ConstIterator i(*this);
            ++*this;
            return i;
        }
        bool operator == (const ConstIterator& o) const 
        {
            return &m_table == &o.m_table && m_i == o.m_i && m_n == o.m_n;
        } 
        bool operator != (const ConstIterator& o) const { return !(*this == o);}
    private:
        friend HashSet;
        ConstIterator(const std::vector<Node*>&table, int i = -1, const Node *n = 0):m_table(table), m_i(i), m_n(n){}
        ConstIterator& forwardUntilValidOrEnd()
        {
            assert(m_i >= 0 && m_i < (int)m_table.size());
            if (m_n != NULL) return *this;
            while (++m_i < (int)m_table.size()) {
                if ((m_n = m_table[m_i]) != NULL) return *this;
            }
            m_i = -1;
            return *this;
        }
    private:
        int m_i;
        const Node *m_n;
        const std::vector<Node*>& m_table;
    };

public:
    HashSet():
        m_size(0), m_table(4)
    {
    }

    template<typename IterT>
    HashSet(IterT begin, IterT end):
        m_size(0), m_table(4)
    {
        insert(begin, end);
    }

    HashSet(const HashSet& o):
        m_size(0), m_table(4)
    {
        *this = o;
    }

    ~HashSet()
    {
        clear();
    }

    HashSet& operator = (const HashSet& o)
    {
        HashSet t(o.begin(), o.end());
        swap(t);
        return *this;
    }

    bool operator == (const HashSet& o) const
    {
        if (size() != o.size()) return false;
        ConstIterator iter = begin();
        ConstIterator oiter = o.begin();
        while (iter != end()) {
            if (*iter != *oiter) return false;
            ++iter; ++oiter;
        }
        return true;
    }
    bool operator != (const HashSet& o) const { return !(*this == o); }

    int size() const { return m_size; }
    bool empty() const { return size() == 0; }
    std::pair<Iterator, bool> insert(const T& val)
    {
        if (size() / m_table.size() > 3) {
            rehash();
        }
        size_t h = Traits::hash(val) % m_table.size();
        Node *p = m_table[h];
        while (p != NULL && !Traits::equal(p->val, val)) p = p->next;
        if (p != NULL) {
            return std::make_pair(Iterator(m_table, (int)h, p), false);
        }
        p = malloc(val);
        p->val = val;
        p->next = m_table[h];
        m_table[h] = p;
        ++m_size;
        return std::make_pair(Iterator(m_table, (int)h, p), true);
    }
    template<typename IterT>
    void insert(IterT begin, IterT end)
    {
        while (begin != end) {
            insert(*begin++);
        }
    }
    bool contain(const T& val) const { return find(val) != end(); }
    int count(const T& val) const { return contain(val) ? 1 : 0; }
    ConstIterator find(const T& val) const
    {
        size_t h = Traits::hash(val) % m_table.size();
        Node *p = m_table[h];
        while (p != NULL && !Traits::equal(p->val, val)) p = p->next;
        if (p != NULL) return ConstIterator(m_table, (int)h, p);
        return ConstIterator(m_table);
    }
    Iterator find(const T& val)
    {
        size_t h = Traits::hash(val) % m_table.size();
        Node *p = m_table[h];
        while (p != NULL && !Traits::equal(p->val, val)) p = p->next;
        if (p != NULL) return Iterator(m_table, (int)h, p);
        return Iterator(m_table);
    }
    void erase(const T& val)
    {
        Iterator iter = find(val);
        if (iter == end()) return;
        erase(iter);
    }
    void erase(Iterator iter)
    {
        assert(iter != end());
        Node *p = m_table[iter.m_i];
        if (p == iter.m_n) {
            m_table[iter.m_i] = p->next;
            free(p);
        }
        else {
            while (p->next != iter.m_n) {
                p = p->next;
            }
            p->next = iter.m_n->next;
            free(iter.m_n);
        }
        --m_size;
    }
    void clear()
    {
        for (int i = 0; i < (int)m_table.size(); ++i) {
            Node *p = m_table[i];
            while (p != NULL) {
                Node *t = p;
                p = p->next;
                free(t);
            }
        }
        m_table.assign(4, (Node*)0);
        m_size = 0;
    }
    void swap(HashSet &o)
    {
        std::swap(m_table, o.m_table);
        std::swap(m_pool, o.m_pool);
        std::swap(m_size, o.m_size);
    }

    Iterator begin() 
    { 
        if (empty()) return end();
        return Iterator(m_table, 0, m_table[0]).forwardUntilValidOrEnd();
    }
    ConstIterator begin() const
    {
        if (empty()) return end();
        return ConstIterator(m_table, 0, m_table[0]).forwardUntilValidOrEnd();
    }
    Iterator end() { return Iterator(m_table); }
    ConstIterator end() const { return ConstIterator(m_table);}

private:
    void rehash()
    {
        assert(!empty());
        Node *head = NULL, *tail = NULL;
        Iterator iter = begin();
        head = iter.m_n;
        tail = head;
        while (++iter != end()) {
            tail->next = iter.m_n;
            tail = iter.m_n;
        }
        assert(tail->next == NULL);

        m_table.assign(m_table.size() * 3 / 2, (Node*)0);

        while (head != NULL) {
            Node *p = head;
            head = head->next;
            size_t h = Traits::hash(p->val) % m_table.size();
            p->next = m_table[h];
            m_table[h] = p;
        }
    }
    Node* malloc(const T& val)
    {
        Node* p = (Node*)m_pool.malloc();
        new (&p->val) T(val);
        return p;
    }
    void free(Node *p)
    {
        p->val.~T();
        m_pool.free(p);
    }

private:
    std::vector<Node*> m_table;
    FixSizeMemoryPool<sizeof(Node)> m_pool;
    int m_size;
};

template<typename KT, typename VT>
struct HashMapTraits
{
    static size_t hash(const std::pair<KT, VT>& val) { return hashValue(val.first); }
    static bool equal(const std::pair<KT, VT>& a, const std::pair<KT, VT>& b) { return a.first == b.first; }
};

template<typename KT, typename VT, typename Traits = HashMapTraits<KT, VT> >
class HashMap:
    public HashSet<std::pair<KT, VT>, Traits>
{
public:
    typedef std::pair<KT, VT>   ValueType;
public:
    HashMap()
    {
    }

    template<typename IterT>
    HashMap(IterT begin, IterT end):
        HashSet(begin, end)
    {
    }

    HashMap(const HashMap& o)
    {
        *this = o;
    }

    ~HashMap()
    {
    }

    HashMap& operator = (const HashMap& o)
    {
        HashMap t(o.begin(), o.end());
        swap(t);
        return *this;
    }

    bool operator == (const HashMap& o) const
    {
        return (const HashSet&)*this == (const HashSet&)o;
    }
    bool operator != (const HashMap& o) const { return !(*this == o); }

    bool contain(const KT& val) const { return find(val) != end(); }
    int count(const KT& val) const { return contain(val) ? 1 : 0; }
    ConstIterator find(const KT& val) const
    {
        ValueType v(val, VT());
        return HashSet::find(v);
    }
    Iterator find(const KT& val)
    {
        ValueType v(val, VT());
        return HashSet::find(v);
    }
    void erase(const KT& val)
    {
        ValueType v(val, VT());
        HashSet::erase(v);
    }
    void erase(Iterator iter)
    {
        HashSet::erase(iter);
    }
    void swap(HashMap &o)
    {
        HashSet::swap(o);
    }

    const VT& operator [] (const KT& k) const
    {
        ConstItertor iter = find(k);
        assert(iter != end());
        return iter->second;
    }
    VT& operator [] (const KT& k)
    {
        ValueType v(k, VT());
        return insert(v).first->second;
    }

private:
};

namespace std
{
    template<typename T, typename Traits>
    inline void swap(HashSet<T, Traits>& a, HashSet<T, Traits>& b)
    {
        a.swap(b);
    }
    template<typename KT, typename VT, typename Traits>
    inline void swap(HashMap<KT, VT, Traits>& a, HashMap<KT, VT, Traits>& b)
    {
        a.swap(b);
    }
}

#endif // #ifndef HASH_H
