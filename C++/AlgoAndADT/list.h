
#include "pch.h"

#include <initializer_list>
#include <functional>

// these class is a skill show about 2-order pointer usage and list node iteration

template<typename T>
class List {
private:
    struct Node {
        Node *next;
        T value;
        Node(const T &val, Node *p = nullptr): next(p), value(val){}
    };
public:
    List(): mHead(nullptr) {
    }
    List(const List &o): mHead(nullptr) {
        Node **p = &mHead;
        for (Node *po = o.mHead; po != nullptr; po = po->next) {
            *p = new Node(po->value);
            p = &(*p)->next;
        }
    }
    List& operator = (const List &o) {
        List t(o);
        swap(t);
        return *this;
    }
    ~List() {
        clear();
    }

    List(List &&o): mHead(nullptr) {
        swap(o);
    }
    List& operator = (List &&o) {
        swap(o);
        return *this;
    }
    List(initializer_list<T> &&o): mHead(nullptr) {
        Node **p = &mHead;
        for (const T &i : o) {
            *p = new Node(i);
            p = &(*p)->next;
        }
    }

    void clear() {
        for (Node *p = mHead; p != nullptr; ) {
            Node *n = p->next;
            delete p;
            p = n;
        }
        mHead = nullptr;
    }
    void pushFront(const T &val) {
        mHead = new Node(val, mHead);
    }
    void remove(const T &val) {
        Node **p = &mHead;
        while (*p) {
            if ((*p)->value == val) {
                Node *t = *p;
                *p = t->next;
                delete t;
            } else p = &(*p)->next;
        }
    }
    void reverse() {
        Node *p = mHead;
        mHead = nullptr;
        while (p != nullptr) {
            Node *n = p->next;
            p->next = mHead;
            mHead = p;
            p = n;
        }
    }
    void sort() {
        vector<Node*> nodes;
        for (Node *p = mHead; p != nullptr; p = p->next) {
            nodes.push_back(p);
        }
        std::sort(nodes.begin(), nodes.end(), [](Node *a, Node *b){ return a->value > b->value; });

        mHead = nullptr;
        for (Node *p : nodes) {
            p->next = mHead;
            mHead = p;
        }
    }
    void foreach(function<void(T& val)> f) {
        for (Node *p = mHead; p != nullptr; p = p->next) {
            f(p->value);
        }
    }
    void swap(List &o) {
        swap(mHead, o.mHead);
    }
private:
    Node *mHead;
};

template<typename T>
class OrderedList {
private:
    struct Node {
        Node *next;
        T value;
        Node(const T &val, Node *p = nullptr): next(p), value(val){}
    };
public:
    OrderedList(): mHead(nullptr){}
    ~OrderedList(){ clear(); }
    OrderedList(const OrderedList& o): mHead(nullptr) {
        Node **p = &mHead;
        for (Node *po = o.mHead; po != nullptr; po = po->next) {
            *p = new Node(po->value);
            p = &(*p)->next;
        }
    }
    OrderedList& operator = (const OrderedList& o) {
        OrderedList t(o);
        swap(t);
        return *this;
    }

    OrderedList(initializer_list<T> &&o): mHead(nullptr) {
        for (const T& val : o) add(val);
    }
    OrderedList(OrderedList&& o): mHead(nullptr) {
        swap(o);
    }
    OrderedList& operator = (OrderedList&& o) {
        swap(o);
        return *this;
    }

    void clear() {
        for (Node *p = mHead; p != nullptr; ) {
            Node *n = p->next;
            delete p;
            p = n;
        }
        mHead = nullptr;
    }
    void add(const T& val) {
        Node **p = &mHead;
        while (*p) {
            if ((*p)->value == val) return;
            else if ((*p)->value > val) {
                *p = new Node(val, *p);
                return;
            } else p = &(*p)->next;
        }
        *p = new Node(val);
    }
    void remove(const T &val) {
        Node **p = &mHead;
        while (*p) {
            if ((*p)->value == val) {
                Node *t = *p;
                *p = t->next;
                delete t;
                break;
            } else p = &(*p)->next;
        }
    }
    void foreach(function<void(const T& val)> f) {
        for (Node *p = mHead; p != nullptr; p = p->next) {
            f(p->value);
        }
    }
    void swap(OrderedList& o) {
        swap(mHead, o.mHead);
    }
private:
private:
    Node *mHead;
};

static void testList() {
    List<int> l = {3, 5, 1, 4, 2, 8, 9};
    l.foreach([](int i){ cout << i << ',';}); puts("");
    l.pushFront(11);
    l.remove(8);
    l.foreach([](int i){ cout << i << ',';}); puts("");
    l.sort();
    l.foreach([](int i){ cout << i << ',';}); puts("");
    l.reverse();
    l.foreach([](int i){ cout << i << ',';}); puts("");
    l.clear();
    l.foreach([](int i){ cout << i << ',';}); puts("");
}

static void testOrderedList() {
    OrderedList<int> l = {3, 5, 1, 4, 2, 8, 10, 9, 11};
    l.foreach([](int i){ cout << i << ',';}); puts("");
    l.add(5);
    l.add(5);
    l.foreach([](int i){ cout << i << ',';}); puts("");
    l.remove(5);
    l.foreach([](int i){ cout << i << ',';}); puts("");
    l.clear();
    l.foreach([](int i){ cout << i << ',';}); puts("");
}

int main() {
    testList();
    testOrderedList();
    puts("finish");
}
