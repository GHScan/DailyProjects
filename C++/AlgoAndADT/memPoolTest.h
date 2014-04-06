#include "pch.h"

#include "Utils.h"

template<typename T>
struct NodeT {
    T value;
    NodeT *next;
    NodeT(const T& _value, NodeT *_next): value(_value), next(_next){}
};

template<typename T>
struct LinkedListSortAlgo {
    typedef NodeT<T> Node;

    virtual ~LinkedListSortAlgo() {}
    virtual const char *getName() = 0;
    virtual int getLimit() = 0;
    virtual void sort(Node *begin, Node *end) = 0;
    virtual void beginAlloc(int n) {}
    virtual void endAlloc() {}
    virtual Node* allocNode(int value, Node *next) { return new Node(value, next); }
    virtual void freeNode(Node *n) { delete n; }

    void assertSorted(Node *n) {
        for (; n->next != nullptr; n = n->next) {
            if (n->value > n->next->value) {
                printf("sort failed! => %s\n", getName());
                assert(0);
            }
        }
    }

    void run(T *begin, T *end) {
        printf("%s: len=%.3fK\n", getName(), (end - begin) / 1024.0);

        Node *head = nullptr;
        {
            double time = getTime();
            beginAlloc(end - begin);
            for (T *p = end - 1; p >= begin; --p) {
                head = allocNode(*p, head);
            }
            printf("\tcreate list: %fs\n", getTime() - time);
        }

        {
            double time = getTime();
            sort(head, nullptr);
            time = getTime() - time;
            printf("\tsort : %fs, TPE=%fns\n", time, time * 1000000000 / (end - begin));
        }
        assertSorted(head); 
        {
            double time = getTime();
            for (Node *next; head != nullptr; head = next) {
                next = head->next;
                freeNode(head);
            }
            endAlloc();
            printf("\tdestroy list: %fs\n", getTime() - time);
        }
    }
};

template<typename T>
class LinkedListSortAlgo_selectionSort: public LinkedListSortAlgo<T> {
public:
    using typename LinkedListSortAlgo<T>::Node;

    virtual const char *getName() { return "selectionSort"; }
    virtual int getLimit() { return 16 * 1024; }
    virtual void sort(Node *begin, Node *end) {
        for (Node *p = begin; p != end; p = p->next) {
            for (Node *q = p->next; q != end; q = q->next) {
                if (q->value < p->value) swap(p->value, q->value);
            }
        }
    }
};
template<typename T>
class LinkedListSortAlgo_selectionSortPool: public LinkedListSortAlgo_selectionSort<T> {
public:
    using typename LinkedListSortAlgo_selectionSort<T>::Node;

    virtual const char *getName() { return "selectionSortPool"; }
    virtual Node* allocNode(int value, Node *next) {
        return new (mPoolFree++) Node(value, next);
    }
    virtual void freeNode(Node *n) {
    }
    virtual void beginAlloc(int n) {
        mPool = (Node*)malloc(sizeof(Node) * n);
        mPoolFree = mPool;
    }
    virtual void endAlloc() {
        free(mPool);
    }
private:
    Node *mPool;
    Node *mPoolFree;
};

template<typename T>
class LinkedListSortAlgo_quickSort: public LinkedListSortAlgo<T> {
public:
    using typename LinkedListSortAlgo<T>::Node;

    virtual const char *getName() { return "quickSort"; }
    virtual int getLimit() { return 1024 * 1024; }
    virtual void sort(Node *begin, Node *end) {
        if (begin == end || begin->next == end) return;

        Node *mid = begin;
        for (Node *p = begin->next; p != end; p = p->next) {
            if (p->value <= begin->value) {
                mid = mid->next;
                swap(p->value, mid->value);
            }
        }
        swap(begin->value, mid->value);
        
        sort(begin, mid);
        sort(mid->next, end);
    }
};

template<typename T>
class LinkedListSortAlgo_quickSortPool: public LinkedListSortAlgo_quickSort<T> {
public:
    using typename LinkedListSortAlgo_quickSort<T>::Node;

    virtual const char *getName() { return "quickSortPool"; }
    virtual Node* allocNode(int value, Node *next) {
        return new (mPoolFree++) Node(value, next);
    }
    virtual void freeNode(Node *n) {
    }
    virtual void beginAlloc(int n) {
        mPool = (Node*)malloc(sizeof(Node) * n);
        mPoolFree = mPool;
    }
    virtual void endAlloc() {
        free(mPool);
    }
private:
    Node *mPool;
    Node *mPoolFree;
};

static void benchmark(vector<LinkedListSortAlgo<int>*> algos) {
    int lens[] = {1, 5, 13, 19, 33, 65, 128, 255, 512, 1024, 2 * 1024, 4 * 1024, 8 * 1024, 16 * 1024, 32 * 1024, 64 * 1024, 128 * 1024, 256 * 1024, 512 * 1024, 1024*1024};
    for (int len : lens) {
        vector<int> data(len);
        for (int &d : data) d = myrand(len);

        vector<int> tdata(data);
        for (auto algo : algos) {
            if (len > algo->getLimit()) continue;
            tdata = data;
            algo->run(&tdata[0], &tdata[0] + tdata.size());
        }
    }
}

int main() {
    srand(time(nullptr));
    setCpuAffinity(1);

    vector<LinkedListSortAlgo<int>*> algos = {
        new LinkedListSortAlgo_selectionSort<int>(),
        new LinkedListSortAlgo_selectionSortPool<int>(),
        new LinkedListSortAlgo_quickSort<int>(),
        new LinkedListSortAlgo_quickSortPool<int>(),
    };
    benchmark(algos);
    for (auto p : algos) delete p;
}
