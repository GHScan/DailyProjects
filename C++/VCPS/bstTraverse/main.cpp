#include "pch.h"

#include <time.h>

#include <functional>
//------------------------------
// ignore the memory leaks issue for cpp

//------------------------------
// adt for binary tree

struct Node {
    int value;
    Node *left, *right;
};
static Node* insert(Node *n, int value) {
    if (!n) {
        n = new Node{value, nullptr, nullptr};
    } else if (value < n->value) {
        n->left = insert(n->left, value);
    } else if (value > n->value) {
        n->right = insert(n->right, value);
    }
    return n;
}

//------------------------------
// variant cps

typedef function<void*(int*)> Continue;
typedef pair<int*, Continue> ValueContinue;
static ValueContinue* traverse(Node *n, Continue k) {
    if (n) {
        return traverse(n->left, [n, k](int *_) -> void* {
            return new ValueContinue(&n->value, [n, k](int *_) -> void* {
                return traverse(n->right, k);
            });
        });
    } else {
        return (ValueContinue*)k(nullptr);
    }
}

//------------------------------
// adapter for cpp

class Iter {
    public:
        Iter(ValueContinue* vc): mVc(vc){}
        int& operator * () { return *mVc->first; }
        Iter& operator ++ () {
            mVc = (ValueContinue*)mVc->second(mVc->first);
            return *this;
        }
        Iter operator ++ (int) {
            Iter old = *this;
            ++*this;
            return old;
        }
        bool operator == (const Iter &o) const {
            return mVc->first == o.mVc->first;
        }
        bool operator != (const Iter &o) const { return !(*this == o);}
    private:
        ValueContinue *mVc;
};

static Iter begin(Node *root) {
    return Iter(traverse(root, [](int *_) ->void* { return new ValueContinue(nullptr, nullptr); }));
}
static Iter end(Node *root) {
    return Iter(new ValueContinue(nullptr, nullptr));
}

//------------------------------

int main() {
    srand(time(nullptr));

    Node *root = nullptr;
    for (int i = 0; i < 10; ++i) {
        root = insert(root, rand() % 32);
    }
    for (int v : root) {
        cout << v << endl;
    }
}

