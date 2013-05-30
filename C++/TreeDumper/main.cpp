#include "pch.h"

struct Node {
    int key, value;
    Node *parent, *lchild, *rchild;
    Node(int _key, int _value): key(_key), value(_value), parent(NULL), lchild(NULL), rchild(NULL){}
};
Node* insert(Node *node, int key, int value) {
    if (node == NULL) return new Node(key, value);
    else {
        if (key == node->key) {
            node->value = value;
        } else if (key < node->key) {
            node->lchild = insert(node->lchild, key, value);
            node->lchild->parent = node;
        } else {
            node->rchild = insert(node->rchild, key, value);
            node->rchild->parent = node;
        }
        return node;
    }
}
int search(Node *node, int key) {
    if (node == NULL) return -1;
    if (node->key == key) return node->value;
    else if (key < node->key) return search(node->lchild, key);
    else return search(node->rchild, key);
}
void dump(Node *node, int depth = 0) {
    if (node == NULL) return;
    dump(node->lchild, depth + 1);
    for (int i = 0; i < depth; ++i) printf("   ");
    printf("(%d=%d)\n", node->key, node->value);
    dump(node->rchild, depth + 1);
}
void destroy(Node *n) {
    if (n == NULL) return;
    destroy(n->lchild);
    destroy(n->rchild);
    delete n;
}
Node* minimum(Node *n) {
    while (n->lchild != NULL) n = n->lchild;
    return n;
}
Node* next(Node *n) {
    if (n->rchild != NULL) {
        return minimum(n->rchild);
    } else {
        while (n->parent != NULL && n->parent->rchild == n) n = n->parent;
        return n->parent;
    }
}
void traverse(Node *n) {
    for (n = minimum(n); n != NULL; n = next(n)) {
        printf("(%d,%d),", n->key, n->value);
    }
    puts("");
}

int main() {
    int numbers[] = {3, 5, 1, 4, 2, 8, 9, 7};
    Node *n = NULL;
    for (int i = 0; i < int(sizeof(numbers) / sizeof(numbers[0])); ++i) {
        n = insert(n, numbers[i], numbers[i] * numbers[i]);
    }
    dump(n);
    traverse(n);
    destroy(n);
}
