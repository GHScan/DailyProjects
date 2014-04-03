#include "pch.h"

#include "utils.h"
#include "BST.h"

static const char *TYPE = "BST_T";

typedef struct Node_T {
    const void *key;
    void *value;
    struct Node_T *parent, *left, *right;
} Node_T;

typedef struct BST_T {
    const char *type;
    int size;
    BST_Cmp cmp;
    Node_T *root;
    Node_T *guard;
} BST_T;

static Node_T*
createNode(void *key, void *value, Node_T *parent, Node_T *left, Node_T *right) {
    Node_T *n = malloc(sizeof(Node_T));
    n->key = key;
    n->value = value;
    n->parent = parent;
    n->left = left;
    n->right = right;
    return n;
}

static void 
destroyNodes(BST_T *o, Node_T *n) {
    if (n == o->guard) return;
    if (n->left) destroyNodes(o, n->left);
    if (n->right) destroyNodes(o, n->right);
    free(n);
}

static Node_T*
minNode(BST_T *o, Node_T *n) {
    assert(n != o->guard);
    for (; n->left != o->guard; n = n->left);
    return n;
}

static Node_T*
nextNode(BST_T *o, Node_T *n) {
    assert(n != o->guard);
    if (n->right != o->guard) return minNode(o, n->right);
    while (n->parent != o->guard && n->parent->right == n) n = n->parent;
    return n->parent; 
}

BST_T*          
BST_create(BST_Cmp cmp) {
    assert(cmp);

    BST_T *o = malloc(sizeof(BST_T));
    o->type = TYPE;
    o->size = 0;
    o->cmp = cmp;

    Node_T *n = createNode(0, 0, 0, 0, 0);
    n->parent = n->left = n->right = n;
    o->guard = o->root = n;

    return o;
}

void            
BST_destroy(BST_T **p) {
    assert(p && *p && (*p)->type == TYPE);

    BST_T *o = *p;
    destroyNodes(o, o->root);
    free(o->guard);
    free(o);

    *p = 0;
}

void            
BST_insert(BST_T *o, void *k, void *v) {
    assert(o && o->type == TYPE);

    o->guard->key = k;

    Node_T **p = &o->root, *parent = o->guard;
    for (;;) {
        int cmp = o->cmp(k, (*p)->key);
        if (cmp == 0) break;
        parent = *p;
        if (cmp < 0) p = &parent->left;
        else p = &parent->right;
    }

    if (*p == o->guard) {
        *p = createNode(k, v, parent, o->guard, o->guard);
        ++o->size;
    } else {
        assert(0 && "k should not in BST");
    }
}

int            
BST_remove(BST_T *o, void *k) {
    assert(o && o->type == TYPE);

    o->guard->key = k;

    Node_T **p = &o->root;
    for (;;) {
        int cmp = o->cmp(k, (*p)->key);
        if (cmp == 0) break;
        if (cmp < 0) p = &(*p)->left;
        else p = &(*p)->right;
    }

    if (*p == o->guard) return 0;

    Node_T *n = *p;
    if (n->left != o->guard && n->right != o->guard) {
        Node_T *min = minNode(o, n->right);
        n->key = min->key;
        n->value = min->value;
        if (min->parent->left == min) min->parent->left = min->right;
        else min->parent->right = min->right;
        if (min->right) min->right->parent = min->parent;
        n = min;
    } else {
        Node_T *child = n->left == o->guard ? n->right : n->left;
        *p = child;
        if (child) child->parent = n->parent;
    }

    free(n);
    --o->size;
    return 1;
}

void**           
BST_get(BST_T *o, void *k) {
    assert(o && o->type == TYPE);

    o->guard->key = k;

    Node_T *n = o->root;
    for (;;) {
        int cmp = o->cmp(k, n->key);
        if (cmp == 0) break;
        if (cmp < 0) n = n->left;
        else n = n->right;
    }

    return n == o->guard ? 0 : &n->value;
}

int             
BST_size(BST_T *o) {
    assert(o && o->type == TYPE);

    return o->size;
}

BSTIter_T*      
BST_begin(BST_T *o) {
    assert(o && o->type == TYPE);

    return (BSTIter_T*)(o->root == o->guard ? o->root : minNode(o, o->root));
}

BSTIter_T*      
BST_end(BST_T *o) {
    assert(o && o->type == TYPE);

    return (BSTIter_T*)o->guard;
}

BSTIter_T*      
BST_next(BST_T *o, BSTIter_T *it) {
    assert(o && o->type == TYPE);

    return (BSTIter_T*)nextNode(o, (Node_T*)it);
}
