#include "pch.h"

#include "utils.h"
#include "hashTable.h"

static const char *TYPE = "HashTable_T";

typedef struct Node_T {
    const void *key;
    void *value;
    struct Node_T *next;
} Node_T;

typedef struct HashTable_T {
    const char *type;
    int size;
    int bucketSize;
    Node_T **buckets;
    HashTable_Hash hash;
    HashTable_Equal equal;
} HashTable_T;

static Node_T*
createNode(const void *k, void *v, Node_T *next) {
    Node_T *n = malloc(sizeof(Node_T));
    n->key = k;
    n->value = v;
    n->next = next;
    return n;
}

static Node_T*
findValidNode(HashTable_T *o, int startBucket) {
    for (int i = startBucket; i < o->bucketSize; ++i) {
        if (o->buckets[i]) return o->buckets[i];
    }
    return 0;
}

static void 
rehash(HashTable_T *o) {
    int newBucketSize = o->bucketSize * (o->bucketSize < 512 ?  4 : 2);
    Node_T **newBuckets = calloc(newBucketSize, sizeof(Node_T*));

    for (int i = 0; i < o->bucketSize; ++i) {
        for (Node_T *n = o->buckets[i], *next; n; n = next) {
            next = n->next;
            Node_T **p = &newBuckets[o->hash(n->key) % newBucketSize];
            n->next = *p;
            *p = n;
        }
    }

    free(o->buckets);
    o->buckets = newBuckets;
    o->bucketSize = newBucketSize;
}

HashTable_T*        
HashTable_create(HashTable_Hash hash, HashTable_Equal equal) {
    assert(hash && equal);

    HashTable_T *o = malloc(sizeof(HashTable_T));
    o->type = TYPE;
    o->size = 0;
    o->hash = hash;
    o->equal = equal;
    o->bucketSize = 4;
    o->buckets = calloc(o->bucketSize, sizeof(Node_T*));

    assert(o);
    return o;
}

void                
HashTable_destroy(HashTable_T **p) {
    assert(p && *p && (*p)->type == TYPE);

    HashTable_T *o = *p;
    for (int i = 0; i < o->bucketSize; ++i) {
        for (Node_T *n = o->buckets[i], *next; n; n = next) {
            next = n->next;
            free(n);
        }
    }
    free(o->buckets);
    free(o);

    *p = 0;
}

void                
HashTable_insert(HashTable_T *o, void *k, void *v) {
    assert(o && o->type == TYPE);
    assert(HashTable_get(o, k) == 0);

    if ((double)o->size / o->bucketSize > 1.5) rehash(o);

    Node_T **p = &o->buckets[o->hash(k) % o->bucketSize];
    *p = createNode(k, v, *p);
    ++o->size;
}

int                
HashTable_remove(HashTable_T *o, void *k) {
    assert(o && o->type == TYPE);

    Node_T **p = &o->buckets[o->hash(k) % o->bucketSize];
    for (; *p && o->equal((*p)->key, k) == 0; p = &(*p)->next);

    if (*p == 0) return 0;

    Node_T *n = *p;
    *p = n->next;
    free(n);

    --o->size;
    return 1;
}

void**
HashTable_get(HashTable_T *o, void *k) {
    assert(o && o->type == TYPE);

    Node_T *n = o->buckets[o->hash(k) % o->bucketSize];
    for (; n && o->equal(n->key, k) == 0; n = n->next);

    return n == 0 ? 0 : &n->value;
}

int                 
HashTable_size(HashTable_T *o) {
    assert(o && o->type == TYPE);

    return o->size;
}

HashTableIter_T*    
HashTable_begin(HashTable_T *o) {
    assert(o && o->type == TYPE);

    return (HashTableIter_T*)findValidNode(o, 0);
}

HashTableIter_T*    
HashTable_end(HashTable_T *o) {
    assert(o && o->type == TYPE);

    return 0;
}

HashTableIter_T*    
HashTable_next(HashTable_T *o, HashTableIter_T *it) {
    assert(o && o->type == TYPE);
    assert(it);

    Node_T *n = (Node_T*)it;
    return (HashTableIter_T*)(n->next ? n->next : findValidNode(o, o->hash(n->key) % o->bucketSize + 1));
}

unsigned int        
HashTable_hashBuf(const void* _buf, int size) {
    assert(_buf);

    const unsigned char *buf = (const unsigned char*)_buf;
    unsigned int basis = 2166136261, prime = 16777619;
    unsigned int ret = basis;
    for (int i = 0; i < size; ++i) {
        ret ^= buf[i];
        ret *= prime;
    }
    return ret;
}
