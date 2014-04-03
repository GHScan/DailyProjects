#ifndef HASH_TABLE_H
#define HASH_TABLE_H

typedef struct HashTable_T HashTable_T;
typedef struct HashTableIter_T {
    const void *key;
    void *value;
} HashTableIter_T;
typedef unsigned int (*HashTable_Hash)(const void *k);
typedef int (*HashTable_Equal)(const void *a, const void *b);

HashTable_T*        HashTable_create(HashTable_Hash hash, HashTable_Equal equal);
void                HashTable_destroy(HashTable_T **o);
void                HashTable_insert(HashTable_T *o, void *k, void *v);
int                 HashTable_remove(HashTable_T *o, void *k);
void**              HashTable_get(HashTable_T *o, void *k);
int                 HashTable_size(HashTable_T *o);
HashTableIter_T*    HashTable_begin(HashTable_T *o);
HashTableIter_T*    HashTable_end(HashTable_T *o);
HashTableIter_T*    HashTable_next(HashTable_T *o, HashTableIter_T *it);

unsigned int        HashTable_hashBuf(const void* buf, int size);

#endif
