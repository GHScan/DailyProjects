#ifndef STRING_MAP_H
#define STRING_MAP_H

#include "StackAllocator.h"

template<typename ValueT>
class StringMap {
public:
    struct Entry {
        ValueT value;
        char key[1];
    };

public:
    StringMap(int initBucketSize = 128): mSize(0) {
        assert(initBucketSize > 0);
        assert((initBucketSize & (initBucketSize - 1)) == 0);

        mBucketMask = initBucketSize - 1;
        mBuckets = static_cast<Node**>(::calloc(initBucketSize, sizeof(Node*)));
    }

    StringMap(const StringMap &o) = delete;
    StringMap& operator = (const StringMap &o) = delete;

    ~StringMap() {
        for (int i = 0; i <= mBucketMask; ++i) {
            for (Node *n = mBuckets[i], *next; n; n = next) {
                next = n->next;
                freeNode(n);
            }
        }

        FREE(mBuckets);
    }

    const Entry* get(const char *key, int len) const {
        uint32_t hash = hashRange(key, key + len);

        for (Node *n = mBuckets[hash & mBucketMask]; n; n = n->next) {
            if (strcmp(key, n->entry.key) == 0) return &n->entry;
        }

        return nullptr;
    }
    Entry* get(const char *key, int len) {
        return const_cast<Entry*>(const_cast<const StringMap*>(this)->get(key, len));
    }

    Entry* insert(const char *key, int len, const ValueT &value) {
        assert(get(key, len) == nullptr);

        if (mSize > mBucketMask) {
            rehash();
        }
        ++mSize;

        uint32_t hash = hashRange(key, key + len);

        Node** p = &mBuckets[hash & mBucketMask];
        return &(*p = mallocNode(key, len, value, *p))->entry;
    }

    int size() const {
        return mSize;
    }

private:
    struct Node {
        Node *next;
        Entry entry;
    };

private:
    void rehash() {
        int newBucketMask = mBucketMask * 2 + 1;
        Node **newBuckets = static_cast<Node**>(::calloc(newBucketMask + 1, sizeof(Node*)));

        for (int i = 0; i <= mBucketMask; ++i) {
            for (Node *n = mBuckets[i], *next; n; n = next) {
                next = n->next;

                char* key = n->entry.key;
                uint32_t hash = hashRange(key, key + ::strlen(key) + 1);
                Node **p = &newBuckets[hash & newBucketMask];
                n->next = *p;
                *p = n;
            }
        }

        FREE(mBuckets);
        mBucketMask = newBucketMask;
        mBuckets = newBuckets;
    }

    Node* mallocNode(const char *key, int len, const ValueT &v, Node *next) {
        Node *n = static_cast<Node*>(mAllocator.malloc(sizeof(Node) + len, alignof(Node)));
        n->next = next;
        new (&n->entry.value) ValueT(v);
        memcpy(n->entry.key, key, len + 1);
        return n;
    }

    void freeNode(Node *n) {
        n->entry.value.~ValueT();
    }

private:
    StackAllocator mAllocator;
    int mBucketMask;
    Node** mBuckets;
    int mSize;
};

#endif
