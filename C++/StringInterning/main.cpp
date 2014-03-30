#include "pch.h"

#include <set>
#include <unordered_set>
#include <memory>

#include <unistd.h>

template<int N>
class MemoryPool {
public:
    MemoryPool(): mFreeList(nullptr) {
    }
    ~MemoryPool() {
        for (char *p : mChunks) ::free(p);
    }
    MemoryPool(const MemoryPool &) = delete;
    MemoryPool& operator = (const MemoryPool &) = delete;

    void* malloc() {
        if (mFreeList == nullptr) allocBlocks();
        Block *b = mFreeList;
        mFreeList = b->next;
        return b;
    }
    void free(void *p) {
        Block *b = (Block*)p;
        b->next = mFreeList;
        mFreeList = b;
    }
private:
    struct Block {
        Block *next;
        char padding[N - sizeof(Block*)];
    };
private:
    void allocBlocks() {
        const int PAGE_SIZE = 4 * 1024;
        const int CHUNK_SIZE = max((N * 128 + PAGE_SIZE - 1) / PAGE_SIZE * PAGE_SIZE, PAGE_SIZE);
        char *p = (char*)::malloc(CHUNK_SIZE);
        for (int i = 0; i + N <= CHUNK_SIZE; i += N) {
            Block *b = (Block*)&p[i];
            b->next = mFreeList;
            mFreeList = b;
        }
        mChunks.push_back(p);
    }
private:
    vector<char*> mChunks;
    Block *mFreeList;
}; 

class OWordStream {
public:
    OWordStream(): mChunks(nullptr) {
        allocChunk(1024);
    }
    ~OWordStream() {
        for (Chunk *next; mChunks; mChunks = next) {
            next = mChunks->next;
            free(mChunks);
        }
    }
    OWordStream(const OWordStream&) = delete;
    OWordStream& operator = (const OWordStream&) = delete;

    const char* append(const char *begin, const char *end) {
        int count = end - begin;
        if (count > mChunks->dataLen - mChunks->payload) {
            allocChunk(count);
        } 
        char *p = mChunks->data + mChunks->payload;
        memcpy(p, begin, count);
        mChunks->payload += count;
        return p;
    }
private:
    struct Chunk {
        Chunk *next;
        int payload;
        int dataLen;
        char data[1];
    };
private:
    void allocChunk(int size) {
        const int PAGE_SIZE = 4 * 1024;
        size = ((size + sizeof(Chunk) - 1) + PAGE_SIZE - 1) / PAGE_SIZE * PAGE_SIZE;

        Chunk *b = (Chunk*)malloc(size);
        b->next = mChunks;
        b->payload = 0;
        b->dataLen = size - (sizeof(Chunk) - 1);
        mChunks = b;
    }
private:
    Chunk *mChunks;
};

template<typename ItT>
static size_t rangeHash(ItT begin, ItT end) {
    size_t seed = 0;
    for (; begin != end; ++begin) {
        auto v = *begin;
        seed ^= std::hash<decltype(v)>()(*begin) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    }
    return seed;
}

template<typename KT, typename HashT = hash<KT>, typename EqualT = equal_to<KT>, int REHASH_THRESHOLD = 2>
class OpenListHashTable {
private:
    struct Node {
        KT key;
        Node *next;
        Node(const KT &_key, Node *_next): key(_key), next(_next){}
    };
public:
    OpenListHashTable(): mSize(0) {
        mBucketSize = 7;
        mBuckets = (Node**)::calloc(mBucketSize, sizeof(Node*));
    }
    ~OpenListHashTable() {
        for (int i = 0; i < mBucketSize; ++i) {
            for (Node *n = mBuckets[i], *next; n != nullptr; n = next) {
                next = n->next;
                freeNode(n);
            }
        }
        ::free(mBuckets);
    }

    KT* insert(const KT &k) {
        if (mSize / mBucketSize > REHASH_THRESHOLD) rehash();

        size_t i = mHash(k) % mBucketSize;
        for (Node *n = mBuckets[i], *next; n != nullptr; n = next) {
            next = n->next;
            if (mEqual(n->key, k)) return &n->key;
        }

        ++mSize;
        Node *n = allocNode(k, mBuckets[i]);
        return &(mBuckets[i] = n)->key;
    }
    KT* get(const KT &k) {
        for (Node *n = mBuckets[mHash(k) % mBucketSize], *next; n != nullptr; n = next) {
            next = n->next;
            if (mEqual(n->key, k)) return &n->key;
        }
        return nullptr;
    }
private:
    void rehash() {
        int newBucketSize = mBucketSize * 3 + 1;
        Node** newBuckets = (Node**)calloc(newBucketSize, sizeof(Node*));

        for (int i = 0; i < mBucketSize; ++i) {
            for (Node *n = mBuckets[i], *next; n != nullptr; n = next) {
                next = n->next;
                Node ** p = newBuckets + (mHash(n->key) % newBucketSize);
                n->next = *p;
                *p = n;
            }
        }

        ::free(mBuckets);
        mBucketSize = newBucketSize;
        mBuckets = newBuckets;
    }
    Node* allocNode(const KT &k, Node *next) {
        return new (mPool.malloc()) Node(k, next);
    }
    void freeNode(Node *p) {
        p->~Node();
        mPool.free(p);
    }
private:
    Node **mBuckets;
    int mBucketSize;
    int mSize;
    MemoryPool<sizeof(Node)> mPool;
    HashT mHash;
    EqualT mEqual;
};

//////////////////////////////

struct InterningString {
    const char *str;
    int len;
};

struct InterningStringHash {
    size_t operator () (const InterningString &s) const {
        return hash<const char *>()(s.str);
    }
};
struct InterningStringEqual {
    bool operator () (const InterningString &a, const InterningString &b) const {
        return a.str == b.str;
    }
};

struct InterningStringHash_Str {
    size_t operator () (const InterningString &s) const {
        return rangeHash(s.str, s.str + s.len);
    }
};
struct InterningStringEqual_Str {
    bool operator () (const InterningString &a, const InterningString &b) const {
        if (a.len != b.len) return false;
        return memcmp(a.str, b.str, a.len) == 0;
    }
};
//////////////////////////////
class StringPool_Empty {
public:
    InterningString intern(const char* begin, const char *end) {
        return {begin, end - begin};
    }
};

class StringPool_Set {
public:
    InterningString intern(const char* begin, const char *end) {
        const string &s = *mSet.insert(string(begin, end)).first;
        return {s.c_str(), (int)s.size()};
    }
private:
    set<string> mSet;
};

class StringPool_Hash {
public:
    InterningString intern(const char* begin, const char *end) {
        const string &s = *mSet.insert(string(begin, end)).first;
        return {s.c_str(), (int)s.size()};
    }
private:
    unordered_set<string> mSet;
};

class StringPool_OStream {
public:
    InterningString intern(const char* begin, const char *end) {
        InterningString s = {begin, end - begin};
        auto it = mSet.find(s);
        if (it != mSet.end()) {
            return *it;
        } else {
            s.str = mOS.append(begin, end + 1);
            return *mSet.insert(s).first;
        }
    }
private:
    OWordStream mOS;
    unordered_set<InterningString, InterningStringHash_Str, InterningStringEqual_Str> mSet;
};

class StringPool_OStreamOpenListHash {
public:
    InterningString intern(const char* begin, const char *end) {
        InterningString s = {begin, end - begin};
        if (InterningString *p = mSet.get(s)) return *p;
        else {
            s.str = mOS.append(begin, end + 1);
            return *mSet.insert(s);
        }
    }
private:
    OWordStream mOS;
    OpenListHashTable<InterningString, InterningStringHash_Str, InterningStringEqual_Str> mSet;
};

template<typename StringPoolT>
static void go() {
    StringPoolT pool;
    {
        clock_t start = clock();
        for (string word; cin >> word;) pool.intern(word.c_str(), word.c_str() + word.size());
        printf(": %fs\n", (clock() - start) / float(CLOCKS_PER_SEC));
    }
    pause();
}
#define GO(type) printf(#type); go<type>()

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage : %s 0/1/2/3\n", argv[0]);
        return 1;
    }

    switch (atoi(argv[1])) {
        case 0: GO(StringPool_Empty); break;
        case 1: GO(StringPool_Set); break;
        case 2: GO(StringPool_Hash); break;
        case 3: GO(StringPool_OStream); break;
        case 4: GO(StringPool_OStreamOpenListHash); break;
        default: break;
    }
}
