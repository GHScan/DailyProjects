#include "pch.h"

#include <map>
#include <unordered_map>
#include <functional>

#include "Utils.h"

template<typename KT, typename VT, typename HashT = hash<KT>, typename EqualT = equal_to<KT>>
class ChainingHashTable {
private:
    struct Node {
        Node *next;
        KT key;
        VT value;
        Node(const KT &k, const VT &v, Node *_next): next(_next), key(k), value(v){}
    };
public:
    ChainingHashTable(int bucketSize = 8): mBuckets(bucketSize, nullptr), mSize(0), mLoadFactor(1) {
    }
    ~ChainingHashTable() {
        clear();
    }

    ChainingHashTable(const ChainingHashTable& o): ChainingHashTable(o.mBuckets.size()) {
        o.foreach([this](const KT &k, VT &v){ insert(k, v); });
    }
    ChainingHashTable& operator = (const ChainingHashTable& o) {
        if (this != &o) {
            ChainingHashTable tmp(o);
            swap(tmp);
        }
        return *this;
    }
    ChainingHashTable(ChainingHashTable&& o): ChainingHashTable(0) {
        swap(o);
    }
    ChainingHashTable& operator = (ChainingHashTable&& o) {
        swap(o);
        return *this;
    }

    void clear() {
        for (Node *n : mBuckets) {
            for (Node *next; n != nullptr; n = next) {
                next = n->next;
                delete n;
            }
        }
        mBuckets.assign(mBuckets.size(), nullptr);
        mSize = 0;
    }
    int size() const { return mSize; }
    bool empty() const { return size() == 0; }
    void setLoadFactor(float loadFactor) { mLoadFactor = loadFactor; }
    void swap(ChainingHashTable& o) {
        std::swap(mBuckets, o.mBuckets);
        std::swap(mSize, o.mSize);
        std::swap(mLoadFactor, o.mLoadFactor);
    }

    bool insert(const KT &k, const VT &v) {
        if (mSize > mBuckets.size() * mLoadFactor) rehash();
        Node **p = &mBuckets[mHash(k) % mBuckets.size()];
        for (Node *n = *p; n != nullptr; n = n->next) {
            if (mEqual(n->key, k)) {
                n->value = v;
                return false;
            }
        }
        *p = new Node(k, v, *p);
        ++mSize;
        return true;
    }
    bool remove(const KT &k) {
        for (Node **p = &mBuckets[mHash(k) % mBuckets.size()]; *p != nullptr; p = &(*p)->next) {
            Node *n = *p;
            if (mEqual(n->key, k)) {
                *p = n->next;
                delete n;
                --mSize;
                return true;
            }
        }
        return false;
    }
    VT* get(const KT &k) {
        for (Node *n = mBuckets[mHash(k) % mBuckets.size()]; n != nullptr; n = n->next) {
            if (mEqual(n->key, k)) return &n->value;
        }
        return nullptr;
    }

    void foreach(function<void(const KT&, const VT&)> f) const {
        for (Node *n : mBuckets) {
            for (; n != nullptr; n = n->next) f(n->key, n->value);
        }
    }
    void foreach(function<void(const KT&, VT&)> f) {
        for (Node *n : mBuckets) {
            for (; n != nullptr; n = n->next) f(n->key, n->value);
        }
    }
private:
    void rehash() {
        vector<Node*> newBuckets(mBuckets.size() * 2, nullptr);
        for (Node *n : mBuckets) {
            for (Node *next; n != nullptr; n = next) {
                next = n->next;
                Node **p = &newBuckets[mHash(n->key) % newBuckets.size()];
                n->next = *p;
                *p = n;
            }
        }
        mBuckets.swap(newBuckets);
    }
private:
    vector<Node*> mBuckets;
    int mSize;
    float mLoadFactor;
    HashT mHash;
    EqualT mEqual;
};

template<typename KT, typename VT, typename HashT = hash<KT>, typename EqualT = equal_to<KT>>
class OpenAddressingHashTable {
private:
    struct Node {
        KT key;
        VT value;
        Node(const KT &k, const VT &v): key(k), value(v){}
    };
public:
    OpenAddressingHashTable(int bucketSize = 4): mBuckets(bucketSize, nullptr), mSize(0), mLoadFactor(0.5) {
        rehash();
    }
    ~OpenAddressingHashTable() {
        clear();
    }

    OpenAddressingHashTable(const OpenAddressingHashTable& o): mBuckets(o.mBuckets.size(), nullptr), mSize(0), mLoadFactor(o.mLoadFactor) {
        o.foreach([this](const KT &k, VT &v){ insert(k, v); });
    }
    OpenAddressingHashTable& operator = (const OpenAddressingHashTable& o) {
        if (this != &o) {
            OpenAddressingHashTable tmp(o);
            swap(tmp);
        }
        return *this;
    }
    OpenAddressingHashTable(OpenAddressingHashTable&& o): OpenAddressingHashTable(0) {
        swap(o);
    }
    OpenAddressingHashTable& operator = (OpenAddressingHashTable&& o) {
        swap(o);
        return *this;
    }

    void clear() {
        for (Node *n : mBuckets) {
            if (n) delete n;
        }
        mBuckets.assign(mBuckets.size(), nullptr);
        mSize = 0;
    }
    int size() const { return mSize; }
    bool empty() const { return size() == 0; }
    void setLoadFactor(float loadFactor) { 
        assert(loadFactor < 1);
        mLoadFactor = loadFactor; 
    }
    void swap(OpenAddressingHashTable& o) {
        std::swap(mBuckets, o.mBuckets);
        std::swap(mSize, o.mSize);
        std::swap(mLoadFactor, o.mLoadFactor);
    }

    bool insert(const KT &k, const VT &v) {
        if (mSize > mLoadFactor * mBuckets.size()) rehash();

        Node **p = findBucket(mBuckets, k);
        if (*p) {
            (*p)->value = v;
            return false;
        }
        *p = new Node(k, v);
        ++mSize;
        return true;
    }
    bool remove(const KT &k) {
        Node **p = findBucket(mBuckets, k);
        if (*p == nullptr) return false;
        delete *p;
        *p = nullptr;
        --mSize;

        for (auto i = (p - &mBuckets[0] + 1) % mBuckets.size(); mBuckets[i]; i = (i + 1) % mBuckets.size()) {
            Node *n = mBuckets[i];
            mBuckets[i] = nullptr;
            p = findBucket(mBuckets, n->key);
            assert(*p == nullptr);
            *p = n;
        }
        return true;
    }
    VT* get(const KT &k) {
        Node **p = findBucket(mBuckets, k);
        return *p == nullptr ? nullptr : &(*p)->value;
    }

    void foreach(function<void(const KT&, const VT&)> f) const {
        for (Node *n : mBuckets) {
            if (n) f(n->key, n->value);
        }
    }
    void foreach(function<void(const KT&, VT&)> f) {
        for (Node *n : mBuckets) {
            if (n) f(n->key, n->value);
        }
    }
private:
    void rehash() {
        vector<Node*> newBuckets(chooseLargerSize(mBuckets.size()), nullptr);
        for (Node *&n : mBuckets) {
            if (n == nullptr) continue;
            *findBucket(newBuckets, n->key) = n;
        }
        mBuckets.swap(newBuckets);
    }
    static int chooseLargerSize(int size) {
        static int SIZE_LIST[] = { 7, 13, 31, 61, 127, 251, 509, 1021, 2039, 4093, 8191, 16381, 32749, 65521, 131071, 262139, 524287, 1048573, 2097143, 4194301, 8388593, 16777213, 33554393, 67108859, 134217689, 268435399, 536870909, 1073741789, 2147483647, };
        int i = 0;
        for (; i < ARRAY_SIZE(SIZE_LIST) - 1 && SIZE_LIST[i] <= size; ++i);
        assert(SIZE_LIST[i] > size);
        return SIZE_LIST[i];
    }
    Node** findBucket(vector<Node*> &buckets, const KT &k) {
        auto i = mHash(k) % buckets.size();
        auto j = i;
        do {
            if (buckets[j] == nullptr || mEqual(buckets[j]->key, k)) break;
            j = (j + 1) % buckets.size();
        } while (j != i);
        return &buckets[j];
    }
private:
    vector<Node*> mBuckets;
    int mSize;
    float mLoadFactor;
    HashT mHash;
    EqualT mEqual;
};

template<typename TableT>
class STLWrapper {
public:
    typedef typename TableT::key_type KT;
    typedef typename TableT::mapped_type VT;

    void clear() {
        mTable.clear();
    }
    int size() const { return mTable.size(); }
    bool empty() const { return size() == 0; }
    void setLoadFactor(float loadFactor) { 
    }

    bool insert(const KT &k, const VT &v) {
        auto oldSize = mTable.size();
        mTable[k] = v;
        return mTable.size() > oldSize;
    }
    bool remove(const KT &k) {
        return mTable.erase(k) > 0;
    }
    VT* get(const KT &k) {
        auto it = mTable.find(k);
        return it == mTable.end() ? nullptr : &it->second;
    }

    void foreach(function<void(const KT&, const VT&)> f) const {
        for (auto &kv : mTable) f(kv.first, kv.second);
    }
    void foreach(function<void(const KT&, VT&)> f) {
        for (auto &kv : mTable) f(kv.first, kv.second);
    }
private:
    TableT mTable;
};

//////////////////////////////
template<typename TableT>
static void assertEqual(const TableT &t, const map<int, int> &m) {
    assert(t.size() == m.size());
    t.foreach([&m](int a, int b){ assert(m.find(a)->second == b); });
}

template<typename TableT>
static void correctnessTest(const char *name) {
    const int LOOP = 16 * 1024;
    const int KEY_MASK = 128 - 1;

    TableT t;
    map<int, int> m;
    for (int i = 0; i < LOOP; ++i) {
        switch (rand() % 3) {
            case 0: {   
                    int k = rand() & KEY_MASK, v = rand();
                    t.insert(k, v);
                    m[k] = v;
                    assertEqual(t, m);
                }
                break;
            case 1: {
                    int k = rand() & KEY_MASK;
                    t.remove(k);
                    m.erase(k);
                    assertEqual(t, m);
                }
                break;
            case 2: {
                    int k = rand() & KEY_MASK, v = rand();
                    if (m.count(k) > 0) {
                        m[k] = v;
                        *t.get(k) = v;
                    }
                    assertEqual(t, m);
                }
                break;
            default: assert(0); break;
        }
    }

    printf("%s Ok!\n", name);
}

template<typename TableT>
static void benchmark(const char *name) {
    printf("\n%s:\n", name);

    vector<int> randints(4 * 1024);
    for (int &i : randints) i = myrand(1024 * 1024);
    const int KEY_MASK = (64 * 1024) - 1;

    TableT t;

    {
        double start = getTime();
        for (int i = 0; i < 4 * 256; ++i) {
            for (int r : randints) {
                r += i;
                switch (r % 4) {
                    case 0: case 1: case 2:
                        t.insert(r & KEY_MASK, r);
                        break;
                    case 3: case 4:
                        t.remove(r & KEY_MASK);
                        break;
                    default: assert(0); break;
                }
            }
        }
        printf("\tbuild: %.6f\n", getTime() - start);
    }

    {
        double start = getTime();
        for (int i = 0; i < 16 * 256; ++i) {
            for (int r : randints) {
                r += i;
                t.get(r & KEY_MASK);
            }
        }
        printf("\tquery: %.6f\n", getTime() - start);
    }

    {
        double start = getTime();
        t.clear();
        printf("\tdestroy: %.6f\n", getTime() - start);
    }
}

typedef STLWrapper<map<int, int>> STLMap;
typedef STLWrapper<unordered_map<int, int>> STLHash;
typedef ChainingHashTable<int, int> ChainingHash;
typedef OpenAddressingHashTable<int, int> OpenAddressingHash;

#define CORRECTNESS_TEST(type) correctnessTest<type>(#type);
#define BENCHMARK(type) benchmark<type>(#type);

int main() {
    srand(time(nullptr));
    setCpuAffinity(1);

    CORRECTNESS_TEST(STLMap);
    CORRECTNESS_TEST(STLHash);
    CORRECTNESS_TEST(ChainingHash);
    CORRECTNESS_TEST(OpenAddressingHash);
    BENCHMARK(STLMap);
    BENCHMARK(STLHash);
    BENCHMARK(ChainingHash);
    BENCHMARK(OpenAddressingHash);
}
