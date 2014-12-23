#include "stdafx.h"

#include <sstream>
#include <vector>
#include <map>
#include <unordered_map>

static inline int roundToPow2(int i) {
    return 1 << (int)ceil(log(i) / log(2));
}

template<typename KeyT, typename ValueT, typename HashT = std::hash<KeyT>, typename EqualT = std::equal_to<KeyT>>
class HashTable {
public:
    HashTable(int capacity = 8) : mSize(0), mFree(0), mBuckets(roundToPow2(capacity), -1), mNodes(roundToPow2(capacity)) {
        mMask = mBuckets.size() - 1;
        for (int i = 0; i < (int)mNodes.size(); ++i) mNodes[i].next = i + 1;
        mNodes.back().next = -1;
    }
    HashTable(HashTable const& o): HashTable(o.capacity()) {
        o.foreach([](KeyT const &key, Value const &value){ insert(key, value); });
    }
    HashTable& operator = (HashTable const& o) {
        HashTable temp(o);
        swap(temp);
        return *this;
    }
    HashTable(HashTable && o): mSize(0), mFree(-1) {
        swap(o);
    }
    HashTable& operator = (HashTable && o) {
        swap(o);
        return *this;
    }

    void swap(HashTable &o) {
        std::swap(mNodes, o.mNodes);
        std::swap(mBuckets, o.mBuckets);
        std::swap(mFree, o.mFree);
        std::swap(mSize, o.mSize);
        std::swap(mMask, o.mMask);
    }


    ValueT const* lookup(KeyT const &key) const {
        for (int nindex = mBuckets[mHash(key) & mMask]; nindex != -1; nindex = mNodes[nindex].next) {
            if (mEqual(mNodes[nindex].key, key)) return &mNodes[nindex].value;
        }
        return nullptr;
    }
    ValueT* lookup(KeyT const &key) {
        return const_cast<ValueT*>(const_cast<HashTable const*>(this)->lookup(key));
    }
    bool insert(KeyT const &key, ValueT const &value) {
        if (mFree == -1) rehash();

        int i = mHash(key) & mMask;
        for (int nindex = mBuckets[i]; nindex != -1; nindex = mNodes[nindex].next) {
            if (mEqual(mNodes[nindex].key, key)) {
                mNodes[nindex].value = value;
                return false;
            }
        }

        ++mSize;
        int newIndex = mFree;
        mFree = mNodes[mFree].next;
        mNodes[newIndex].key = key;
        mNodes[newIndex].value = value;
        mNodes[newIndex].next = mBuckets[i];
        mBuckets[i] = newIndex;
        return true;
    }
    bool remove(KeyT const &key) {
        int *p = &mBuckets[mHash(key) & mMask];
        for (; *p != -1 && !mEqual(mNodes[*p].key, key); p = &mNodes[*p].next);
        if (*p == -1) return false;

        --mSize;
        int toFree = *p;
        *p = mNodes[toFree].next;
        mNodes[toFree].next = mFree;
        mFree = toFree;
        return true;
    }
    void clear() {
        mSize = 0;
        mFree = 0;
        for (int i = 0; i < (int)mNodes.size(); ++i) mNodes[i].next = i + 1;
        mNodes.back().next = -1;
    }
    bool empty() const {
        return size() == 0;
    }
    int size() const {
        return mSize;
    }
    int capacity() const {
        return (int)mNodes.size();
    }
    template<typename FuncT>
    void foreach(FuncT f) const {
        for (auto nindex : mBuckets) {
            for (; nindex != -1; nindex = mNodes[nindex].next) {
                f(mNodes[nindex].key, mNodes[nindex].value);
            }
        }
    }

private:
    void rehash() {
        int oldSize = (int)mNodes.size();
        mNodes.resize(oldSize * 2);
        mBuckets.assign(oldSize * 2, -1);
        mMask = mBuckets.size() - 1;
        {
            assert(mFree == -1);
            mFree = oldSize;
            for (int i = oldSize; i < (int)mNodes.size(); ++i) {
                mNodes[i].next = i + 1;
            }
            mNodes.back().next = -1;
        }

        for (int nindex = 0; nindex < oldSize; ++nindex) {
            int i = mHash(mNodes[nindex].key) & mMask;
            mNodes[nindex].next = mBuckets[i];
            mBuckets[i] = nindex;
        }
    }

private:
    struct Node {
        KeyT key;
        ValueT value;
        int next;
    };

private:
    vector<Node> mNodes;
    vector<int> mBuckets;
    int mFree;
    int mSize;
    int mMask;
    HashT mHash;
    EqualT mEqual;
};

template<typename KeyT, typename ValueT>
class ScopedSymbolTable {
public:

private:
};

template<typename KeyT, typename ValueT, typename HashT = std::hash<KeyT>, typename EqualT = std::equal_to<KeyT>>
class ScopedSymbolTable_HashTable {
private:
    typedef HashTable<KeyT, ValueT, HashT, EqualT> HashTableT;
public:
    void pushScope() {
        mTables.push_back(HashTableT());
    }
    void popScope() {
        mTables.pop_back();
    }
    void push(KeyT const &key, ValueT const& value) {
        mTables.back().insert(key, value);
    }
    ValueT const *lookup(KeyT const &key) const {
        for (auto it = mTables.rbegin(); it != mTables.rend(); ++it) {
            auto p = it->lookup(key);
            if (p != nullptr) return p;
        }
        return nullptr;
    }
    ValueT *lookup(KeyT const &key) {
        return const_cast<ValueT*>(const_cast<ScopedSymbolTable_HashTable const*>(this)->lookup(key));
    }

private:
    vector<HashTableT> mTables;
};

template<typename TableT>
class ScopedSymbolTable_STL {
private:
    typedef typename TableT::key_type KeyT;
    typedef typename TableT::mapped_type ValueT;
public:
    void pushScope() {
        mTables.push_back(TableT());
    }
    void popScope() {
        mTables.pop_back();
    }
    void push(KeyT const &key, ValueT const& value) {
        mTables.back().insert(make_pair(key, value));
    }
    ValueT const *lookup(KeyT const &key) const {
        for (auto it = mTables.rbegin(); it != mTables.rend(); ++it) {
            auto p = it->find(key);
            if (p != it->end()) return &p->second;
        }
        return nullptr;
    }
    ValueT *lookup(KeyT const &key) {
        return const_cast<ValueT*>(const_cast<ScopedSymbolTable_STL const*>(this)->lookup(key));
    }

private:
    vector<TableT> mTables;
};

//-------------------------------------------------------------------------
#pragma warning(push)
#pragma warning(disable : 4311 4312)
#include <Windows.h>
#pragma warning(pop)

#define ARRAY_SIZE(a) (sizeof(a) / sizeof((a)[0]))

static inline double getTime() {
    long long time, freq;
    ::QueryPerformanceFrequency((LARGE_INTEGER*)&freq);
    ::QueryPerformanceCounter((LARGE_INTEGER*)&time);
    return double(time) / freq;
}

template<typename TableT>
static void correctnessTest(const char *name) {
    TableT table;
    ScopedSymbolTable_STL<unordered_map<string, int>> stlTable;

    string keys[] = {"abc", "abc1", "abc2", "abc3", "x", "y", "z", "z1", "z2", "z3"};

    for (int i = 0; i < 8; ++i) {
        table.pushScope();
        stlTable.pushScope();

        for (auto key : keys) {
            if (rand() % 2 == 0) {
                table.push(key, 0);
                stlTable.push(key, 0);
            }
        }

        for (int j = 0; j < 8; ++j) {
            table.pushScope();
            stlTable.pushScope();

            for (auto key : keys) {
                if (rand() % 2 == 0) {
                    table.push(key, 1);
                    stlTable.push(key, 1);
                }
            }
            for (int k = 0; k < 8; ++k) {
                table.pushScope();
                stlTable.pushScope();

                for (auto key : keys) {
                    if (rand() % 2 == 0) {
                        table.push(key, 2);
                        stlTable.push(key, 2);
                    }
                }

                for (auto key : keys) {
                    auto p1 = table.lookup(key);
                    auto p2 = stlTable.lookup(key);
                    if (p1 != nullptr) assert(*p1 == *p2);
                    else {
                        assert(p1 == nullptr);
                        assert(p2 == nullptr);
                    }
                }

                stlTable.popScope();
                table.popScope();
            }
            stlTable.popScope();
            table.popScope();
        }
        stlTable.popScope();
        table.popScope();
    }

    printf("%-32s Ok!\n", name);
}

template<int LOOKUP, int MAX_DEPTH, typename TableT>
static double benchmark_iterate(int depth, TableT &table, vector<string> const &keys) {
    if (depth == 0) {
        double start = getTime();
        for (int i = 0; i < LOOKUP; ++i) {
            for (auto &key : keys) {
                table.lookup(key);
            }
        }
        return getTime() - start;
    } else {
        double totalTime = 0;

        auto K = (MAX_DEPTH - depth) / 2;
        for (int i = 0; i < K + 1; ++i) {
            table.pushScope();

            for (auto &key : keys) {
                if (rand() % MAX_DEPTH * 2 <= depth) table.push(key, depth);
            }

            totalTime += benchmark_iterate<LOOKUP, MAX_DEPTH>(depth - 1, table, keys);

            table.popScope();
        }

        return totalTime;
    }
}

template<typename TableT>
static void benchmark(const char *name) {
    TableT table;

    const int KEY_COUNT = 32;
    const int LOOP = 4;
    const int MAX_DEPTH = 7;
    const int LOOKUP = 512;

    vector<string> keys;
    for (int i = 0; i < KEY_COUNT; ++i) {
        ostringstream so;
        so << "var_" << i;
        keys.push_back(so.str());
    }

    double totalTime = 0;
    double startTime2 = getTime();
    for (int i = 0; i < LOOP; ++i) {
        totalTime = benchmark_iterate<LOOKUP, MAX_DEPTH>(MAX_DEPTH, table, keys);
    }

    printf("%-32s: %gs, %gs\n", name, totalTime, getTime() - startTime2);
}

int main() {
    correctnessTest<ScopedSymbolTable_HashTable<string, int>>("ScopedSymbolTable_HashTable");
#ifndef _DEBUG
    benchmark<ScopedSymbolTable_HashTable<string, int>>("HashTable");
    benchmark<ScopedSymbolTable_STL<unordered_map<string, int>>>("unordered_map");
    benchmark<ScopedSymbolTable_STL<map<string, int>>>("map");
#endif
}
