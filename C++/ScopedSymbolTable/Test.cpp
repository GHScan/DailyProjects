#include "stdafx.h"

#include <sstream>
#include <list>
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


template<typename T, int CHUNK_SIZE = 128>
class StackAllocator {
private:
    struct Chunk {
        T blocks[CHUNK_SIZE];
    };

    class ReverseIterator {
    public:
        ReverseIterator(typename list<Chunk>::reverse_iterator it, int off) : mIter(it), mOff(off) {
        }
        ReverseIterator& operator ++ () {
            if (--mOff < 0) {
                ++mIter;
                mOff = CHUNK_SIZE - 1;
            }
            return *this;
        }
        ReverseIterator operator ++ (int) {
            ReverseIterator temp(*this);
            ++*this;
            return temp;
        }
        T* operator *() {
            return &mIter->blocks[mOff];
        }
        bool operator == (ReverseIterator const &o) {
            return mIter == o.mIter && mOff == o.mOff;
        }
        bool operator != (ReverseIterator const &o) {
            return !(*this == o);
        }

    private:
        typename list<Chunk>::reverse_iterator mIter;
        int mOff;
    };
public:
    StackAllocator(): mNextFree(CHUNK_SIZE - 1) {
        mChunks.push_front(Chunk());
        mNextChunk = mChunks.rbegin();
    }
    bool empty() const {
        return mNextFree == CHUNK_SIZE - 1 && mNextChunk == mChunks.rbegin();
    }
    T* push() {
        if (mNextFree == -1) {
            auto newChunk = mNextChunk;
            ++newChunk;
            if (newChunk == mChunks.rend()) {
                // use new Chunk should be better
                mChunks.push_front(Chunk());
                ++mNextChunk;
            } else {
                mNextChunk = newChunk;
            }
            mNextFree = CHUNK_SIZE - 1;
        } 

        T *p = &mNextChunk->blocks[mNextFree];
        --mNextFree;
        return p;
    }
    void pop() {
        if (mNextFree == CHUNK_SIZE - 1) {
            --mNextChunk;
            mNextFree = 0;
        } else {
            ++mNextFree;
        }
    }
    T* top() {
        if (mNextFree == CHUNK_SIZE - 1) {
            auto prevChunk = mNextChunk;
            --prevChunk;
            return &prevChunk->blocks[0];
        } else {
            return &mNextChunk->blocks[mNextFree + 1];
        }
    }

    ReverseIterator rbegin() {
        return ReverseIterator(mChunks.rbegin(), CHUNK_SIZE - 1);
    }
    ReverseIterator rend() {
        return ++ReverseIterator(mNextChunk, mNextFree + 1);
    }

private:
    list<Chunk> mChunks;
    typename list<Chunk>::reverse_iterator mNextChunk;
    int mNextFree;
};

template<typename KeyT, typename ValueT, typename HashT = std::hash<KeyT>, typename EqualT = std::equal_to<KeyT>>
class ScopedSymbolTable {
public:
    typedef KeyT KeyT;
    typedef ValueT ValueT;
public:
    ScopedSymbolTable(int capacity = 16): mBuckets(roundToPow2(capacity), nullptr), mScope(0), mSize(0) {
        mMask = (int)mBuckets.size() - 1;
    }
    ~ScopedSymbolTable() {

    }
    ScopedSymbolTable(ScopedSymbolTable const&) = delete;
    ScopedSymbolTable& operator = (ScopedSymbolTable const&) = delete;
    ScopedSymbolTable(ScopedSymbolTable &&) = delete;
    ScopedSymbolTable& operator = (ScopedSymbolTable &&) = delete;

    void pushScope() {
        ++mScope;
    }
    void popScope() {
        while (!mStack.empty() && mStack.top()->scope == mScope) pop();
        --mScope;
    }
    void push(KeyT const &key, ValueT const& value) {
        if (mSize == (int)mBuckets.size()) rehash();

        ++mSize;
        auto p = mStack.push();
        p->key = key;
        p->value = value;
        p->scope = mScope;
        int i = mHash(p->key) & mMask;
        p->next = mBuckets[i];
        mBuckets[i] = p;
    }
    void pop() {
        assert(!empty());
        auto p = mStack.top();
        int i = mHash(p->key) & mMask;
        mBuckets[i] = mBuckets[i]->next;
        mStack.pop();
        --mSize;
    }
    ValueT const *lookup(KeyT const &key) const {
        for (Node *p = mBuckets[mHash(key) & mMask]; p != nullptr; p = p->next) {
            if (mEqual(key, p->key)) return &p->value;
        }
        return nullptr;
    }
    ValueT *lookup(KeyT const &key) {
        return const_cast<ValueT*>(const_cast<ScopedSymbolTable const*>(this)->lookup(key));
    }
    int size() const {
        return mSize;
    }
    bool empty() const {
        return size() == 0;
    }
    template<typename FuncT>
    void foreach(FuncT f) {
        for (auto it = mStack.rbegin(), end = mStack.rend(); it != end; ++it) {
            auto p = *it;
            f(p->key, p->value);
        }
    }

private:
    void rehash() {
        mBuckets.assign(mBuckets.size() * 2, nullptr);
        mMask = (int)mBuckets.size() - 1;
        for (auto it = mStack.rbegin(), end = mStack.rend(); it != end; ++it) {
            auto p = *it;
            auto i = mHash(p->key) & mMask;
            p->next = mBuckets[i];
            mBuckets[i] = p;
        }
    }

private:
    struct Node {
        Node *next;
        KeyT key;
        ValueT value;
        int scope;
    };
    friend class StackAllocator<Node>;

private:
    vector<Node*> mBuckets;
    StackAllocator<Node> mStack;
    int mScope;
    int mSize;
    int mMask;
    HashT mHash;
    EqualT mEqual;
};

template<typename KeyT, typename ValueT, typename HashT = std::hash<KeyT>, typename EqualT = std::equal_to<KeyT>>
class ScopedSymbolTable_HashTable {
public:
    typedef KeyT KeyT;
    typedef ValueT ValueT;
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
public:
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


class SymbolPool {
public:
    typedef map<string, int>::const_iterator SymbolInternal;
public:
    SymbolInternal intern(string const &str) {
        auto it = mSymbols.lower_bound(str);
        if (it != mSymbols.end() && it->first == str) return it;
        return mSymbols.insert(it, make_pair(str, (int)hash<string>()(str)));
    }

private:
    map<string, int> mSymbols;
};

class Symbol {
public:
    Symbol(string const &str) : mInternal(sPool.intern(str)) {
    }
    Symbol() {}
    bool operator == (Symbol const &o) const {
        return mInternal == o.mInternal;
    }
    bool operator < (Symbol const &o) const {
        return mInternal->first < o.mInternal->first;
    }
    int hashCode() const { return mInternal->second; }
    const char* c_str() const { return mInternal->first.c_str(); }

private:
    SymbolPool::SymbolInternal mInternal;
    static SymbolPool sPool;
};
SymbolPool Symbol::sPool;
ostream& operator << (ostream &so, Symbol const &symbol) {
    so << symbol.c_str();
    return so;
}

namespace std {
    template<>
    struct hash<Symbol>: public unary_function<Symbol, size_t>
    {	
        size_t operator()(Symbol const& key) const
        {	
            return key.hashCode();
        }
    };
}
//-------------------------------------------------------------------------
#pragma warning(push)
#pragma warning(disable : 4311 4312)
#include <Windows.h>
#pragma warning(pop)

#ifdef _DEBUG
#define ASSERT assert
#else
#define ASSERT(b) (b || (throw new exception("assert failed!"), false))
#endif

static inline double getTime() {
    long long time, freq;
    ::QueryPerformanceFrequency((LARGE_INTEGER*)&freq);
    ::QueryPerformanceCounter((LARGE_INTEGER*)&time);
    return double(time) / freq;
}

class Random {
public:
    Random() : mOff(0){}
    int next() {
        if (mOff == (int)mRands.size()) {
            if (mOff > 1024 * 256) mOff = 0;
            else mRands.push_back(::rand());
        }
        return mRands[mOff++];
    }
    void reset() {
        mOff = 0;
    }

    static Random* instance() {
        static Random sIns;
        return &sIns;
    }
private:
    vector<int> mRands;
    int mOff;
};

template<typename TableT>
static void correctnessTest(const char *name) {
    TableT table;
    ScopedSymbolTable_STL<unordered_map<typename TableT::KeyT, int>> stlTable;

    typename TableT::KeyT keys[] = { "abc", "abc1", "abc2", "abc3", "x", "y", "z", "z1", "z2", "z3" };

    auto r = Random::instance();
    r->reset();

    for (int i = 0; i < 8; ++i) {
        table.pushScope();
        stlTable.pushScope();

        for (auto key : keys) {
            if (r->next() % 2 == 0) {
                table.push(key, 0);
                stlTable.push(key, 0);
            }
        }

        for (int j = 0; j < 8; ++j) {
            table.pushScope();
            stlTable.pushScope();

            for (auto key : keys) {
                if (r->next() % 2 == 0) {
                    table.push(key, 1);
                    stlTable.push(key, 1);
                }
            }
            for (int k = 0; k < 8; ++k) {
                table.pushScope();
                stlTable.pushScope();

                for (auto key : keys) {
                    if (r->next() % 2 == 0) {
                        table.push(key, 2);
                        stlTable.push(key, 2);
                    }
                }

                for (auto key : keys) {
                    auto p1 = table.lookup(key);
                    auto p2 = stlTable.lookup(key);
                    if (p1 != nullptr) ASSERT(*p1 == *p2);
                    else {
                        ASSERT(p1 == nullptr);
                        ASSERT(p2 == nullptr);
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

int gHits;
int gLastHits;
template<int LOOKUP, int MAX_DEPTH, typename TableT>
static double benchmark_iterate(int depth, TableT &table, vector<typename TableT::KeyT> const &keys) {
    if (depth == 0) {
        double start = getTime();
        int hits = 0;
        for (int i = 0; i < LOOKUP; ++i) {
            for (auto &key : keys) {
                hits += table.lookup(key) != nullptr;
            }
        }
        gHits += hits;
        return getTime() - start;
    } else {
        double totalTime = 0;

        auto r = Random::instance();
        auto K = (MAX_DEPTH - depth) / 2;
        for (int i = 0; i < K + 1; ++i) {
            table.pushScope();

            for (auto &key : keys) {
                if (r->next() % MAX_DEPTH * 2 <= depth) table.push(key, depth);
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

    vector<typename TableT::KeyT> keys;
    for (int i = 0; i < KEY_COUNT; ++i) {
        ostringstream so;
        so << "var_" << i;
        keys.push_back(so.str());
    }

    Random::instance()->reset();
    gHits = 0;

    double totalTime = 0;
    double startTime2 = getTime();
    for (int i = 0; i < LOOP; ++i) {
        totalTime = benchmark_iterate<LOOKUP, MAX_DEPTH>(MAX_DEPTH, table, keys);
    }

    if (gLastHits != 0) ASSERT(gLastHits == gHits);
    gLastHits = gHits;

    printf("%-32s: %gs, %gs\n", name, totalTime, getTime() - startTime2);
}

int main() {
    correctnessTest<ScopedSymbolTable_HashTable<string, int>>("ScopedSymbolTable_HashTable");
    correctnessTest<ScopedSymbolTable<string, int>>("ScopedSymbolTable");
    correctnessTest<ScopedSymbolTable_HashTable<Symbol, int>>("ScopedSymbolTable_HashTable [Symbol]");
    correctnessTest<ScopedSymbolTable<Symbol, int>>("ScopedSymbolTable [Symbol]");
#ifndef _DEBUG
    benchmark<ScopedSymbolTable_STL<map<string, int>>>("map");
    benchmark<ScopedSymbolTable_STL<unordered_map<string, int>>>("unordered_map");
    benchmark<ScopedSymbolTable_HashTable<string, int>>("HashTable");
    benchmark<ScopedSymbolTable<string, int>>("ScopedSymbolTable");
    benchmark<ScopedSymbolTable_STL<map<Symbol, int>>>("map [Symbol]");
    benchmark<ScopedSymbolTable_STL<unordered_map<Symbol, int>>>("unordered_map [Symbol]");
    benchmark<ScopedSymbolTable_HashTable<Symbol, int>>("HashTable [Symbol]");
    benchmark<ScopedSymbolTable<Symbol, int>>("ScopedSymbolTable [Symbol]");
#endif
}
