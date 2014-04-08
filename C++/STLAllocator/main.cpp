#include "pch.h"

#include <forward_list>
#include <unordered_set>
#include <set>
#include <memory>
#include <fstream>
#include <chrono>
#include <functional>

#include <unistd.h>

static const int PAGE_SIZE = 4 * 1024;

class StackMemPool {
public:
    StackMemPool(int chunkAlign): mData(nullptr), mDataEnd(nullptr), mChunkAlign(chunkAlign) {}
    ~StackMemPool() {
        for (char *p : mChunks) ::free(p);
    }
    void* alloc(int size, int align) {{}
        assert(((align - 1) & align) == 0);
        int safeSize = size + align - 1;
        if (mDataEnd - mData < safeSize) allocChunk(safeSize);
        assert(mDataEnd - mData >= safeSize);

        char *p = mData;
        p += (align - ((long)p & (align - 1))) & (align - 1);
        assert((long)p % align == 0);
        assert(p + size - mData <= safeSize);
        mData = p + size;
        return p;
    }

    StackMemPool(const StackMemPool&) = delete;
    StackMemPool& operator = (const StackMemPool&) = delete;
private:
    void allocChunk(int size) {
        size = (size + mChunkAlign - 1) / mChunkAlign * mChunkAlign;
        mData = (char*)::malloc(size);
        mDataEnd = mData + size;
        mChunks.push_front(mData);
    }
private:
    forward_list<char*> mChunks;
    char *mData, *mDataEnd;
    int mChunkAlign;
};

class NodeMemPool {
private:
    struct Node {
        Node *next;
    };
public:
    NodeMemPool(int blockSize, int chunkSize): mFreeList(nullptr), mBlockSize(blockSize), mChunkSize(chunkSize) {
        mBlockSize = max(mBlockSize, (int)sizeof(Node));
        assert(chunkSize > 0);
    }
    ~NodeMemPool() {
        for (char *p : mChunks) ::free(p);
    }
    void* alloc() {
        if (mFreeList == nullptr) allocChunk();
        assert(mFreeList != nullptr);
        Node *n = mFreeList;
        mFreeList = n->next;
        return n;
    }
    void free(void *p) {
        Node *n = (Node*)p;
        n->next = mFreeList;
        mFreeList = n;
    }

    NodeMemPool(const NodeMemPool&) = delete;
    NodeMemPool& operator = (const NodeMemPool&) = delete;
private:
    void allocChunk() {
        char *p = (char*)::malloc(mChunkSize);
        mChunks.push_front(p);
        for (int i = 0; i + mBlockSize <= mChunkSize; i += mBlockSize) {
            Node *n = (Node*)&p[i];
            n->next = mFreeList;
            mFreeList = n;
        }
    }
private:
    forward_list<char*> mChunks;
    Node *mFreeList;
    int mBlockSize, mChunkSize;
};

class TemporalPoolStack {
public:
    static TemporalPoolStack* instance() {
        static TemporalPoolStack s_ins;
        return &s_ins;
    }
    TemporalPoolStack() { push(); }
    ~TemporalPoolStack() { pop(); }

    void push() { mStack.push_front(make_shared<StackMemPool>(8 * PAGE_SIZE)); }
    void pop() { mStack.pop_front(); }
    void* alloc(int size, int align) { return mStack.front()->alloc(size, align);}

    TemporalPoolStack(const TemporalPoolStack&) = delete;
    TemporalPoolStack& operator = (const TemporalPoolStack&) = delete;
private:
    forward_list<shared_ptr<StackMemPool>> mStack;
};

class NodePoolList {
public:
    static NodePoolList* instance() {
        static NodePoolList s_ins;
        return &s_ins;
    }
    NodePoolList() {
        for (int i = 1; i <= 256 / STEP; ++i) mPools.push_back(make_shared<NodeMemPool>(i * STEP, 4 * PAGE_SIZE));
    }

    void* alloc(int size) {
        int idx = (size + STEP - 1) / STEP;
        assert(idx >= 0 && idx < (int)mPools.size());
        return mPools[idx]->alloc();
    }
    void free(void *p, int size) {
        int idx = (size + STEP - 1) / STEP;
        assert(idx >= 0 && idx < (int)mPools.size());
        return mPools[idx]->free(p);
    }

    NodePoolList(const NodePoolList&) = delete;
    NodePoolList& operator = (const NodePoolList&) = delete;
private:
    vector<shared_ptr<NodeMemPool>> mPools;
    static const int STEP = 4;
};

template<typename T>
class TemporalAllocator: public allocator<T> {
public:
    using typename allocator<T>::size_type;
    using typename allocator<T>::pointer;

    TemporalAllocator(){}
    TemporalAllocator(const TemporalAllocator&) {}
    template <class U> TemporalAllocator(const TemporalAllocator<U>&) {}

    template <class U> struct rebind { typedef TemporalAllocator<U> other; };

    pointer allocate(size_type n, const void* = nullptr) {
        int size = n * sizeof(T);
        if (size >= PAGE_SIZE) return (pointer)::malloc(size);
        return (pointer)TemporalPoolStack::instance()->alloc(size, alignof(T));
    }
    void deallocate(pointer p, size_type n) {
        int size = n * sizeof(T);
        if (size >= PAGE_SIZE) ::free(p);
    }
};

template<typename T>
class NodeOptimalAllocator: public allocator<T> {
public:
    using typename allocator<T>::size_type;
    using typename allocator<T>::pointer;

    NodeOptimalAllocator(){}
    NodeOptimalAllocator(const NodeOptimalAllocator&) {}
    template <class U> NodeOptimalAllocator(const NodeOptimalAllocator<U>&) {}

    template <class U> struct rebind { typedef NodeOptimalAllocator<U> other; };

    pointer allocate(size_type n, const void* = nullptr) {
        if (n == 1) {
            return (pointer)NodePoolList::instance()->alloc(sizeof(T));
        } else {
            return (pointer)::malloc(n * sizeof(T));
        }
    }
    void deallocate(pointer p, size_type n) {
        if (n == 1) {
            NodePoolList::instance()->free(p, sizeof(T));
        } else {
            ::free(p);
        }
    }
};
//////////////////////////////
static void getMemUsage(double& vm_usage, double& resident_set)
{
    vm_usage     = 0.0;
    resident_set = 0.0;

    ifstream stat_stream("/proc/self/stat");

    string pid, comm, state, ppid, pgrp, session, tty_nr;
    string tpgid, flags, minflt, cminflt, majflt, cmajflt;
    string utime, stime, cutime, cstime, priority, nice;
    string O, itrealvalue, starttime;

    unsigned long vsize;
    long rss;

    stat_stream >> pid >> comm >> state >> ppid >> pgrp >> session >> tty_nr
        >> tpgid >> flags >> minflt >> cminflt >> majflt >> cmajflt
        >> utime >> stime >> cutime >> cstime >> priority >> nice
        >> O >> itrealvalue >> starttime >> vsize >> rss;

    stat_stream.close();

    long page_size_kb = sysconf(_SC_PAGE_SIZE) / 1024;
    vm_usage     = vsize / 1024.0 / 1024.0;
    resident_set = rss * page_size_kb / 1024.0;
}

template<typename ItT>
static size_t rangeHash(ItT begin, ItT end) {
    size_t seed = 0;
    for (; begin != end; ++begin) {
        auto v = *begin;
        seed ^= std::hash<decltype(v)>()(*begin) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    }
    return seed;
}
//////////////////////////////
class SimpleString {
public:
    SimpleString(const char *str) {
        int len = strlen(str);
        mStr = (char*)TemporalPoolStack::instance()->alloc(len + 1, alignof(char));
        memcpy(mStr, str, len + 1);
    }
    SimpleString(const SimpleString& o): SimpleString(o.mStr) {
    }
    SimpleString& operator = (const SimpleString& o) {
        if (this != &o) {
            SimpleString tmp(o);
            swap(tmp);
        }
        return *this;
    }
    SimpleString(SimpleString&& o): mStr(nullptr) {
        swap(o);
    }
    SimpleString& operator = (SimpleString&& o) {
        swap(o);
        return *this;
    }
    void swap(SimpleString &o) {
        std::swap(mStr, o.mStr);
    }
    bool operator == (const SimpleString& o) const {
        return strcmp(mStr, o.mStr) == 0;
    }
    const char *c_str() const { return mStr;}
private:
    char *mStr;
};
struct SimpleStringHash {
    size_t operator () (const SimpleString& a) const {
        const char *str = a.c_str();
        return rangeHash(str, str + strlen(str));
    }
};

typedef basic_string<char, char_traits<char>, TemporalAllocator<char>> TemporalString;

struct TemporalStringHash {
    size_t operator () (const TemporalString& a) const {
        return rangeHash(a.c_str(), a.c_str() + a.size());
    }
};

template<typename KT>
class DummySet {
public:
    typedef KT key_type;
    int size() { return 0; }
    void insert(const KT&) {}
};

template<typename T>
class EmptyAllocator: public allocator<T> {
public:
    using typename allocator<T>::size_type;
    using typename allocator<T>::pointer;

    EmptyAllocator(){}
    EmptyAllocator(const EmptyAllocator&) {}
    template <class U> EmptyAllocator(const EmptyAllocator<U>&) {}

    template <class U> struct rebind { typedef EmptyAllocator<U> other; };

    pointer allocate(size_type n, const void* = nullptr) {
        return nullptr;
    }
    void deallocate(pointer p, size_type n) {
    }
};

template<typename SetT>
static void go(const char *name) {
    auto start = chrono::high_resolution_clock::now();

    SetT set;
    for (string line; getline(cin, line); ) set.insert(line.c_str());

    printf("%-60s:", name);
    printf("(%s=%.3fs)", "time", chrono::duration<double>(chrono::high_resolution_clock::now() - start).count());

    double vm, pm;
    getMemUsage(vm, pm);
    printf("(%s=%.3f)", "vm", vm);
    printf("(%s=%.3f)", "pm", pm);
    printf("(%s=%d)", "uniq", (int)set.size());
    puts("");
}

template<typename Allocator>
static void benchmarkAllocator(const char *name) {
    auto start = chrono::high_resolution_clock::now();

    Allocator a;

    const int SIZE = 1024 * 1024;
    vector<int*> data;
    data.reserve(SIZE);
    for (int i = 0; i < 8; ++i) {
        while (data.size() < SIZE) {
            switch (rand() % 5) {
                case 0: case 1: case 2: case 3:
                    data.push_back(a.allocate(1));
                    break;
                case 4: case 5: case 6:
                    if (!data.empty()) {
                        int i = rand() % (int)data.size();
                        a.deallocate(data[i], 1);
                        data[i] = data.back();
                        data.pop_back();
                    }
                    break; 
                default: break;
            }
        }
        while (!data.empty()) {
            a.deallocate(data.back(), 1);
            data.pop_back();
        }
    }

    printf("%s=%.3fs\n", name, chrono::duration<double>(chrono::high_resolution_clock::now() - start).count());
}
static void benchmark() {
    benchmarkAllocator<EmptyAllocator<int>>("EmptyAllocator");
    benchmarkAllocator<allocator<int>>("allocator");
    benchmarkAllocator<TemporalAllocator<int>>("TemporalAllocator");
    benchmarkAllocator<NodeOptimalAllocator<int>>("NodeOptimalAllocator");
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        benchmark();
        return 0;
    }

    switch (atoi(argv[1])) {
        case 0: go<DummySet<string>>("Dummy<string>"); break;
        case 1: go<set<string>>("set<string>"); break;
        case 2: go<unordered_set<string>>("unorderd_set<string>"); break;
        case 3: go<set<TemporalString, less<TemporalString>, TemporalAllocator<TemporalString>>>(
                        "set<TemporalString,less,TemporalAllocator>"); break;
        case 4: go<unordered_set<TemporalString, TemporalStringHash, equal_to<TemporalString>, TemporalAllocator<TemporalString>>>(
                        "unordered_set<TemporalString,less,TemporalAllocator>"); break;
        case 5: go<set<string, less<string>, NodeOptimalAllocator<string>>>(
                        "set<string,less,NodeOptimalAllocator>"); break;
        case 6: go<unordered_set<string, hash<string>, equal_to<string>, NodeOptimalAllocator<string>>>(
                        "unordered_set<string,less,NodeOptimalAllocator>"); break;
        case 7: go<set<TemporalString, less<TemporalString>, NodeOptimalAllocator<TemporalString>>>(
                        "set<TemporalString,less,NodeOptimalAllocator>"); break;
        case 8: go<unordered_set<TemporalString, TemporalStringHash, equal_to<TemporalString>, NodeOptimalAllocator<TemporalString>>>(
                        "unordered_set<TemporalString,less,NodeOptimalAllocator>"); break;
        case 9: go<unordered_set<SimpleString, SimpleStringHash, equal_to<SimpleString>, TemporalAllocator<SimpleString>>>(
                        "unordered_set<SimpleString,less,TemporalAllocator>"); break;
        case 10: go<unordered_set<SimpleString, SimpleStringHash, equal_to<SimpleString>, NodeOptimalAllocator<SimpleString>>>(
                        "unordered_set<SimpleString,less,NodeOptimalAllocator>"); break;
        default:
            fprintf(stderr, "%s\n", "Invalid algo type!");
            return 1;
    }
}
