#include "pch.h"

#include <chrono>
#include <fstream>

#include <unistd.h>
//////////////////////////////
static const int PAGE_SIZE = 4 * 1024;

class StackMemPool {
private:
    struct Chunk {
        Chunk *next;
        char data[1];
    };
public:
    StackMemPool(int chunkAlign): mChunks(nullptr), mData(nullptr), mDataEnd(nullptr), mChunkAlign(chunkAlign) {
    }
    ~StackMemPool() {
        for (Chunk *next; mChunks != nullptr; mChunks = next) {
            next = mChunks->next;
            ::free(mChunks);
        }
    }
    void* alloc(int size, int align) {
        assert(((align - 1) & align) == 0);
        int safeSize = size + align - 1;
        if (mDataEnd - mData < safeSize) {
            allocChunk(safeSize);
        }
        assert(mDataEnd - mData >= safeSize);

        char *p = mData;
        p += (align - ((long)p & (align - 1))) & (align - 1);
        assert((long)p % align == 0);
        assert(p + size - mData <= safeSize);
        mData = p + size;
        return p;
    }
    void swap(StackMemPool &o) {
        std::swap(mChunks, o.mChunks);
        std::swap(mData, o.mData);
        std::swap(mDataEnd, o.mDataEnd);
        std::swap(mChunkAlign, o.mChunkAlign);
    }
private:
    void allocChunk(int size) {
        int memSize = ((sizeof(Chunk) - 1 + size) + mChunkAlign - 1) / mChunkAlign * mChunkAlign;
        Chunk *p = (Chunk*)::malloc(memSize);
        p->next = mChunks;
        mChunks = p;
        mData = p->data;
        mDataEnd = mData + (memSize - (sizeof(Chunk) - 1));
    }
private:
    Chunk *mChunks;
    char *mData, *mDataEnd;
    int mChunkAlign;
};

template<int N>
class NodeMemPool {
private:
    struct Node {
        Node *next;
        char padding[N - sizeof(Node*)];
    };
    static_assert(sizeof(Node) == N, "");
    struct Chunk {
        Chunk *next;
        Node nodes[1];
    };
public:
    NodeMemPool(int chunkSize): mFreeNodes(nullptr), mChunks(nullptr), mChunkSize(chunkSize) {
        assert(chunkSize > sizeof(Chunk));
    }
    ~NodeMemPool() {
        for (Chunk *next; mChunks != nullptr; mChunks = next) {
            next = mChunks->next;
            ::free(mChunks);
        }
    }
    void* alloc() {
        if (mFreeNodes == nullptr) allocChunk();
        assert(mFreeNodes != nullptr);
        Node *n = mFreeNodes;
        mFreeNodes = n->next;
        return n;
    }
    void free(void *p) {
        Node *n = (Node*)p;
        n->next = mFreeNodes;
        mFreeNodes = n;
    }
private:
    void allocChunk() {
        Chunk *p = (Chunk*)::malloc(mChunkSize);
        p->next = mChunks;
        mChunks = p;
        for (Node *n = p->nodes; (char*)n - (char*)p + sizeof(Node) <= mChunkSize; ++n) {
            n->next = mFreeNodes;
            mFreeNodes = n;
        }
    }
private:
    Node *mFreeNodes;
    Chunk *mChunks;
    int mChunkSize;
};


template<typename T>
class Handle {
public:
    Handle(): mNode(nullptr){}
    operator T*() { return *mNode; }
    operator const T*() const { return *mNode; }
private:
    friend class HandleManager;
    explicit Handle(T **node): mNode(node){}
private:
    T **mNode;
};

class HandleManager {
private:
    struct Node {
        void *p;
        Node *prev, *next;
        int size : 28;
        int align : 4;
        Node(void *_p, int _size, int _align, Node *_prev, Node *_next): p(_p), prev(_prev), next(_next), size(_size), align(_align) {}
    };
public:
    template<typename T, typename ...ArgsT>
    Handle<T> create(ArgsT&& ...args) {
        int size = sizeof(T), align = alignof(T);
        void *p = size >= LARGE_OBJ_SIZE ? ::malloc(size) : mStackPool.alloc(size, align);
        new (p) T(forward<ArgsT>(args)...);
        Node *n = createNode(p, size, align);
        return Handle<T>((T**)n);
    }
    template<typename T>
    void destroy(Handle<T> h) {
        Node *n = (Node*)h.mNode;
        ((T*)h)->~T();
        if (n->size >= LARGE_OBJ_SIZE) ::free(n->p);
        destroyNode(n);
    }
    template<typename T>
    void setRelative(const Handle<T> &a, const Handle<T> &b) {
        Node *an = (Node*)a.mNode;
        Node *bn = (Node*)b.mNode;
        bn->next->prev = bn->prev;
        bn->prev->next = bn->next;
        bn->next = an->next;
        bn->prev = an;
        an->next->prev = bn;
        an->next = bn;
    }
    void compact() {
        StackMemPool newPool(STACK_POOL_CHUNK_ALIGN);
        for (Node *n = mHead.next; n != &mHead; n = n->next) {
            if (n->size >= LARGE_OBJ_SIZE) continue;
            void *p = newPool.alloc(n->size, n->align);
            memcpy(p, n->p, n->size);
            n->p = p;
        }
        mStackPool.swap(newPool);
    }

    HandleManager(): mStackPool(STACK_POOL_CHUNK_ALIGN), mNodePool(2 * PAGE_SIZE), mHead(nullptr, 0, 0, &mHead, &mHead) {
    }
    ~HandleManager() {
        assert(mHead.next == &mHead);
        assert(mHead.prev == &mHead);
    }
private:
    Node* createNode(void *p, int size, int align) {
         Node* n = new (mNodePool.alloc()) Node(p, size, align, &mHead, mHead.next);
         mHead.next->prev = n;
         mHead.next = n;
         return n;
    }
    void destroyNode(Node *n) {
        n->next->prev = n->prev;
        n->prev->next = n->next;
        n->~Node();
        mNodePool.free(n);
    }
private:
    StackMemPool mStackPool;
    NodeMemPool<sizeof(Node)> mNodePool;
    Node mHead;
    static const int LARGE_OBJ_SIZE = PAGE_SIZE;
    static const int STACK_POOL_CHUNK_ALIGN = PAGE_SIZE * 4;
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

//////////////////////////////
template<typename PtrT>
class SortBenchmark {
public:
    void run(int size) {
        mData.reserve(size);

        printf("%s:\n", getName());
        generate(size);
        benchmark();
        cleanup();
    }
    void generate(int size) {
        auto start = chrono::steady_clock::now();

        while (mData.size() < size) {
            switch (rand() % 7) {
                case 0: case 1: case 2: 
                    if (mData.size() > 0) {
                        int i = int(rand() % mData.size());
                        freePtr(mData[i]);
                        mData[i] = mData.back();
                        mData.pop_back();
                    }
                    break;
                case 3: case 4: case 5: case 6:
                    mData.push_back(allocPtr());
                    *mData.back() = rand() % mData.size();
                    break;
                default: break;
            }
        }

        printf("\tgenerate:%.3fs\n", chrono::duration<double>(chrono::steady_clock::now() - start).count());
    }
    void benchmark() {
        auto start = chrono::steady_clock::now();
        preparing();
        printf("\tpreparing:%.3fs\n", chrono::duration<double>(chrono::steady_clock::now() - start).count());

        double vm, pm;
        getMemUsage(vm, pm);
        printf("\tmemory usage: %.3fMB, %.3fMB\n", vm, pm);

        start = chrono::steady_clock::now();
        sort(mData.begin(), mData.end(), [](const PtrT &a, const PtrT &b){ return *a < *b; });
        printf("\tsort:%.3fs\n", chrono::duration<double>(chrono::steady_clock::now() - start).count());
    }
    void cleanup() {
        auto start = chrono::steady_clock::now();
        while (!mData.empty()) {
            freePtr(mData.back());
            mData.pop_back();
        }
        printf("\tcleanup:%.3fs\n", chrono::duration<double>(chrono::steady_clock::now() - start).count());
    }
    virtual const char* getName() = 0;
    virtual PtrT allocPtr() = 0;
    virtual void freePtr(PtrT p) = 0;
    virtual void preparing() = 0;
protected:
    vector<PtrT> mData;
};

class SortBenchmark_Default: public SortBenchmark<int*> {
    virtual const char* getName() { return "default"; }
    virtual int* allocPtr() { return (int*)::malloc(sizeof(int)); }
    virtual void freePtr(int *p) { ::free(p); }
    virtual void preparing() {}
};

class SortBenchmark_Handle: public SortBenchmark<Handle<int>> {
    virtual const char* getName() { return "handle compaction"; }
    virtual Handle<int> allocPtr() { return mMgr.create<int>(); }
    virtual void freePtr(Handle<int> p) { mMgr.destroy(p); }
    virtual void preparing() {
        mMgr.compact();
    }
    HandleManager mMgr;
};

class SortBenchmark_HandleRelative: public SortBenchmark<Handle<int>> {
    virtual const char* getName() { return "handle relative compaction"; }
    virtual Handle<int> allocPtr() { return mMgr.create<int>(); }
    virtual void freePtr(Handle<int> p) { mMgr.destroy(p); }
    virtual void preparing() {
        for (int i = 0; i < (int)mData.size() - 1; ++i) {
            mMgr.setRelative(mData[i], mData[i + 1]);
        }
        mMgr.compact();
    }
    HandleManager mMgr;
};
//////////////////////////////
template<typename T>
static void go(int size) {
    T o;
    o.run(size);
}

int main() {
    srand(time(nullptr));

    for (int size = 1024; size < (4 * 1024 * 1024); size <<= 2) {
        printf("@len=%.3fK\n", size / 1024.0);
        go<SortBenchmark_Default>(size);
        go<SortBenchmark_Handle>(size);
        go<SortBenchmark_HandleRelative>(size);
    }
}
