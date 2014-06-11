#ifndef POOL_ALLOCATOR_H
#define POOL_ALLOCATOR_H

template<int N>
class PoolAllocator {
public:
    PoolAllocator(int chunkSize = 16 * 1024): 
        mChunkSize(chunkSize), mChunkList(nullptr), mFreeList(nullptr) {
        assert(chunkSize > sizeof(Chunk));
    }

    PoolAllocator(const PoolAllocator &o) = delete;
    PoolAllocator& operator = (const PoolAllocator &o) = delete;

    ~PoolAllocator() {
        for (Chunk *next; mChunkList; mChunkList = next) {
            next = mChunkList->next;
            ::free(mChunkList);
        }
    }

    void* malloc() {
        if (mFreeList == nullptr) {
            mallocChunk();
        }

        void *p = mFreeList;
        mFreeList = mFreeList->next;
        return p;
    }

    void free(void* p) {
        Block* b = (Block*)p;
        b->next = mFreeList;
        mFreeList = b;
    }

private:
    struct Block {
        Block *next;
        char data[N - sizeof(Block*)];
    };

    struct Chunk {
        Chunk *next;
        Block blocks[1];
    };

private:
    void mallocChunk() {
        Chunk *chunk = (Chunk*)::malloc(mChunkSize);
        chunk->next = mChunkList;
        mChunkList = chunk;

        int blockCount = (mChunkSize - sizeof(Chunk)) / sizeof(Block) + 1;
        for (int i = 0; i < blockCount; ++i) {
            Block *b = &chunk->blocks[i];
            b->next = mFreeList;
            mFreeList = b;
        }
    }

private:
    int mChunkSize;
    Chunk *mChunkList;
    Block *mFreeList;
};

#endif
