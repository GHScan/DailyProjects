#ifndef POOL_ALLOCATOR_H
#define POOL_ALLOCATOR_H

class PoolAllocator {
public:
    PoolAllocator(int blockSize, int chunkSize = 16 * 1024): 
        mBlockSize(blockSize), mChunkSize(chunkSize), mChunkList(nullptr), mFreeList(nullptr) {

        ASSERT(blockSize >= (int)sizeof(Block));
        ASSERT(chunkSize > (int)sizeof(Chunk));
    }

    PoolAllocator(const PoolAllocator &o) = delete;
    PoolAllocator& operator = (const PoolAllocator &o) = delete;

    ~PoolAllocator() {
        for (Chunk *next; mChunkList; mChunkList = next) {
            next = mChunkList->next;
            FREE(mChunkList);
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
        Block* b = static_cast<Block*>(p);
        b->next = mFreeList;
        mFreeList = b;
    }

private:
    struct Block {
        Block *next;
    };

    struct Chunk {
        Chunk *next;
    };

private:
    void mallocChunk() {
        char* p = static_cast<char*>(::malloc(mChunkSize));

        Chunk *chunk = reinterpret_cast<Chunk*>(p);
        chunk->next = mChunkList;
        mChunkList = chunk;

        ASSERT(mBlockSize >= (int)sizeof(Chunk));
        for (int i = mBlockSize; i + mBlockSize <= mChunkSize; i += mBlockSize) {
            Block *b = reinterpret_cast<Block*>(p + i);
            b->next = mFreeList;
            mFreeList = b;
        }
    }

private:
    int mBlockSize;
    int mChunkSize;
    Chunk *mChunkList;
    Block *mFreeList;
};

#endif
