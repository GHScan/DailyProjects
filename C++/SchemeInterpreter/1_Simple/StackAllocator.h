#ifndef STACK_ALLOCATOR_H
#define STACK_ALLOCATOR_H

class StackAllocator {
public:
    StackAllocator(int chunkSize = 16 * 1024): mChunkPayloadSize(chunkSize - sizeof(Chunk) + 1) {
        assert(mChunkPayloadSize > 0);

        mFirstChunk = mCurrChunk = mallocChunk(mChunkPayloadSize);
        mCurrPtr = mCurrChunk->data;
    }

    StackAllocator(const StackAllocator &o) = delete;
    StackAllocator& operator = (const StackAllocator &o) = delete;

    ~StackAllocator() {
        for (Chunk *next; mFirstChunk; mFirstChunk = next) {
            next = mFirstChunk->next;
            FREE(mFirstChunk);
        }
    }

    void* malloc(int size, int alignment) {
        assert(size > 0 && alignment > 0);
        assert((alignment & (alignment - 1)) == 0);

        int alignmentMask = alignment - 1;
        mCurrPtr += (alignment - ((PtrValue)mCurrPtr & alignmentMask)) & alignmentMask;

        if (mCurrChunk->endPtr - mCurrPtr < size) {
            preparingNextChunk(size + alignment);
            return malloc(size, alignment);
        }

        void *p = mCurrPtr;
        mCurrPtr += size;

        assert(((PtrValue)p & alignmentMask) == 0);
        return p;
    }

    template<typename T>
    T* malloc() {
        return static_cast<T*>(malloc(sizeof(T), alignof(T)));
    }

    template<typename T>
    T* mallocArray(int size) {
        return static_cast<T*>(malloc(sizeof(T) * size, alignof(T)));
    }

    void reset() {
        mCurrChunk = mFirstChunk;
        mCurrPtr = mCurrChunk->data;
    }

private:
    struct Chunk {
        Chunk *next;
        char *endPtr;
        char data[1];
    };

private:
    static Chunk* mallocChunk(int payloadSize) {
        Chunk* p = static_cast<Chunk*>(::malloc(sizeof(Chunk) - 1 + payloadSize));
        p->next = nullptr;
        p->endPtr = p->data + payloadSize;
        return p;
    }

    void preparingNextChunk(int requireSize) {
        for (; mCurrChunk->next && (mCurrChunk->next->endPtr - mCurrChunk->next->data) < requireSize; mCurrChunk = mCurrChunk->next);

        if (mCurrChunk->next == nullptr) {
            mCurrChunk->next = mallocChunk(max(mChunkPayloadSize, requireSize));
        }

        mCurrChunk = mCurrChunk->next;
        mCurrPtr = mCurrChunk->data;
    }

private:
    int mChunkPayloadSize;
    Chunk *mFirstChunk; 
    Chunk *mCurrChunk;
    char *mCurrPtr;
};

#endif
