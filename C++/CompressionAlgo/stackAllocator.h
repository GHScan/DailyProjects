#ifndef STACK_ALLOCATOR_H
#define STACK_ALLOCATOR_H

#include <list>

template<typename T>
class TStackAllocator {
public:
    TStackAllocator(int chunkSize): mChunkSize(chunkSize), mOff(chunkSize) {
        mCurChunk = mChunks.begin();
        allocChunk();
    }
    ~TStackAllocator() {
        for (auto p : mChunks) ::free(p);
    }
    T* malloc() {
        if (mOff >= mChunkSize) {
            if (++mCurChunk == mChunks.end()) {
                allocChunk();
            } else {
                mOff = 0;
            }
        }
        assert(mOff < mChunkSize);

        return *mCurChunk + mOff++;
    }
    void reset() {
        mCurChunk = mChunks.begin();
        mOff = 0;
    }
private:
    void allocChunk() {
        assert(mCurChunk == mChunks.end() && mOff >= mChunkSize);

        T* p = (T*)::malloc(sizeof(T) * mChunkSize);
        mCurChunk = mChunks.insert(mChunks.end(), p);
        mOff = 0;
    }
private:
    int mChunkSize;
    list<T*> mChunks;
    typename list<T*>::iterator mCurChunk;
    int mOff;
};

#endif
