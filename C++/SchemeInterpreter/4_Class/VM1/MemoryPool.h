#ifndef MEMORYPOOL_H
#define MEMORYPOOL_H

#ifndef USE_TCMALLOC

#include "PoolAllocator.h"

class MemoryPool {
public:
    MemoryPool(): mMemSize(0) {
        for (int i = 0; i < ARRAY_SIZE(mAllocs0); ++i) {
            mAllocs0[i] = new PoolAllocator((i + 1) * STEP_0, 4 * 1024);
        }
        for (int i = 0; i < ARRAY_SIZE(mAllocs1); ++i) {
            mAllocs1[i] = new PoolAllocator((i + 1) * STEP_1 + FENCE_0, 4 * 1024);
        }
    }

    ~MemoryPool() {
        for (auto p : mAllocs0) DELETE(p);
        for (auto p : mAllocs1) DELETE(p);
    }

    MemoryPool(const MemoryPool&) = delete;
    MemoryPool& operator = (const MemoryPool&) = delete;

    void* malloc(int size, int alignment) {
        void *p;

        if (size > FENCE_1) {
            p = ::malloc(size);
        } else if (size > FENCE_0) {
            p = mAllocs1[((size - FENCE_0) + STEP_1 - 1) / STEP_1 - 1]->malloc();
        } else {
            p = mAllocs0[(size + STEP_0 - 1) / STEP_0 - 1]->malloc();
        }

        mMemSize += size;

        ASSERT(force_cast<PtrValue>(p) % alignment == 0);
        return p;
    }

    void free(void *p, int size, int alignment) {
        mMemSize -= size;

        if (size > FENCE_1) {
            ::free(p);
        } else if (size > FENCE_0) {
            mAllocs1[((size - FENCE_0) + STEP_1 - 1) / STEP_1 -1]->free(p);
        } else {
            mAllocs0[(size + STEP_0 - 1) / STEP_0 - 1]->free(p);
        }
    }

    uint64_t getMemorySize() const {
        return mMemSize;
    }

private:
    static const int FENCE_0 = 512;
    static const int FENCE_1 = 1024;
    static const int STEP_0 = 8;
    static const int STEP_1 = 16;

private:
    PoolAllocator *mAllocs0[FENCE_0 / STEP_0];
    PoolAllocator *mAllocs1[(FENCE_1 - FENCE_0) / STEP_1];
    uint64_t mMemSize;
};

#else

class MemoryPool {
public:
    MemoryPool(): mMemSize(0) {
    }

    void* malloc(int size, int alignment) {
        mMemSize += size;

        void *p = ::malloc(size);
        ASSERT(force_cast<PtrValue>(p) % alignment == 0);
        return p;
    }

    void free(void *p, int size, int alignment) {
        mMemSize -= size;

        ::free(p);
    }

    uint64_t getMemorySize() const {
        return mMemSize;
    }

    MemoryPool(const MemoryPool&);
    MemoryPool& operator = (const MemoryPool&);

private:
    uint64_t mMemSize;
};

#endif

#endif
