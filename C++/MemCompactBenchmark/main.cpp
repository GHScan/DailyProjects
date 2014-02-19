
#include "pch.h"

void timeit(const char *name, function<void()> f) {
    clock_t start = clock();
    f();
    printf("%s : %f\n", name, float(clock() - start) / CLOCKS_PER_SEC);
}

struct IAllocator {
    virtual ~IAllocator(){}
    virtual void* alloc(int size) = 0;
    virtual void free(void *p) = 0;
};
class StandardAllocator: public IAllocator {
public:
    virtual void* alloc(int size) override { return ::malloc(size);}
    virtual void free(void *p) override { ::free(p);}
private:
};
class PoolAllocator: public IAllocator {
public:
    PoolAllocator(int size): m_pbegin((char*)::malloc(size)){ m_pcur = m_pbegin; m_pend = m_pbegin + size;}
    ~PoolAllocator(){ ::free(m_pbegin); }
    virtual void* alloc(int size) override { 
        assert(m_pcur + size <= m_pend); 
        void *p = m_pcur;
        m_pcur += size;
        return p;
    }
    virtual void free(void *p) override {}
    int getAllocatedSize() { return m_pcur - m_pbegin;}
private:
    char *m_pbegin, *m_pend, *m_pcur;
};

class Object {
public:
    static Object* create(IAllocator *allocator, int len) {
        Object *r = (Object*)allocator->alloc(sizeof(Object));
        r->m_len = len;
        r->m_allocator = allocator;
        r->m_fields = (int**)allocator->alloc(len * sizeof(int*));
        for (int i = 0; i < len; ++i) r->m_fields[i] = (int*)allocator->alloc(sizeof(int));
        return r;
    }
    void destroy() {
        for (int i = 0; i < m_len; ++i) {
            m_allocator->free(m_fields[i]);
        }
        m_allocator->free(m_fields);
        m_allocator->free(this);
    }
    void init() {
        for (int i = 0; i < m_len; ++i) *m_fields[i] = rand();
    }
    int action() {
        int r = 0;
        for (int i = 0; i < m_len; ++i) {
            r += *m_fields[i] * *m_fields[i];
        }
        return r;
    }
    Object* cloneWithAllocator(IAllocator *allocator) {
        Object *r = create(allocator, m_len);
        for (int i = 0; i < m_len; ++i) {
            *r->m_fields[i] = *m_fields[i];
        }
        return r;
    }
private:
    IAllocator *m_allocator;
    int **m_fields;
    int m_len;
};

const int COUNT = 1 << 18;
const int MAX_LEN = 32;
#ifdef NDEBUG
const int LOOP = 100;
#else
const int LOOP = 10;
#endif

int main() {
    srand(time(NULL));

    StandardAllocator stdAlloc;
    PoolAllocator poolAlloc(MAX_LEN * COUNT * 4);
    Object *objs[COUNT] = {0};

    for (int i = 0; i < COUNT; ++i) {
        objs[i] = Object::create(&stdAlloc, rand() % MAX_LEN);
        if (rand() % 3 == 0) {
            objs[i]->destroy();
            objs[i] = nullptr;
        }
        else objs[i]->init();
    }
    random_shuffle(objs, objs + COUNT);

    timeit("standard", [objs]() {
        int r = 0;
        for (int i = 0; i < LOOP; ++i)
        for (int i = 0; i < COUNT; ++i) {
            if (objs[i] != nullptr) r += objs[i]->action();
        }
        printf("result: %d\n", r);
    });

    for (int i = 0; i < COUNT; ++i) {
        if (objs[i] != nullptr) {
            Object *p = objs[i]->cloneWithAllocator(&poolAlloc);
            swap(p, objs[i]);
            p->destroy();
        }
    }
    printf("pool allocated size :%.3fMB\n", poolAlloc.getAllocatedSize() / float(1 << 20));

    timeit("compact", [objs]() {
        int r = 0;
        for (int i = 0; i < LOOP; ++i)
        for (int i = 0; i < COUNT; ++i) {
            if (objs[i] != nullptr) r += objs[i]->action();
        }
        printf("result: %d\n", r);
    });

    for (int i = 0; i < COUNT; ++i) {
        if (objs[i]) objs[i]->destroy();
    }
}
