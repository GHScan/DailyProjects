#include "pch.h"

#include <time.h>

#include "FixsizeAllocator.h"
#include "BitVectorAllocator.h"

static void* alloc0(int sz) { return ::malloc(sz);}
static void free0(void *p){ ::free(p);}
static void* alloc1(int sz) { return FixsizeAllocator::instance()->malloc(sz);}
static void free1(void *p){ FixsizeAllocator::instance()->free(p);}
static void* alloc2(int sz) { return BitVectorAllocator::instance()->alloc(sz);}
static void free2(void *p){ BitVectorAllocator::instance()->free(p);}

static int genRanSize()
{
    switch (rand() % 10) {
        case 0: case 1: case 2: case 3:
            return rand() % 12 + 4;
        case 4: case 5: case 6: 
            return rand() % 16 + 16;
        case 7: case 8:
            return rand() % 32 + 32;
        case 9: 
            return rand() % 64 + 64;
        default:
            return 4;
    }
}

static void test()
{
    void* (*allocas[])(int) = {&alloc0, &alloc1, &alloc2};
    void (*frees[])(void*) = {&free0, &free1, &free2};

    vector<int> ranSizes;
    for (int i = 0; i < (1<<15); ++i) ranSizes.push_back(genRanSize());
    vector<void*> pts;
    pts.reserve(ranSizes.size());

    for (int i = 0; i < COUNT_OF(allocas); ++i) {
        auto alloc = allocas[i];
        auto free = frees[i];

        clock_t total = 0;
        for (int j = 0; j < 200; ++j) {
            clock_t t1 = clock();
            for (auto sz : ranSizes) pts.push_back(alloc(sz));
            t1 = clock() - t1;

            for (auto p : pts) *(char*)p = 23;
            random_shuffle(pts.begin(), pts.end());

            clock_t t2 = clock();
            for (auto p : pts) free(p);
            t2 = clock() - t2;

            pts.clear();

            total += t2 + t1;
        }

        printf("%d : %f\n", i, total / float(CLOCKS_PER_SEC));
    } 
}

int main()
{
    srand((int)time(NULL));

    test();
}
