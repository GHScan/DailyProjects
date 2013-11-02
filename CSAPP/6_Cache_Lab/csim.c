#include "cachelab.h"

#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <unistd.h>
#include <getopt.h>

static const int ADDRESS_BIT_COUNT = 64;

typedef struct _CacheLine {
    long long tag;
    long long timestamp : 32;
    long long isValid : 1;
} CacheLine;

typedef struct _CacheSet {
    CacheLine *lines;
} CacheSet;

typedef struct _Cache {
    CacheSet *sets;
    int setBitCount;
    int blockBitCount;
    int tagBitCount;
    int associativity;
    int timeClock;
    void(*hitCallback)(long long addr, int size);
    void(*missCallback)(long long addr, int size);
    void(*evictionCallback)(long long addr, int size);
} Cache;

static Cache* setupCache(
        int setBitCount, int associativity, int blockBitCount,
        void(*hitCallback)(), void(*missCallback)(), void(*evictionCallback)()) {
    Cache *cache = (Cache*)malloc(sizeof(Cache));
    cache->hitCallback = hitCallback;
    cache->missCallback = missCallback;
    cache->evictionCallback = evictionCallback;
    cache->associativity = associativity;
    cache->tagBitCount = ADDRESS_BIT_COUNT - (setBitCount + blockBitCount);
    cache->blockBitCount = blockBitCount;
    cache->setBitCount = setBitCount;
    cache->timeClock = 0;

    int setCount = 1 << setBitCount;
    cache->sets = (CacheSet*)malloc(sizeof(CacheSet) * setCount);
    for (int i = 0; i < setCount; ++i) {
        CacheSet *set = &cache->sets[i];
        set->lines = (CacheLine*)malloc(sizeof(CacheLine) * associativity);
        for (int i = 0; i < associativity; ++i) {
            CacheLine *line = &set->lines[i];
            line->isValid = 0;
            line->timestamp = 0;
            line->tag = -1;
        }
    }
    return cache;
}
static void accessCache(Cache *cache, long long addr, int size) {
    ++cache->timeClock;

    long long addrTag = addr & (-1 << (ADDRESS_BIT_COUNT - cache->tagBitCount));
    long long setMask = ((1 << cache->setBitCount) - 1) << cache->blockBitCount;
    int setIdx = (addr & setMask) >> cache->blockBitCount;
    CacheSet *set = &cache->sets[setIdx];

    for (int i = 0; i < cache->associativity; ++i) {
        CacheLine *line = &set->lines[i];
        if (line->isValid && line->tag == addrTag) {
            line->timestamp = cache->timeClock;
            cache->hitCallback(addr, size);
            return;
        }
    }
    for (int i = 0; i < cache->associativity; ++i) {
        CacheLine *line = &set->lines[i];
        if (!line->isValid) {
            line->isValid = 1;
            line->tag = addrTag;
            line->timestamp = cache->timeClock;
            cache->missCallback(addr, size);
            return;
        }
    }
    int minTimestamp = set->lines[0].timestamp;
    int evictionIdx = 0;
    for (int i = 1; i < cache->associativity; ++i) {
        if (set->lines[i].timestamp < minTimestamp) {
            minTimestamp = set->lines[i].timestamp;
            evictionIdx = i;
        }
    }
    cache->missCallback(addr, size);
    cache->evictionCallback(addr, size);
    set->lines[evictionIdx].timestamp = cache->timeClock;
    set->lines[evictionIdx].tag = addrTag;
}
static void cleanupCache(Cache *cache) {
    int setCount = 1 << cache->setBitCount;
    for (int i = 0; i < setCount; ++i) {
        free(cache->sets[i].lines);
    }
    free(cache->sets);
    free(cache);
}

static int g_hitCount = 0;
static int g_missCount = 0;
static int g_evictionCount = 0;
static int g_isVerbose = 0;
static void onCacheHit(long long addr, int size) {
    ++g_hitCount;
    if (g_isVerbose) printf(" hit");
}
static void onCacheMiss(long long addr, int size) {
    ++g_missCount;
    if (g_isVerbose) printf(" miss");
}
static void onCacheEviction(long long addr, int size) {
    ++g_evictionCount;
    if (g_isVerbose) printf(" eviction");
}

int main(int argc, char *argv[]) {
    int opt = 0;
    int setBitCount = 1, blockBitCount = 1, associativity = 1;
    while ((opt = getopt(argc, argv, "hvs:E:b:t:")) != -1) {
        switch(opt) {
            case 'h':
                printf(
                        "Usage: %s [-hv] -s <num> -E <num> -b <num> -t <file>\n"
                        "Options:\n"
                        "-h         Print this help message.\n"
                        "-v         Optional verbose flag.\n"
                        "-s <num>   Number of set index bits.\n"
                        "-E <num>   Number of lines per set.\n"
                        "-b <num>   Number of block offset bits.\n"
                        "-t <file>  Trace file.\n"
                        "\n"
                        "Examples:\n"
                        "linux>  %s -s 4 -E 1 -b 4 -t traces/yi.trace\n"
                        "linux>  %s -v -s 8 -E 2 -b 4 -t traces/yi.trace\n", argv[0], argv[0], argv[0]);
                break;
            case 'v':
                g_isVerbose = 1;
                break;
            case 's':
                setBitCount = atoi(optarg);
                break;
            case 'E':
                associativity = atoi(optarg);
                break;
            case 'b':
                blockBitCount = atoi(optarg);
                break;
            case 't': {
                FILE *traceFile = fopen(optarg, "r");
                dup2(fileno(traceFile), fileno(stdin));
                fclose(traceFile);
              } break;
            default:
                break;
        }
    }

    Cache* cache = setupCache(setBitCount, associativity, blockBitCount, &onCacheHit, &onCacheMiss, &onCacheEviction);
    {
        char buf[32] = "";
        long long addr;
        int size;
        while (scanf("%s %llx,%d", buf, &addr, &size) == 3) {
            printf("%s %llx,%d", buf, addr, size);
            switch (buf[0]) {
                case 'I':
                    break;
                case 'L':
                    accessCache(cache, addr, size);
                    break;
                case 'S':
                    accessCache(cache, addr, size);
                    break;
                case 'M':
                    accessCache(cache, addr, size);
                    accessCache(cache, addr, size);
                    break;
            }
            puts("");
        }
    }
    cleanupCache(cache);

    printSummary(g_hitCount, g_missCount, g_evictionCount);
    return 0;
}
