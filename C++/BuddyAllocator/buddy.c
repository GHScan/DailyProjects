#include "buddy.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>

#define max(a, b) ((a) > (b) ? (a) : (b))

static inline int isPowerOf2(int i) 
{
    return !(i & (i - 1));
}
static inline int toCeilPowerOf2(int i)
{
    assert(i > 0 && sizeof(i) == 4);
    if (!isPowerOf2(i)) {
        i |= i >> 1; i |= i >> 2; i |= i >> 4; i |= i >> 8; i |= i >> 16;
        ++i;
    }
    assert(isPowerOf2(i));
    for (int j = 0; j < 31; ++j) {
        if (i & (1 << j)) return j;
    }
    assert(0);
    return 0;
}

struct buddy {
	int level;
    char freeLevel[1];
};

static inline int getNodeSize(struct buddy *self, int idx, int freeLevel)
{
    return 1 << (freeLevel - 1);
}
static inline int getNodeOffset(struct buddy *self, int idx, int freeLevel)
{
    return (idx - (1 << (self->level + 1 - freeLevel))) * getNodeSize(self, idx, freeLevel);
}

struct buddy * 
buddy_new(int level) {
    int size = 1 << level;
    struct buddy *self = malloc(sizeof(struct buddy) + size * 2 - 1);
    self->level = level;

    int freeLevel = level + 2;
    for (int i = 1; i < size * 2; ++i) {
        if (isPowerOf2(i)) --freeLevel;
        self->freeLevel[i] = freeLevel;
    }
    return self;
}

void
buddy_delete(struct buddy * self) {
    free(self);
}

int 
buddy_alloc(struct buddy * self , int s) {
    if (s == 0) s = 1;
    int freeLevel = toCeilPowerOf2(s) + 1;
    if (self->freeLevel[1] < freeLevel) return -1;
    int idx = 1;
    for (int i = 0; i < (self->level + 1 - freeLevel); ++i) { 
        idx *= 2;
        if (self->freeLevel[idx] < freeLevel) ++idx;
    }
    self->freeLevel[idx] = 0;
    int off = getNodeOffset(self, idx, freeLevel);
    while (idx > 1) {
        idx /= 2;
        self->freeLevel[idx] = max(self->freeLevel[2 * idx], self->freeLevel[2 * idx + 1]);
    }
    return off;
}
void
buddy_free(struct buddy * self, int offset) {
    int idx = offset + (1 << self->level);
    int freeLevel = 1;
    for (; self->freeLevel[idx]; idx /= 2, ++freeLevel);
    self->freeLevel[idx] = freeLevel;
    for (;idx > 1; ++freeLevel) {
        idx /= 2;
        int leftFL = self->freeLevel[idx * 2], rightFL = self->freeLevel[idx * 2 + 1];
        if (leftFL == rightFL && leftFL == freeLevel) {
            self->freeLevel[idx] = freeLevel + 1;
        }
        else self->freeLevel[idx] = max(self->freeLevel[2 * idx], self->freeLevel[2 * idx + 1]);
    }
}

int
buddy_size(struct buddy * self, int offset) {
    int size = 1;
    for (int idx = offset + (1 << self->level); self->freeLevel[idx]; idx /= 2, size *= 2);
    return size;
}

static void dump(struct buddy *self, int idx, int freeLevel)
{
    if (self->freeLevel[idx] == freeLevel) {
        printf("(%d,%d)", getNodeOffset(self, idx, freeLevel), getNodeSize(self, idx, freeLevel));
    }
    else if (self->freeLevel[idx] == 0) {
        if (freeLevel > 1 && self->freeLevel[idx * 2] == 0) {
            printf("{");
            dump(self, idx * 2, freeLevel - 1);
            dump(self, idx * 2 + 1, freeLevel - 1);
            printf("}");
        }
        else printf("[%d,%d]", getNodeOffset(self, idx, freeLevel), getNodeSize(self, idx, freeLevel));
    }
    else if (self->freeLevel[idx] < freeLevel) {
        printf("{");
        dump(self, idx * 2, freeLevel - 1);
        dump(self, idx * 2 + 1, freeLevel - 1);
        printf("}");
    }
    else assert(0);
}

void
buddy_dump(struct buddy * self) {
    dump(self, 1, self->level + 1);
    printf("\n");
}
