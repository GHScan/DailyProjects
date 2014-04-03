#include "pch.h"

#include "utils.h"
#include "list.h"

static const char *TYPE = "List_T";

typedef struct List_T {
    const char *type;
    int capacity;
    int size;
    void** array;
} List_T;

static void
ensureCapacity(List_T *o, int capacity) {
    if (o->capacity < capacity) {
        int newCapacity = MAX(o->capacity * 3 / 2, capacity);
        o->array = realloc(o->array, sizeof(void*) * newCapacity);
        assert(o->array);
        o->capacity = newCapacity;
    }

    assert(o->capacity >= capacity);
}

List_T*         
List_create(int capacity) {
    List_T *o = malloc(sizeof(List_T));
    o->type = TYPE;
    o->capacity = 0;
    o->size = 0;
    o->array = 0;

    ensureCapacity(o, capacity);
    return o;
}

void            
List_destroy(List_T **p) {
    assert(p && *p && (*p)->type == TYPE);

    List_T *o = *p;
    free(o->array);
    free(o);
    *p = 0;
}

void            
List_push(List_T *o, void *p) {
    assert(o && o->type == TYPE);

    ensureCapacity(o, o->size + 1);
    o->array[o->size++] = p;
}

void*           
List_pop(List_T *o) {
    assert(o && o->type == TYPE);
    assert(o->size > 0);

    return o->array[--o->size];
}

void            
List_insert(List_T *o, int idx, void *p) {
    assert(o && o->type == TYPE);
    assert(idx >= 0 && idx <= o->size);

    ensureCapacity(o, o->size + 1);
    memmove(&o->array[idx + 1], &o->array[idx], (o->size - idx) * sizeof(void*));
    o->array[idx] = p;
    ++o->size;
}

void            
List_remove(List_T *o, int idx) {
    assert(o && o->type == TYPE);
    assert(idx >= 0 && idx < o->size);

    memmove(&o->array[idx], &o->array[idx + 1], (o->size - idx - 1) * sizeof(void*));
    --o->size;
}

void*           
List_front(List_T *o) {
    assert(o && o->type == TYPE);
    assert(o->size > 0);

    return o->array[0];
}

void*           
List_back(List_T *o) {
    assert(o && o->type == TYPE);
    assert(o->size > 0);

    return o->array[o->size - 1];
}

void*           
List_element(List_T *o, int idx) {
    assert(o && o->type == TYPE);
    assert(idx >= 0 && idx < o->size);

    return o->array[idx];
}

void            
List_setElement(List_T *o, int idx, void *p) {
    assert(o && o->type == TYPE);
    assert(idx >= 0 && idx < o->size);

    o->array[idx] = p;
}

int             
List_size(List_T *o) {
    assert(o && o->type == TYPE);

    return o->size;
}

void            
List_resize(List_T *o, int size, void *initv) {
    assert(o && o->type == TYPE);

    if (o->size < size) {
        ensureCapacity(o, size);
        if (initv == 0) {
            memset(&o->array[o->size], 0, (size - o->size) * sizeof(o->array[0]));
        } else {
            for (int i = o->size; i < size; ++i) {
                o->array[i] = initv;
            }
        }
    }
    o->size = size;
}

int             
List_capacity(List_T *o) {
    assert(o && o->type == TYPE);

    return o->capacity;
}

ListIter_T*     
List_begin(List_T *o) {
    assert(o && o->type == TYPE);

    return (ListIter_T*)&o->array[0];
}

ListIter_T*     
List_end(List_T *o) {
    assert(o && o->type == TYPE);

    return (ListIter_T*)&o->array[o->size];
}

ListIter_T*     
List_next(List_T *o, ListIter_T *it) {
    assert(o && o->type == TYPE);

    return ++it;
}
