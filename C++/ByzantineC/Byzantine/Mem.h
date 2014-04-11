#ifndef MEM_H
#define MEM_H

#include <memory.h>

void*   byMem_alloc(int nbytes, const char *file, int line);
void*   byMem_resize(void *p, int nbytes, const char *file, int line);
void    byMem_free(void *p, const char *file, int line);

#define by_ALLOC(p, n)          ((p) = byMem_alloc(sizeof(*(p)) * (n), __FILE__, __LINE__))
#define by_NEW(p)               by_ALLOC(p, 1)
#define by_RESIZE(p, n)         ((p) = byMem_resize(p, sizeof(*(p)) * (n), __FILE__, __LINE__))
#define by_FREE(p)              (byMem_free(p, __FILE__, __LINE__), (p) = NULL)

#endif
