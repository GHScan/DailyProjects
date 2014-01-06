#ifndef UTILS_H
#define UTILS_H
//==============================

#ifdef __GNUC__

//#define FORCE_INLINE  __attribute__((always_inline))
#define FORCE_INLINE  inline

#endif

//------------------------------

#ifdef _MSC_VER

#define FORCE_INLINE  __forceinline

#endif

#define COUNT_OF_A(a) int(sizeof(a) / sizeof(a[0]))

extern void* allocExceMem(int memSize);
extern void freeExecMem(void *p, int memSize);

//==============================
#endif // UTILS_H
