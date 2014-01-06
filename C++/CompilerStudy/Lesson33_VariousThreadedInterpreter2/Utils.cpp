
#include "pch.h"
#include "Utils.h"

#ifdef __GNUC__

#include <sys/mman.h>

void* allocExceMem(int memSize) {
    char *p = (char*)mmap(NULL, memSize, PROT_EXEC | PROT_WRITE | PROT_READ, MAP_ANON | MAP_PRIVATE, -1, 0);
    if (p == MAP_FAILED) puts(strerror(errno)), exit(1);
    return p;
}
void freeExecMem(void *p, int memSize) {
    munmap(p, memSize);
}

#endif
//------------------------------
#ifdef _MSC_VER

#pragma warning(disable : 4312)
#pragma warning(disable : 4311)
#include <Windows.h>
#pragma warning(default : 4312)
#pragma warning(default : 4311)

void* allocExceMem(int memSize) {
    return VirtualAlloc(NULL, memSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
}
void freeExecMem(void *p, int memSize) {
	VirtualFree(p, 0, MEM_RELEASE);
}

#endif
