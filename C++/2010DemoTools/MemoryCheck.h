// #pragma once

/*
    注意, 本文件不能有pragma once!!!
    考虑:

    #include "memoryCheck.h"     
    #undef new
    #include "memoryCheck.h"

    第一句include是在一堆头文件中嵌套包含的, 这样就算最后用户按要求在头文件表的
    最尾端包含了memoryCheck, new的define也不回起作用, 因为pragma once禁止了第二次
    头文件展开! 所以注释掉pragma once
*/

/*
    内存泄漏检测

    使用: 在每个cpp的包含的各种头文件的最后, 包含本头文件

    说明: 只要工程中有"memoryCheck.cpp", 就能为所有的文件检测内存泄漏, 但只有
    包含了"memoryCheck.h"的cpp, 才能精确检测到泄漏行; 头文件不能包含本文件,
    因为会和重新声明new和delete的内存管理类冲突; 所以头文件中的内存分配不能
    精确到行的检测泄漏
*/

#if defined(_DEBUG) || defined(DEBUG)

namespace Scan
{
    void* allocMemoryCheck(size_t memSize, const char *fileName, unsigned int line);
    void freeMemoryCheck(void* memChunk, const char *fileName, unsigned int line);
}

#define _CRTDBG_MAP_ALLOC 

void* operator new (size_t memSize, const char *fileName, unsigned int line);
void* operator new[] (size_t memSize, const char *fileName, unsigned int line);
void operator delete (void* memChunk, const char *fileName, unsigned int line);
void operator delete[] (void* memChunk, const char *fileName, unsigned int line);

#undef new
#define new new(__FILE__, __LINE__)

#endif