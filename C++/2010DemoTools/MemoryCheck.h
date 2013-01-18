// #pragma once

/*
    ע��, ���ļ�������pragma once!!!
    ����:

    #include "memoryCheck.h"     
    #undef new
    #include "memoryCheck.h"

    ��һ��include����һ��ͷ�ļ���Ƕ�װ�����, ������������û���Ҫ����ͷ�ļ����
    ��β�˰�����memoryCheck, new��defineҲ����������, ��Ϊpragma once��ֹ�˵ڶ���
    ͷ�ļ�չ��! ����ע�͵�pragma once
*/

/*
    �ڴ�й©���

    ʹ��: ��ÿ��cpp�İ����ĸ���ͷ�ļ������, ������ͷ�ļ�

    ˵��: ֻҪ��������"memoryCheck.cpp", ����Ϊ���е��ļ�����ڴ�й©, ��ֻ��
    ������"memoryCheck.h"��cpp, ���ܾ�ȷ��⵽й©��; ͷ�ļ����ܰ������ļ�,
    ��Ϊ�����������new��delete���ڴ�������ͻ; ����ͷ�ļ��е��ڴ���䲻��
    ��ȷ���еļ��й©
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