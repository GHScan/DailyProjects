// stdafx.cpp : ֻ������׼�����ļ���Դ�ļ�
// Console.pch ����ΪԤ����ͷ
// stdafx.obj ������Ԥ����������Ϣ

#include "stdafx.h"

// TODO: �� STDAFX.H ��
// �����κ�����ĸ���ͷ�ļ����������ڴ��ļ�������

#ifdef _DEBUG
#define LIB_TYPE    "d"
#else
#define LIB_TYPE
#endif

#pragma comment(lib, "qtgui" LIB_TYPE "4")
#pragma comment(lib, "qtcore" LIB_TYPE "4")