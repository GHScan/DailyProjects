// stdafx.cpp : ֻ������׼�����ļ���Դ�ļ�
// Test.pch ����ΪԤ����ͷ
// stdafx.obj ������Ԥ����������Ϣ

#include "stdafx.h"

// TODO: �� STDAFX.H ��
// �����κ�����ĸ���ͷ�ļ����������ڴ��ļ�������

#ifdef _DEBUG
#define IRR_DLL_POST    // "_d"
#else
#define IRR_DLL_POST
#endif

#pragma comment(lib, "irrlicht" IRR_DLL_POST)
#pragma comment(lib, "irrKlang")