// stdafx.h : ��׼ϵͳ�����ļ��İ����ļ���
// ���Ǿ���ʹ�õ��������ĵ�
// �ض�����Ŀ�İ����ļ�
//

#pragma once


#define WIN32_LEAN_AND_MEAN		// �� Windows ͷ���ų�����ʹ�õ�����
#include <stdio.h>
#include <tchar.h>

#include <windows.h>

// TODO: �ڴ˴����ó�����Ҫ������ͷ�ļ�
#pragma warning(disable : 4311 4312)
#include <QtGui/QtGui>
#include <QtCore/QtCore>
#pragma warning(default : 4311 4312)

#ifdef __cplusplus
#include <iostream>
using std::cin;
using std::cout;
using std::endl;
#endif

#define _STRING_MACRO(a) #a
#define STRING_MACRO(a) _STRING_MACRO(a)

#define FILE_LINE  __FILE__ "(" STRING_MACRO(__LINE__) ")"

#define verify(exp)\
    if (exp){}\
    else\
{\
    const char *msg = FILE_LINE " : \n" STRING_MACRO(exp) "\n";\
    ::OutputDebugString(msg);\
    ::MessageBox(NULL, msg, "����ʧ��", MB_OK);\
    ::DebugBreak();\
}

#define ASSERT_INSIDE_N(a, n)   verify(a >= 0 && a < n)