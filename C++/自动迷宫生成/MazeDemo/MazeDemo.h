// MazeDemo.h : PROJECT_NAME Ӧ�ó������ͷ�ļ�
//

#pragma once

#ifndef __AFXWIN_H__
	#error "�ڰ������ļ�֮ǰ������stdafx.h�������� PCH �ļ�"
#endif

#include "resource.h"		// ������


// CMazeDemoApp:
// �йش����ʵ�֣������ MazeDemo.cpp
//

class CMazeDemoApp : public CWinApp
{
public:
	CMazeDemoApp();

// ��д
	public:
	virtual BOOL InitInstance();

// ʵ��

	DECLARE_MESSAGE_MAP()
};

extern CMazeDemoApp theApp;